type parsed_msg = {
  msg : Maildir.message_info;
  email : Email.message option;
}

let parse_msg msg =
  let raw = Maildir.read_message_raw msg in
  let email = match Email.parse_email raw with
    | Ok e -> Some e
    | Error _ -> None
  in
  { msg; email }

let get_date_epoch pm =
  match pm.email with
  | Some e ->
    (match e.date with
     | Some d -> Util.parse_iso8601_epoch d
     | None -> None)
  | None -> None

let get_from_str pm =
  match pm.email with
  | Some e ->
    (match e.from with
     | addr :: _ ->
       (match addr.name with Some n -> n | None -> addr.email)
     | [] -> "")
  | None -> ""

type filter_params = {
  from : string option;
  to_ : string option;
  subject : string option;
  body : string option;
  flags : string option;
  unread : string option;
  has_attachment : string option;
  after : string option;
  before : string option;
}

let empty_filters = {
  from = None; to_ = None; subject = None; body = None;
  flags = None; unread = None; has_attachment = None;
  after = None; before = None;
}

let apply_filters params parsed_msgs =
  let filter_opt value pred msgs =
    match value with
    | None -> msgs
    | Some v -> List.filter (pred v) msgs
  in
  parsed_msgs
  |> filter_opt params.from (fun v pm ->
    match pm.email with
    | Some e -> List.exists (fun a ->
        Util.contains_ci a.Email.email v ||
        (match a.Email.name with Some n -> Util.contains_ci n v | None -> false)
      ) e.from
    | None -> false)
  |> filter_opt params.to_ (fun v pm ->
    match pm.email with
    | Some e -> List.exists (fun a ->
        Util.contains_ci a.Email.email v ||
        (match a.Email.name with Some n -> Util.contains_ci n v | None -> false)
      ) e.to_
    | None -> false)
  |> filter_opt params.subject (fun v pm ->
    match pm.email with
    | Some e -> Util.contains_ci e.subject v
    | None -> false)
  |> filter_opt params.body (fun v pm ->
    match pm.email with
    | Some e ->
      (match e.body_text with Some t -> Util.contains_ci t v | None -> false) ||
      (match e.body_html with Some h -> Util.contains_ci h v | None -> false)
    | None -> false)
  |> filter_opt params.flags (fun v pm ->
    let required = String.split_on_char ',' v
      |> List.filter_map Maildir.flag_of_string in
    List.for_all (fun f -> List.mem f pm.msg.flags) required)
  |> (fun msgs ->
    match params.unread with
    | Some "true" ->
      List.filter (fun pm -> not (List.mem Maildir.Seen pm.msg.flags)) msgs
    | Some "false" ->
      List.filter (fun pm -> List.mem Maildir.Seen pm.msg.flags) msgs
    | _ -> msgs)
  |> (fun msgs ->
    match params.has_attachment with
    | Some "true" ->
      List.filter (fun pm ->
        match pm.email with
        | Some e -> e.attachments <> []
        | None -> false) msgs
    | Some "false" ->
      List.filter (fun pm ->
        match pm.email with
        | Some e -> e.attachments = []
        | None -> true) msgs
    | _ -> msgs)
  |> (fun msgs ->
    match params.after with
    | None -> msgs
    | Some v ->
      match Util.parse_date_iso v with
      | None -> msgs
      | Some threshold ->
        List.filter (fun pm ->
          match get_date_epoch pm with
          | Some d -> d >= threshold
          | None -> false) msgs)
  |> (fun msgs ->
    match params.before with
    | None -> msgs
    | Some v ->
      match Util.parse_date_iso v with
      | None -> msgs
      | Some threshold ->
        List.filter (fun pm ->
          match get_date_epoch pm with
          | Some d -> d < threshold
          | None -> false) msgs)

type sort_field = string * [ `Asc | `Desc ]

let parse_sort_param s =
  String.split_on_char ',' s
  |> List.filter_map (fun part ->
    match String.split_on_char ':' (String.trim part) with
    | [field; "asc"] -> Some (field, `Asc)
    | [field; "desc"] -> Some (field, `Desc)
    | [field] -> Some (field, `Asc)
    | _ -> None)

let default_sort = [("date", `Desc)]

let compare_parsed_msgs sort_fields a b =
  let rec cmp = function
    | [] -> 0
    | (field, dir) :: rest ->
      let c = match field with
        | "date" ->
          let da = get_date_epoch a |> Option.value ~default:0.0 in
          let db = get_date_epoch b |> Option.value ~default:0.0 in
          Float.compare da db
        | "from" ->
          String.compare (get_from_str a) (get_from_str b)
        | "subject" ->
          let sa = match a.email with Some e -> e.subject | None -> "" in
          let sb = match b.email with Some e -> e.subject | None -> "" in
          String.compare sa sb
        | "size" ->
          Int.compare a.msg.size b.msg.size
        | _ -> 0
      in
      let c = match dir with `Asc -> c | `Desc -> -c in
      if c <> 0 then c else cmp rest
  in
  cmp sort_fields

let sort_msgs sort_fields msgs =
  List.sort (compare_parsed_msgs sort_fields) msgs

type cursor_data = {
  sort_value : string;
  msg_id : string;
} [@@deriving yojson]

let encode_cursor cd =
  Yojson.Safe.to_string (cursor_data_to_yojson cd)
  |> Base64.encode_exn

let decode_cursor s =
  try
    let json_str = Base64.decode_exn s in
    match Yojson.Safe.from_string json_str |> cursor_data_of_yojson with
    | Ok cd -> Some cd
    | Error _ -> None
  with _ -> None

let primary_sort_value sort_fields pm =
  match sort_fields with
  | [] | ("date", _) :: _ ->
    Printf.sprintf "%.6f" (get_date_epoch pm |> Option.value ~default:0.0)
  | ("from", _) :: _ -> get_from_str pm
  | ("subject", _) :: _ ->
    (match pm.email with Some e -> e.subject | None -> "")
  | ("size", _) :: _ -> string_of_int pm.msg.size
  | _ -> ""

type page_result = {
  page : parsed_msg list;
  total : int;
  limit : int;
  next_cursor : string option;
  prev_cursor : string option;
}

let paginate ~limit ~cursor sort_fields sorted_msgs =
  let total = List.length sorted_msgs in
  let after_cursor = match cursor with
    | None -> sorted_msgs
    | Some c ->
      match decode_cursor c with
      | None -> sorted_msgs
      | Some cd ->
        let rec drop_until = function
          | [] -> []
          | pm :: rest ->
            if pm.msg.Maildir.id = cd.msg_id then rest
            else drop_until rest
        in
        drop_until sorted_msgs
  in
  let page = List.filteri (fun i _ -> i < limit) after_cursor in
  let next_cursor = match List.rev page with
    | [] -> None
    | last :: _ ->
      if List.length page < limit then None
      else Some (encode_cursor {
        sort_value = primary_sort_value sort_fields last;
        msg_id = last.msg.Maildir.id;
      })
  in
  let prev_cursor = match cursor with
    | None -> None
    | Some c ->
      match decode_cursor c with
      | None -> None
      | Some _cd -> Some c
  in
  { page; total; limit; next_cursor; prev_cursor }

let msg_to_summary (pm : parsed_msg) =
  match pm.email with
  | Some email ->
    `Assoc [
      "id", `String pm.msg.id;
      "mailbox", `String pm.msg.mailbox;
      "subject", `String email.subject;
      "from", `List (List.map Email.address_to_yojson email.from);
      "to", `List (List.map Email.address_to_yojson email.to_);
      "date", (match email.date with Some d -> `String d | None -> `Null);
      "flags", `List (List.map (fun f -> `String (Maildir.string_of_flag f)) pm.msg.flags);
      "is_new", `Bool pm.msg.is_new;
      "size", `Int pm.msg.size;
      "has_attachments", `Bool (email.attachments <> []);
    ]
  | None ->
    `Assoc [
      "id", `String pm.msg.id;
      "mailbox", `String pm.msg.mailbox;
      "flags", `List (List.map (fun f -> `String (Maildir.string_of_flag f)) pm.msg.flags);
      "is_new", `Bool pm.msg.is_new;
      "size", `Int pm.msg.size;
    ]

let msg_to_detail (pm : parsed_msg) =
  match pm.email with
  | None -> None
  | Some email ->
    let base = Email.message_to_yojson email in
    let merged = match base with
      | `Assoc fields ->
        `Assoc (("id", `String pm.msg.id) :: fields @ [
          "flags", `List (List.map (fun f -> `String (Maildir.string_of_flag f)) pm.msg.flags);
          "mailbox", `String pm.msg.mailbox;
          "size", `Int pm.msg.size;
        ])
      | other -> other
    in
    Some merged

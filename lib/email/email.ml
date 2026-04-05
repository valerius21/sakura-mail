(** Email parsing and generation — wraps mrmime into an interface. *)

type address = {
  name : string option;
  email : string;
} [@@deriving yojson]

type attachment_info = {
  index : int;
  filename : string option;
  content_type : string;
  size : int;
} [@@deriving yojson]

type message = {
  message_id : string option;
  in_reply_to : string option;
  references : string list;
  from : address list;
  to_ : address list; [@key "to"]
  cc : address list;
  bcc : address list;
  subject : string;
  date : string option;
  body_text : string option;
  body_html : string option;
  attachments : attachment_info list;
} [@@deriving yojson]

type draft = {
  to_ : string list; [@key "to"]
  cc : string list;
  bcc : string list;
  subject : string;
  body : string;
  in_reply_to : string option;
} [@@deriving yojson]

let unstructured_to_string (elts : Mrmime.Unstructured.elt list) =
  let buf = Buffer.create 128 in
  List.iter (fun (elt : Mrmime.Unstructured.elt) ->
    match elt with
    | `Uchar u -> Buffer.add_utf_8_uchar buf u
    | `WSP s -> Buffer.add_string buf (s :> string)
    | `FWS _ -> Buffer.add_char buf ' '
    | `CR | `LF | `d0 | `OBS_NO_WS_CTL _ | `Invalid_char _ -> ()
    | _ -> ()
  ) elts;
  Buffer.contents buf

let unstructured_with_encoded_to_string (elts : Mrmime.Unstructured_with_encoded.elt list) =
  let buf = Buffer.create 128 in
  List.iter (fun (elt : Mrmime.Unstructured_with_encoded.elt) ->
    match elt with
    | `Uchar u -> Buffer.add_utf_8_uchar buf u
    | `WSP s -> Buffer.add_string buf (s :> string)
    | `FWS _ -> Buffer.add_char buf ' '
    | `CR | `LF | `d0 | `OBS_NO_WS_CTL _ | `Invalid_char _ -> ()
    | `Encoded (text, _raw) -> Buffer.add_string buf text
    | _ -> ()
  ) elts;
  Buffer.contents buf

let address_of_mailbox (mb : Mrmime.Mailbox.t) : address =
  let email = Mrmime.Mailbox.to_string mb in
  let name = match mb.Emile.name with
    | Some phrase -> Some (Mrmime.Mailbox.Phrase.to_string phrase)
    | None -> None
  in
  let email = match mb.Emile.name with
    | Some _ ->
      let local_str = Mrmime.Mailbox.Local.to_string mb.Emile.local in
      let domain_str = Mrmime.Mailbox.Domain.to_string (fst mb.Emile.domain) in
      local_str ^ "@" ^ domain_str
    | None -> email
  in
  { name; email }

let addresses_of_mailboxes (mbs : Mrmime.Mailbox.t list) : address list =
  List.map address_of_mailbox mbs

let addresses_of_address_list (addrs : Mrmime.Address.t list) : address list =
  List.concat_map (fun addr ->
    match addr with
    | `Mailbox mb -> [address_of_mailbox mb]
    | `Group g -> List.map address_of_mailbox g.Emile.mailboxes
  ) addrs

let subject_witness =
  Mrmime.Field_name.Map.singleton
    Mrmime.Field_name.subject
    (Mrmime.Field.Witness Mrmime.Field.Unstructured_with_encoded)

let decode_rfc2047_word encoded =
  try
    Scanf.sscanf encoded "=?%[^?]?%c?%[^?]?="
      (fun _charset encoding text ->
        match Char.uppercase_ascii encoding with
        | 'B' -> Base64.decode_exn text
        | 'Q' ->
          let buf = Buffer.create (String.length text) in
          let i = ref 0 in
          while !i < String.length text do
            if text.[!i] = '_' then
              (Buffer.add_char buf ' '; incr i)
            else if text.[!i] = '=' && !i + 2 < String.length text then begin
              let hex = String.sub text (!i + 1) 2 in
              (try Buffer.add_char buf (Char.chr (int_of_string ("0x" ^ hex)))
               with _ -> Buffer.add_string buf ("=" ^ hex));
              i := !i + 3
            end else
              (Buffer.add_char buf text.[!i]; incr i)
          done;
          Buffer.contents buf
        | _ -> encoded)
  with _ -> encoded

let is_encoded_word s =
  String.length s > 6 &&
  String.sub s 0 2 = "=?" &&
  String.sub s (String.length s - 2) 2 = "?="

let decode_rfc2047 s =
  if not (String.contains s '=') then s
  else
    let parts = String.split_on_char ' ' s in
    let was_encoded = List.map is_encoded_word parts in
    let decoded = List.map (fun part ->
      if is_encoded_word part then decode_rfc2047_word part
      else part
    ) parts in
    let buf = Buffer.create (String.length s) in
    let rec join ds es =
      match ds, es with
      | [], _ | _, [] -> ()
      | [x], _ -> Buffer.add_string buf x
      | a :: b_rest, ea :: eb_rest ->
        Buffer.add_string buf a;
        let eb = match eb_rest with x :: _ -> x | [] -> false in
        if not (ea && eb) then Buffer.add_char buf ' ';
        join b_rest eb_rest
    in
    join decoded was_encoded;
    Buffer.contents buf

let extract_subject header =
  let raw = match Mrmime.Header.assoc Mrmime.Field_name.subject header with
    | Mrmime.Field.Field (_, Mrmime.Field.Unstructured_with_encoded, v) :: _ ->
      unstructured_with_encoded_to_string v
    | Mrmime.Field.Field (_, Mrmime.Field.Unstructured, v) :: _ ->
      unstructured_to_string v
    | _ -> ""
  in
  String.trim (decode_rfc2047 (String.trim raw))

let extract_mailboxes header field_name =
  match Mrmime.Header.assoc field_name header with
  | Mrmime.Field.Field (_, Mrmime.Field.Mailboxes, mbs) :: _ ->
    addresses_of_mailboxes mbs
  | Mrmime.Field.Field (_, Mrmime.Field.Addresses, addrs) :: _ ->
    addresses_of_address_list addrs
  | _ -> []

let extract_message_id header =
  match Mrmime.Header.message_id header with
  | Some mid -> Some (Format.asprintf "%a" Mrmime.MessageID.pp mid)
  | None -> None

let date_to_iso8601 (d : Mrmime.Date.t) =
  match Mrmime.Date.to_ptime d with
  | Ok (ptime, tz_offset_s) ->
    let (y, m, day), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s ptime in
    let tz_h = tz_offset_s / 3600 in
    let tz_m = abs (tz_offset_s mod 3600) / 60 in
    if tz_offset_s = 0 then
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" y m day hh mm ss
    else
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d%+03d:%02d" y m day hh mm ss tz_h tz_m
  | Error _ ->
    Format.asprintf "%a" Mrmime.Date.pp d

let extract_date header =
  match Mrmime.Header.assoc (Mrmime.Field_name.v "date") header with
  | Mrmime.Field.Field (_, Mrmime.Field.Date, d) :: _ ->
    Some (date_to_iso8601 d)
  | _ -> None

let extract_unstructured_field header name =
  match Mrmime.Header.assoc (Mrmime.Field_name.v name) header with
  | Mrmime.Field.Field (_, Mrmime.Field.Unstructured, v) :: _ ->
    Some (unstructured_to_string v)
  | _ -> None

let is_html_content_type header =
  let ct = Mrmime.Header.content_type header in
  match Mrmime.Content_type.subty ct with
  | `Iana_token "html" | `Iana_token "HTML" -> true
  | _ -> false

let is_plain_content_type header =
  let ct = Mrmime.Header.content_type header in
  match Mrmime.Content_type.ty ct, Mrmime.Content_type.subty ct with
  | `Text, `Iana_token "plain" | `Text, `Iana_token "Plain" -> true
  | _ -> false

let extract_body_text header (body : string Mrmime.Mail.t) =
  let rec find_text = function
    | Mrmime.Mail.Leaf content -> Some content
    | Mrmime.Mail.Message (_hdr, sub) -> find_text sub
    | Mrmime.Mail.Multipart parts ->
      List.find_map (fun (hdr, part_opt) ->
        match part_opt with
        | None -> None
        | Some part ->
          if is_plain_content_type hdr then find_text part
          else None
      ) parts
  in
  if is_html_content_type header then None
  else find_text body

let extract_body_html header (body : string Mrmime.Mail.t) =
  let rec find_html = function
    | Mrmime.Mail.Leaf content -> Some content
    | Mrmime.Mail.Message (_hdr, sub) -> find_html sub
    | Mrmime.Mail.Multipart parts ->
      List.find_map (fun (hdr, part_opt) ->
        match part_opt with
        | None -> None
        | Some part ->
          if is_html_content_type hdr then find_html part
          else None
      ) parts
  in
  if is_html_content_type header then find_html body
  else
    match body with
    | Mrmime.Mail.Multipart _ -> find_html body
    | _ -> None

let extract_content_disposition_filename header =
  match Mrmime.Header.assoc (Mrmime.Field_name.v "content-disposition") header with
  | Mrmime.Field.Field (_, Mrmime.Field.Unstructured, v) :: _ ->
    let s = unstructured_to_string v in
    let s = String.trim s in
    (match String.split_on_char ';' s with
     | _ :: params ->
       List.find_map (fun param ->
         let param = String.trim param in
         if String.length param > 9 && String.sub param 0 9 = "filename=" then
           let v = String.sub param 9 (String.length param - 9) in
           let v = String.trim v in
           let v = if String.length v >= 2 && v.[0] = '"' && v.[String.length v - 1] = '"'
             then String.sub v 1 (String.length v - 2) else v in
           Some v
         else None
       ) params
     | _ -> None)
  | _ -> None

let extract_content_type_name header =
  let ct = Mrmime.Header.content_type header in
  let params = Mrmime.Content_type.parameters ct in
  match List.find_opt (fun (k, _) -> String.lowercase_ascii k = "name") params with
  | Some (_, v) ->
    let v = match v with `Token s | `String s -> s in
    Some v
  | None -> None

let is_attachment header =
  let ct = Mrmime.Header.content_type header in
  let ty = Mrmime.Content_type.ty ct in
  match ty with
  | `Text -> false
  | _ ->
    match extract_content_disposition_filename header with
    | Some _ -> true
    | None ->
      match extract_content_type_name header with
      | Some _ -> true
      | None -> ty <> `Multipart

let extract_attachments (body : string Mrmime.Mail.t) =
  let attachments = ref [] in
  let idx = ref 0 in
  let rec walk = function
    | Mrmime.Mail.Leaf _content -> ()
    | Mrmime.Mail.Message (_hdr, sub) -> walk sub
    | Mrmime.Mail.Multipart parts ->
      List.iter (fun (hdr, part_opt) ->
        match part_opt with
        | None -> ()
        | Some part ->
          if is_attachment hdr then begin
            let ct = Mrmime.Header.content_type hdr in
            let ty = Mrmime.Content_type.ty ct in
            let subty = Mrmime.Content_type.subty ct in
            let content_type = Printf.sprintf "%s/%s"
              (match ty with
               | `Text -> "text" | `Image -> "image" | `Audio -> "audio"
               | `Video -> "video" | `Application -> "application"
               | `Message -> "message" | `Multipart -> "multipart"
               | `Ietf_token s | `X_token s -> s)
              (match subty with
               | `Iana_token s | `Ietf_token s | `X_token s -> s) in
            let filename =
              match extract_content_disposition_filename hdr with
              | Some _ as f -> f
              | None -> extract_content_type_name hdr in
            let size = match part with
              | Mrmime.Mail.Leaf content -> String.length content
              | _ -> 0 in
            attachments := { index = !idx; filename; content_type; size } :: !attachments;
            incr idx
          end else
            walk part
      ) parts
  in
  walk body;
  List.rev !attachments

let get_attachment_content (body : string Mrmime.Mail.t) (target_index : int) =
  let idx = ref 0 in
  let result = ref None in
  let rec walk = function
    | Mrmime.Mail.Leaf _content -> ()
    | Mrmime.Mail.Message (_hdr, sub) -> walk sub
    | Mrmime.Mail.Multipart parts ->
      List.iter (fun (hdr, part_opt) ->
        match part_opt with
        | None -> ()
        | Some part ->
          if is_attachment hdr then begin
            if !idx = target_index then begin
              let ct = Mrmime.Header.content_type hdr in
              let ty = Mrmime.Content_type.ty ct in
              let subty = Mrmime.Content_type.subty ct in
              let content_type = Printf.sprintf "%s/%s"
                (match ty with
                 | `Text -> "text" | `Image -> "image" | `Audio -> "audio"
                 | `Video -> "video" | `Application -> "application"
                 | `Message -> "message" | `Multipart -> "multipart"
                 | `Ietf_token s | `X_token s -> s)
                (match subty with
                 | `Iana_token s | `Ietf_token s | `X_token s -> s) in
              let content = match part with
                | Mrmime.Mail.Leaf c -> c
                | _ -> "" in
              result := Some (content_type, content)
            end;
            incr idx
          end else
            walk part
      ) parts
  in
  walk body;
  !result

let normalize_line_endings s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    if s.[!i] = '\r' && !i + 1 < len && s.[!i + 1] = '\n' then begin
      Buffer.add_string buf "\r\n";
      i := !i + 2
    end else if s.[!i] = '\n' then begin
      Buffer.add_string buf "\r\n";
      i := !i + 1
    end else begin
      Buffer.add_char buf s.[!i];
      i := !i + 1
    end
  done;
  Buffer.contents buf

let parse_email_raw (raw : string) =
  let raw = normalize_line_endings raw in
  let parser = Mrmime.Mail.mail (Some subject_witness) in
  match Angstrom.parse_string ~consume:Angstrom.Consume.Prefix parser raw with
  | Error msg -> Error ("Failed to parse email: " ^ msg)
  | Ok (header, body) -> Ok (header, body)

let parse_email (raw : string) : (message, string) result =
  match parse_email_raw raw with
  | Error _ as e -> e
  | Ok (header, body) ->
    let from = extract_mailboxes header (Mrmime.Field_name.v "from") in
    let to_ = extract_mailboxes header (Mrmime.Field_name.v "to") in
    let cc = extract_mailboxes header (Mrmime.Field_name.v "cc") in
    let bcc = extract_mailboxes header (Mrmime.Field_name.v "bcc") in
    let subject = extract_subject header in
    let message_id = extract_message_id header in
    let date = extract_date header in
    let in_reply_to = extract_unstructured_field header "in-reply-to" in
    let references_str = extract_unstructured_field header "references" in
    let references = match references_str with
      | None -> []
      | Some s ->
        String.split_on_char ' ' s
        |> List.filter (fun s -> String.length s > 0)
    in
    let body_text = extract_body_text header body in
    let body_html = extract_body_html header body in
    let attachments = extract_attachments body in
    Ok { message_id; in_reply_to; references; from; to_; cc; bcc;
         subject; date; body_text; body_html; attachments }

let get_attachment (raw : string) (index : int) =
  match parse_email_raw raw with
  | Error e -> Error e
  | Ok (_header, body) ->
    match get_attachment_content body index with
    | None -> Error "Attachment not found"
    | Some (ct, content) ->
      let attachments = extract_attachments body in
      let filename = match List.find_opt (fun a -> a.index = index) attachments with
        | Some a -> a.filename
        | None -> None
      in
      Ok (ct, filename, content)

let generate_draft (d : draft) : string =
  let buf = Buffer.create 4096 in
  let add_header name value =
    Buffer.add_string buf name;
    Buffer.add_string buf ": ";
    Buffer.add_string buf value;
    Buffer.add_string buf "\r\n"
  in
  let hostname = try Unix.gethostname () with _ -> "localhost" in
  let msg_id = Printf.sprintf "<%s.%d@%s>"
    (Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ()))
    (int_of_float (Unix.gettimeofday ()))
    hostname
  in
  add_header "Message-ID" msg_id;
  add_header "Date" (let t = Unix.gmtime (Unix.gettimeofday ()) in
    Printf.sprintf "%02d %s %04d %02d:%02d:%02d +0000"
      t.Unix.tm_mday
      (match t.Unix.tm_mon with
       | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr"
       | 4 -> "May" | 5 -> "Jun" | 6 -> "Jul" | 7 -> "Aug"
       | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | _ -> "Dec")
      (1900 + t.Unix.tm_year)
      t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec);
  add_header "Subject" d.subject;
  if d.to_ <> [] then
    add_header "To" (String.concat ", " d.to_);
  if d.cc <> [] then
    add_header "Cc" (String.concat ", " d.cc);
  if d.bcc <> [] then
    add_header "Bcc" (String.concat ", " d.bcc);
  (match d.in_reply_to with
   | Some ref_id ->
     add_header "In-Reply-To" ref_id;
     add_header "References" ref_id
   | None -> ());
  add_header "MIME-Version" "1.0";
  add_header "Content-Type" "text/plain; charset=UTF-8";
  add_header "Content-Transfer-Encoding" "8bit";
  Buffer.add_string buf "\r\n";
  Buffer.add_string buf d.body;
  Buffer.contents buf

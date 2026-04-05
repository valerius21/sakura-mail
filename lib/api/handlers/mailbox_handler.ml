open Api_common

let handle_list registry base_dir request =
  with_account_maildir registry base_dir request (fun mdir ->
    let mailboxes = Maildir.list_mailboxes ~maildir_root:mdir in
    let json = `List (List.map (fun m -> `String m) mailboxes) in
    json_response (Yojson.Safe.to_string json))

let filters_of_request request : Query.filter_params =
  { from = Dream.query request "from";
    to_ = Dream.query request "to";
    subject = Dream.query request "subject";
    body = Dream.query request "body";
    flags = Dream.query request "flags";
    unread = Dream.query request "unread";
    has_attachment = Dream.query request "has_attachment";
    after = Dream.query request "after";
    before = Dream.query request "before" }

let sort_fields_of_request request =
  match Dream.query request "sort" with
  | None -> Query.default_sort
  | Some s -> Query.parse_sort_param s

let handle_create registry sync_scheduler base_dir request =
  with_auth registry request (fun account ->
    let mdir = Account.maildir_path ~base_dir account.id in
    match%lwt body_json request with
    | Error err -> json_error `Bad_Request err
    | Ok json ->
      let name = match json with
        | `Assoc a -> (match List.assoc_opt "name" a with
          | Some (`String s) -> Some s | _ -> None)
        | _ -> None
      in
      match name with
      | None -> json_error `Bad_Request "Missing 'name' field"
      | Some name ->
        match Maildir.create_mailbox ~maildir_root:mdir ~name with
        | Error err -> json_error `Conflict err
        | Ok _ ->
          let%lwt _result = Sync.try_sync sync_scheduler ~base_dir account in
          json_response ~status:`Created
            (Yojson.Safe.to_string (`Assoc ["name", `String name])))

let handle_delete_mailbox registry sync_scheduler base_dir request name =
  with_auth registry request (fun account ->
    let mdir = Account.maildir_path ~base_dir account.id in
    let name = Dream.from_percent_encoded name in
    let path = Filename.concat mdir name in
    if not (Sys.file_exists path) then
      json_error `Not_Found (Printf.sprintf "Mailbox '%s' not found" name)
    else
    match Maildir.delete_mailbox ~maildir_root:mdir ~name with
    | Error err -> json_error `Conflict err
    | Ok () ->
      let%lwt _result = Sync.try_sync sync_scheduler ~base_dir account in
      json_response {|{"ok": true}|})

let handle_rename registry _sync_scheduler base_dir request name =
  with_auth registry request (fun account ->
    let mdir = Account.maildir_path ~base_dir account.id in
    let name = Dream.from_percent_encoded name in
    match%lwt body_json request with
    | Error err -> json_error `Bad_Request err
    | Ok json ->
      let new_name = match json with
        | `Assoc a -> (match List.assoc_opt "name" a with
          | Some (`String s) -> Some s | _ -> None)
        | _ -> None
      in
      match new_name with
      | None -> json_error `Bad_Request "Missing 'name' field"
      | Some new_name ->
        let path = Filename.concat mdir name in
        if not (Sys.file_exists path) then
          json_error `Not_Found (Printf.sprintf "Mailbox '%s' not found" name)
        else
        let new_path = Filename.concat mdir new_name in
        if Sys.file_exists new_path then
          json_error `Conflict (Printf.sprintf "Mailbox '%s' already exists" new_name)
        else
        match Maildir.rename_mailbox ~maildir_root:mdir ~name ~new_name with
        | Error err -> json_error `Bad_Request err
        | Ok _ ->
          json_response
            (Yojson.Safe.to_string (`Assoc ["name", `String new_name])))

let handle_messages settings registry base_dir request mailbox =
  with_account_maildir registry base_dir request (fun mdir ->
    let mailbox = Dream.from_percent_encoded mailbox in
    let mailbox_path = Filename.concat mdir mailbox in
    if not (Sys.file_exists mailbox_path) then
      json_error `Not_Found (Printf.sprintf "Mailbox '%s' not found" mailbox)
    else
    let msgs = Maildir.scan_mailbox ~maildir_root:mdir ~mailbox in
    let filters = filters_of_request request in
    let sort_fields = sort_fields_of_request request in
    let limit =
      let raw = Dream.query request "limit"
        |> Option.map int_of_string_opt |> Option.join
        |> Option.value ~default:settings.Settings.default_page_limit in
      if raw <= 0 then settings.Settings.default_page_limit else raw in
    let cursor = Dream.query request "cursor" in
    let parsed = List.map Query.parse_msg msgs in
    let filtered = Query.apply_filters filters parsed in
    let sorted = Query.sort_msgs sort_fields filtered in
    let r = Query.paginate ~limit ~cursor sort_fields sorted in
    let json_msgs = List.map Query.msg_to_summary r.page in
    let result = `Assoc [
      "messages", `List json_msgs;
      "total", `Int r.total;
      "limit", `Int r.limit;
      "next_cursor", (match r.next_cursor with Some c -> `String c | None -> `Null);
      "prev_cursor", (match r.prev_cursor with Some c -> `String c | None -> `Null);
    ] in
    json_response (Yojson.Safe.to_string result))

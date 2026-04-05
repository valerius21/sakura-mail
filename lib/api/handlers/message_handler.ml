open Api_common

let handle_read registry base_dir request id =
  with_account_maildir registry base_dir request (fun mdir ->
    match Maildir.find_message ~maildir_root:mdir ~id with
    | None -> json_error `Not_Found "Message not found"
    | Some msg ->
      let pm = Query.parse_msg msg in
      match Query.msg_to_detail pm with
      | None -> json_error `Internal_Server_Error "Failed to parse email"
      | Some json -> json_response (Yojson.Safe.to_string json))

let handle_raw registry base_dir request id =
  with_account_maildir registry base_dir request (fun mdir ->
    match Maildir.find_message ~maildir_root:mdir ~id with
    | None -> json_error `Not_Found "Message not found"
    | Some msg ->
      let raw = Maildir.read_message_raw msg in
      Dream.respond ~headers:[("Content-Type", "message/rfc822")] raw)

let handle_attachment registry base_dir request id index_str =
  with_account_maildir registry base_dir request (fun mdir ->
    match Maildir.find_message ~maildir_root:mdir ~id with
    | None -> json_error `Not_Found "Message not found"
    | Some msg ->
      match int_of_string_opt index_str with
      | None -> json_error `Bad_Request "Invalid attachment index"
      | Some index ->
        let raw = Maildir.read_message_raw msg in
        match Email.get_attachment raw index with
        | Error err -> json_error `Not_Found err
        | Ok (content_type, filename, content) ->
          let disposition = match filename with
            | Some name ->
              Printf.sprintf "attachment; filename=\"%s\"" name
            | None -> "attachment"
          in
          Dream.respond
            ~headers:[
              ("Content-Type", content_type);
              ("Content-Disposition", disposition);
            ]
            content)

type flags_body = {
  flags : string list;
} [@@deriving yojson]

let handle_set_flags registry base_dir request id =
  with_account_maildir registry base_dir request (fun mdir ->
    match Maildir.find_message ~maildir_root:mdir ~id with
    | None -> json_error `Not_Found "Message not found"
    | Some msg ->
      match%lwt body_json request with
      | Error err -> json_error `Bad_Request err
      | Ok json ->
        match flags_body_of_yojson json with
        | Error err -> json_error `Bad_Request ("Invalid body: " ^ err)
        | Ok body ->
          let invalid = List.filter (fun s -> Maildir.flag_of_string s = None) body.flags in
          if invalid <> [] then
            json_error `Bad_Request
              (Printf.sprintf "Unknown flags: %s" (String.concat ", " invalid))
          else
            let flags = List.filter_map Maildir.flag_of_string body.flags in
            let _updated = Maildir.set_flags msg flags in
            json_response {|{"ok": true}|})

type move_body = {
  destination : string;
} [@@deriving yojson]

let handle_move registry base_dir request id =
  with_account_maildir registry base_dir request (fun mdir ->
    match Maildir.find_message ~maildir_root:mdir ~id with
    | None -> json_error `Not_Found "Message not found"
    | Some msg ->
      match%lwt body_json request with
      | Error err -> json_error `Bad_Request err
      | Ok json ->
        match move_body_of_yojson json with
        | Error err -> json_error `Bad_Request ("Invalid body: " ^ err)
        | Ok body ->
          let _moved = Maildir.move_message ~maildir_root:mdir msg
            ~dest_mailbox:body.destination in
          json_response {|{"ok": true}|})

let handle_delete registry base_dir request id =
  with_account_maildir registry base_dir request (fun mdir ->
    match Maildir.find_message ~maildir_root:mdir ~id with
    | None -> json_error `Not_Found "Message not found"
    | Some msg ->
      let _trashed = Maildir.trash_message ~maildir_root:mdir msg in
      json_response {|{"ok": true}|})

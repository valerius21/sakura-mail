open Api_common

let handle_create registry base_dir request =
  with_account_maildir registry base_dir request (fun mdir ->
    match%lwt body_json request with
    | Error err -> json_error `Bad_Request err
    | Ok json ->
      match Email.draft_of_yojson json with
      | Error err -> json_error `Bad_Request ("Invalid draft: " ^ err)
      | Ok draft ->
        let content = Email.generate_draft draft in
        let msg = Maildir.write_message ~maildir_root:mdir
          ~mailbox:"Drafts" ~content in
        json_response ~status:`Created
          (Yojson.Safe.to_string (`Assoc [
            "id", `String msg.id;
            "mailbox", `String msg.mailbox;
          ])))

let handle_update registry base_dir request id =
  with_account_maildir registry base_dir request (fun mdir ->
    match Maildir.find_message ~maildir_root:mdir ~id with
    | None -> Api_common.json_error `Not_Found "Draft not found"
    | Some old_msg ->
      match%lwt body_json request with
      | Error err -> json_error `Bad_Request err
      | Ok json ->
        match Email.draft_of_yojson json with
        | Error err -> json_error `Bad_Request ("Invalid draft: " ^ err)
        | Ok draft ->
          Sys.remove old_msg.path;
          let content = Email.generate_draft draft in
          let msg = Maildir.write_message ~maildir_root:mdir
            ~mailbox:"Drafts" ~content in
          json_response
            (Yojson.Safe.to_string (`Assoc [
              "id", `String msg.id;
              "mailbox", `String msg.mailbox;
            ])))

open Api_common

let handle_verify request =
  match%lwt body_json request with
  | Error msg -> json_error `Bad_Request msg
  | Ok json ->
    match Account_handler.register_body_of_yojson json with
    | Error msg -> json_error `Bad_Request (Account_handler.friendly_error msg)
    | Ok body ->
      let tmp_dir = Filename.get_temp_dir_name () in
      let tmp_id = Printf.sprintf "verify_%d_%d" (Unix.getpid ()) (Random.bits ()) in
      let tmp_base = Filename.concat tmp_dir tmp_id in
      Unix.mkdir tmp_base 0o700;
      let tmp_maildir = Filename.concat tmp_base "Maildir" in
      Unix.mkdir tmp_maildir 0o700;
      Maildir.ensure_maildir_structure (Filename.concat tmp_maildir "INBOX");
      let config_path = Filename.concat tmp_base ".mbsyncrc" in
      let mbsync_conf : Mbsync_config.t = {
        account = {
          host = body.host;
          port = body.port;
          user = body.username;
          password = body.password;
          use_tls = body.use_tls;
        };
        maildir_path = tmp_maildir;
        channel_name = "verify";
      } in
      Mbsync_config.write_config ~path:config_path mbsync_conf;
      let config = { Mbsync.default_config with config_file = Some config_path } in
      let%lwt result = Mbsync.list_mailboxes ~config Mbsync.All in
      let cleanup () =
        let rec rm path =
          if Sys.is_directory path then begin
            Array.iter (fun n -> rm (Filename.concat path n)) (Sys.readdir path);
            Unix.rmdir path
          end else Sys.remove path
        in
        (try rm tmp_base with _ -> ())
      in
      cleanup ();
      match result with
      | Ok mailboxes ->
        json_response (Yojson.Safe.to_string (`Assoc [
          "ok", `Bool true;
          "mailboxes", `List (List.map (fun m -> `String m) mailboxes);
        ]))
      | Error err ->
        json_error `Bad_Request
          (Printf.sprintf "Connection failed (exit %d): %s"
            err.Mbsync.exit_code (String.trim err.Mbsync.stderr))

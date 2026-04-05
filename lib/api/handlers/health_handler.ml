open Api_common

let start_time = Unix.gettimeofday ()

let handle_health _request =
  json_response {|{"status": "ok"}|}

let handle_stats registry sync_scheduler base_dir settings request =
  with_auth registry request (fun _account ->
    let account_count = List.length registry.Account.accounts in
    let uptime = Unix.gettimeofday () -. start_time in
    let last_sync =
      let latest = ref None in
      List.iter (fun (a : Account.t) ->
        match Sync.last_sync_info sync_scheduler a.id with
        | Some ts ->
          (match !latest with
           | None -> latest := Some (a.id, ts)
           | Some (_, prev_ts) when ts > prev_ts -> latest := Some (a.id, ts)
           | _ -> ())
        | None -> ()
      ) registry.Account.accounts;
      match !latest with
      | None -> `Null
      | Some (id, ts) ->
        let t = Unix.gmtime ts in
        `Assoc [
          "account_id", `String id;
          "at", `String (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
            (1900 + t.Unix.tm_year) (t.Unix.tm_mon + 1) t.Unix.tm_mday
            t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec);
        ]
    in
    let (total_messages, total_size) =
      List.fold_left (fun (msgs, sz) (a : Account.t) ->
        let mdir = Account.maildir_path ~base_dir a.id in
        if Sys.file_exists mdir then
          let (m, s) = Maildir.mailbox_stats ~maildir_root:mdir in
          (msgs + m, sz + s)
        else (msgs, sz)
      ) (0, 0) registry.Account.accounts
    in
    json_response (Yojson.Safe.to_string (`Assoc [
      "status", `String "ok";
      "accounts", `Int account_count;
      "uptime_seconds", `Int (int_of_float uptime);
      "last_sync", last_sync;
      "maildir", `Assoc [
        "total_messages", `Int total_messages;
        "total_size_bytes", `Int total_size;
      ];
      "settings", Settings.to_json settings;
    ])))

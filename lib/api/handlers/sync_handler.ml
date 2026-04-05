open Api_common

let handle_sync sync_scheduler registry base_dir request =
  with_auth registry request (fun account ->
    match%lwt Sync.try_sync sync_scheduler ~base_dir account with
    | Error `Already_syncing ->
      json_error `Conflict "Sync already in progress"
    | Error (`Sync_failed result) ->
      json_error `Internal_Server_Error
        (Printf.sprintf "Sync failed (exit %d): %s"
          result.Mbsync.exit_code result.Mbsync.stderr)
    | Ok _result ->
      json_response {|{"ok": true, "message": "Sync completed"}|})

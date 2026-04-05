open Api_common

let handle_logs registry base_dir request id =
  with_auth registry request (fun account ->
    if account.Account.id <> id then
      json_error `Not_Found "Account not found"
    else
      let entries = Sync_log.read_log ~base_dir account.id in
      let json = `List (List.map Sync_log.entry_to_json entries) in
      json_response (Yojson.Safe.to_string json))

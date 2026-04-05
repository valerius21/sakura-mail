open Api_common

type register_body = {
  host : string;
  port : int;
  username : string;
  password : string;
  use_tls : bool;
} [@@deriving yojson]

let friendly_error msg =
  let prefix = "Account_handler.register_body." in
  let plen = String.length prefix in
  let buf = Buffer.create (String.length msg) in
  let i = ref 0 in
  while !i < String.length msg do
    if !i + plen <= String.length msg && String.sub msg !i plen = prefix then
      i := !i + plen
    else begin
      Buffer.add_char buf msg.[!i];
      i := !i + 1
    end
  done;
  "Invalid request body: " ^ Buffer.contents buf

let handle_register registry request =
  match%lwt body_json request with
  | Error msg -> json_error `Bad_Request msg
  | Ok json ->
    match register_body_of_yojson json with
    | Error msg -> json_error `Bad_Request (friendly_error msg)
    | Ok body ->
      let imap : Account.imap_config = {
        host = body.host;
        port = body.port;
        username = body.username;
        password = body.password;
        use_tls = body.use_tls;
      } in
      try
        let account = Account.register registry ~imap in
        json_response ~status:`Created
          (Yojson.Safe.to_string (`Assoc [
            "id", `String account.id;
            "api_key", `String account.api_key;
          ]))
      with Account.Duplicate_account username ->
        json_error `Conflict ("Account already registered for user: " ^ username)

let handle_list registry request =
  with_auth registry request (fun account ->
    let accounts = Account.list_for_account registry account in
    let json = `List (List.map Account.account_to_json accounts) in
    json_response (Yojson.Safe.to_string json))

let handle_get registry request id =
  with_auth registry request (fun account ->
    if account.Account.id <> id then
      json_error `Not_Found "Account not found"
    else
      json_response (Yojson.Safe.to_string (Account.account_to_json account)))

let handle_update registry sync_scheduler base_dir request id =
  with_auth registry request (fun account ->
    if account.Account.id <> id then
      json_error `Not_Found "Account not found"
    else
      match%lwt body_json request with
      | Error msg -> json_error `Bad_Request msg
      | Ok json ->
        match register_body_of_yojson json with
        | Error msg -> json_error `Bad_Request (friendly_error msg)
        | Ok body ->
          let imap : Account.imap_config = {
            host = body.host;
            port = body.port;
            username = body.username;
            password = body.password;
            use_tls = body.use_tls;
          } in
          let updated = Account.update registry account ~imap in
          let do_sync = Dream.query request "sync" = Some "true" in
          if do_sync then begin
            let%lwt _result = Sync.try_sync sync_scheduler ~base_dir updated in
            json_response (Yojson.Safe.to_string (Account.account_to_json updated))
          end else
            json_response (Yojson.Safe.to_string (Account.account_to_json updated)))

let handle_delete registry request =
  with_auth registry request (fun account ->
    Account.unregister registry account;
    json_response {|{"ok": true}|})

let handle_reroll_key registry request =
  with_auth registry request (fun account ->
    let updated = Account.reroll_api_key registry account in
    json_response
      (Yojson.Safe.to_string (`Assoc [
        "api_key", `String updated.api_key;
      ])))

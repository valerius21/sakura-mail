let json_response ?(status = `OK) body =
  Dream.json ~status body

let json_error status message =
  Dream.json ~status
    (Printf.sprintf {|{"error": %s}|} (Yojson.Safe.to_string (`String message)))

let body_json request =
  let%lwt body = Dream.body request in
  try Lwt.return_ok (Yojson.Safe.from_string body)
  with _ -> Lwt.return_error "Invalid JSON"

let bearer_token request =
  match Dream.header request "Authorization" with
  | Some h ->
    let prefix = "Bearer " in
    if String.length h > String.length prefix
       && String.sub h 0 (String.length prefix) = prefix then
      Some (String.sub h (String.length prefix) (String.length h - String.length prefix))
    else
      None
  | None -> None

let require_auth registry request =
  match bearer_token request with
  | None -> Lwt.return_error "Missing Authorization header"
  | Some key ->
    match Account.find_by_api_key registry key with
    | None -> Lwt.return_error "Invalid API key"
    | Some account -> Lwt.return_ok account

let with_auth registry request handler =
  match%lwt require_auth registry request with
  | Error msg -> json_error `Unauthorized msg
  | Ok account -> handler account

let with_account_maildir registry base_dir request f =
  with_auth registry request (fun account ->
    let mdir = Account.maildir_path ~base_dir account.id in
    f mdir)

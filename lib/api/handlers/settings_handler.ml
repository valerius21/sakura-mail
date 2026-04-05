open Api_common

let handle_get registry settings request =
  with_auth registry request (fun _account ->
    json_response (Yojson.Safe.to_string (Settings.to_json settings)))

let handle_update registry settings request =
  with_auth registry request (fun _account ->
    match%lwt body_json request with
    | Error err -> json_error `Bad_Request err
    | Ok json ->
      let _updated = Settings.update settings json in
      json_response (Yojson.Safe.to_string (Settings.to_json settings)))

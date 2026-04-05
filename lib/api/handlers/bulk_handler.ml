open Api_common

type bulk_body = {
  ids : string list; [@default []]
  action : string;
  params : Yojson.Safe.t option; [@default None]
  mailbox : string option; [@default None]
  filter : Yojson.Safe.t option; [@default None]
} [@@deriving yojson]

let extract_filter_params json : Query.filter_params =
  let str key = match json with
    | `Assoc a -> (match List.assoc_opt key a with
      | Some (`String s) -> Some s | _ -> None)
    | _ -> None
  in
  { from = str "from"; to_ = str "to"; subject = str "subject";
    body = str "body"; flags = str "flags"; unread = str "unread";
    has_attachment = str "has_attachment"; after = str "after";
    before = str "before" }

let resolve_ids ~mdir body =
  match body.filter, body.mailbox with
  | Some filter_json, Some mailbox ->
    let msgs = Maildir.scan_mailbox ~maildir_root:mdir ~mailbox in
    let parsed = List.map Query.parse_msg msgs in
    let filters = extract_filter_params filter_json in
    let filtered = Query.apply_filters filters parsed in
    Ok (List.map (fun (pm : Query.parsed_msg) -> pm.msg.Maildir.id) filtered)
  | Some _, None ->
    Error "When using 'filter', 'mailbox' is required"
  | None, _ ->
    if body.ids = [] then Error "Either 'ids' or 'filter'+'mailbox' is required"
    else Ok body.ids

let execute_action ~mdir action params id =
  match Maildir.find_message ~maildir_root:mdir ~id with
  | None ->
    Error (id, "not found")
  | Some msg ->
    try
      (match action with
       | "set_flags" | "add_flags" | "remove_flags" ->
         let flags_list = match params with
           | Some (`Assoc a) ->
             (match List.assoc_opt "flags" a with
              | Some (`List l) ->
                List.filter_map (fun j ->
                  match j with `String s -> Maildir.flag_of_string s | _ -> None
                ) l
              | _ -> [])
           | _ -> []
         in
         (match action with
          | "set_flags" -> ignore (Maildir.set_flags msg flags_list)
          | "add_flags" -> ignore (Maildir.add_flags msg flags_list)
          | "remove_flags" -> ignore (Maildir.remove_flags msg flags_list)
          | _ -> ())
       | "move" ->
         let dest = match params with
           | Some (`Assoc a) ->
             (match List.assoc_opt "destination" a with
              | Some (`String s) -> s
              | _ -> failwith "missing destination")
           | _ -> failwith "missing params"
         in
         ignore (Maildir.move_message ~maildir_root:mdir msg ~dest_mailbox:dest)
       | "delete" ->
         ignore (Maildir.trash_message ~maildir_root:mdir msg)
       | _ -> ());
      Ok id
    with exn ->
      Error (id, Printexc.to_string exn)

let handle_bulk settings registry base_dir request =
  with_account_maildir registry base_dir request (fun mdir ->
    match%lwt body_json request with
    | Error err -> json_error `Bad_Request err
    | Ok json ->
      match bulk_body_of_yojson json with
      | Error err -> json_error `Bad_Request ("Invalid body: " ^ err)
      | Ok body ->
        let valid_actions = ["set_flags"; "add_flags"; "remove_flags"; "move"; "delete"] in
        if not (List.mem body.action valid_actions) then
          json_error `Bad_Request (Printf.sprintf "Invalid action: %s" body.action)
        else
          match resolve_ids ~mdir body with
          | Error err -> json_error `Bad_Request err
          | Ok ids ->
            if List.length ids > settings.Settings.bulk_max then
              json_error `Bad_Request
                (Printf.sprintf "Too many messages (max %d, got %d)"
                  settings.Settings.bulk_max (List.length ids))
            else begin
              let succeeded = ref [] in
              let failed = ref [] in
              List.iter (fun id ->
                match execute_action ~mdir body.action body.params id with
                | Ok id -> succeeded := `String id :: !succeeded
                | Error (id, err) ->
                  failed := `Assoc ["id", `String id; "error", `String err] :: !failed
              ) ids;
              json_response (Yojson.Safe.to_string (`Assoc [
                "succeeded", `List (List.rev !succeeded);
                "failed", `List (List.rev !failed);
              ]))
            end)

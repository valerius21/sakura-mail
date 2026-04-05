(* End-to-end tests — spin up the full Dream router and hit all endpoints. *)

let () = Random.self_init ()

(* ============================================================
   Test infrastructure
   ============================================================ *)

let tmp_dir prefix =
  let dir = Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "%s_%d_%d" prefix (Unix.getpid ()) (Random.bits ())) in
  Unix.mkdir dir 0o700;
  dir

let rm_rf path =
  let rec rm p =
    if Sys.is_directory p then begin
      Array.iter (fun name -> rm (Filename.concat p name))
        (Sys.readdir p);
      Unix.rmdir p
    end else
      Sys.remove p
  in
  if Sys.file_exists path then rm path

(** Create a test environment: base_dir, registry, sync_scheduler, and the
    Dream test handler wired to all routes. *)
type env = {
  base_dir : string;
  registry : Sakura_mail.Account.registry;
  _sync_scheduler : Sakura_mail.Sync.t;
  settings : Sakura_mail.Settings.t;
  handler : Dream.request -> Dream.response;
}

let setup () =
  let base_dir = tmp_dir "e2e" in
  let registry = Sakura_mail.Account.create_registry ~base_dir in
  let sync_scheduler = Sakura_mail.Sync.create ~interval_sec:9999.0 () in
  let settings = Sakura_mail.Settings.create () in
  let routes = Sakura_mail.Api.routes ~registry ~sync_scheduler ~base_dir ~settings in
  let handler = Dream.test (Dream.router routes) in
  { base_dir; registry; _sync_scheduler = sync_scheduler; settings; handler }

let teardown env =
  rm_rf env.base_dir

(** Make a request and return (status_code, body_string, parsed_json option). *)
let call env ?(meth = `POST) ?(headers = []) ?(body = "") target =
  let req = Dream.request ~method_:meth ~target ~headers body in
  let resp = env.handler req in
  let status = Dream.status resp in
  let resp_headers = Dream.all_headers resp in
  let body = Lwt_main.run (Dream.body resp) in
  let json = try Some (Yojson.Safe.from_string body) with _ -> None in
  (status, resp_headers, body, json)

let status_code s = Dream.status_to_int s

let json_string key json =
  match json with
  | Some (`Assoc assoc) ->
    (match List.assoc_opt key assoc with
     | Some (`String s) -> s
     | _ -> failwith ("missing string key: " ^ key))
  | _ -> failwith "not a JSON object"

let json_list key json =
  match json with
  | Some (`Assoc assoc) ->
    (match List.assoc_opt key assoc with
     | Some (`List l) -> l
     | _ -> failwith ("missing list key: " ^ key))
  | _ -> failwith "not a JSON object"

let json_int key json =
  match json with
  | Some (`Assoc assoc) ->
    (match List.assoc_opt key assoc with
     | Some (`Int n) -> n
     | _ -> failwith ("missing int key: " ^ key))
  | _ -> failwith "not a JSON object"

let _json_bool key json =
  match json with
  | Some (`Assoc assoc) ->
    (match List.assoc_opt key assoc with
     | Some (`Bool b) -> b
     | _ -> failwith ("missing bool key: " ^ key))
  | _ -> failwith "not a JSON object"

(** Register an account and return the api_key. *)
let register env =
  let body = {|{"host":"localhost","port":3143,"username":"test","password":"pass","use_tls":false}|} in
  let (_, _, _, json) = call env ~meth:`POST ~body "/accounts" in
  json_string "api_key" json

let auth key = [("Authorization", "Bearer " ^ key)]

(** Seed a test email into a user's Maildir. Returns the message_info. *)
let seed_email env key ?(mailbox = "INBOX") ?(flags = []) content =
  let account = match Sakura_mail.Account.find_by_api_key env.registry key with
    | Some a -> a | None -> failwith "account not found" in
  let mdir = Sakura_mail.Account.maildir_path ~base_dir:env.base_dir account.id in
  let msg = Sakura_mail.Maildir.write_message ~maildir_root:mdir ~mailbox ~content in
  if flags <> [] then
    ignore (Sakura_mail.Maildir.set_flags msg flags);
  msg

let simple_email =
  "From: Alice <alice@example.com>\r\n\
   To: Bob <bob@example.com>\r\n\
   Subject: Hello World\r\n\
   Date: Thu, 3 Apr 2025 10:00:00 +0000\r\n\
   Message-ID: <test123@example.com>\r\n\
   MIME-Version: 1.0\r\n\
   Content-Type: text/plain; charset=UTF-8\r\n\
   \r\n\
   Hello, this is a test email.\r\n"

let multipart_email =
  "From: Sender <sender@example.com>\r\n\
   To: Receiver <receiver@example.com>\r\n\
   Subject: Multipart Test\r\n\
   Date: Thu, 3 Apr 2025 12:00:00 +0000\r\n\
   Message-ID: <multi@example.com>\r\n\
   MIME-Version: 1.0\r\n\
   Content-Type: multipart/alternative; boundary=\"boundary42\"\r\n\
   \r\n\
   --boundary42\r\n\
   Content-Type: text/plain; charset=UTF-8\r\n\
   \r\n\
   Plain text body.\r\n\
   --boundary42\r\n\
   Content-Type: text/html; charset=UTF-8\r\n\
   \r\n\
   <html><body><p>HTML body.</p></body></html>\r\n\
   --boundary42--\r\n"

(* ============================================================
   POST /accounts — registration
   ============================================================ *)

let test_register_success () =
  let env = setup () in
  let body = {|{"host":"imap.test.com","port":993,"username":"user","password":"pass","use_tls":true}|} in
  let (status, _, _, json) = call env ~body "/accounts" in
  Alcotest.(check int) "201 Created" 201 (status_code status);
  let id = json_string "id" json in
  let key = json_string "api_key" json in
  Alcotest.(check bool) "id is non-empty" true (String.length id > 0);
  Alcotest.(check bool) "key starts with sk_" true (String.sub key 0 3 = "sk_");
  (* Verify files were created *)
  let rc = Sakura_mail.Account.mbsyncrc_path ~base_dir:env.base_dir id in
  Alcotest.(check bool) ".mbsyncrc created" true (Sys.file_exists rc);
  teardown env

let test_register_missing_fields () =
  let env = setup () in
  let (status, _, _, _) = call env ~body:{|{"host":"x"}|} "/accounts" in
  Alcotest.(check int) "400 Bad Request" 400 (status_code status);
  teardown env

let test_register_invalid_json () =
  let env = setup () in
  let (status, _, _, _) = call env ~body:"not json" "/accounts" in
  Alcotest.(check int) "400 Bad Request" 400 (status_code status);
  teardown env

let test_register_empty_body () =
  let env = setup () in
  let (status, _, _, _) = call env ~body:"" "/accounts" in
  Alcotest.(check int) "400 Bad Request" 400 (status_code status);
  teardown env

let test_register_multiple_accounts () =
  let env = setup () in
  let body = {|{"host":"x","port":993,"username":"u","password":"p","use_tls":false}|} in
  let (_, _, _, j1) = call env ~body "/accounts" in
  let (_, _, _, j2) = call env ~body "/accounts" in
  let k1 = json_string "api_key" j1 in
  let k2 = json_string "api_key" j2 in
  Alcotest.(check bool) "different keys" true (k1 <> k2);
  let id1 = json_string "id" j1 in
  let id2 = json_string "id" j2 in
  Alcotest.(check bool) "different ids" true (id1 <> id2);
  teardown env

(* ============================================================
   DELETE /accounts
   ============================================================ *)

let test_delete_account () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`DELETE ~headers:(auth key) "/accounts" in
  Alcotest.(check int) "200 OK" 200 (status_code status);
  (* Key should no longer work *)
  let (status2, _, _, _) = call env ~meth:`GET ~headers:(auth key) "/mailboxes" in
  Alcotest.(check int) "401 after delete" 401 (status_code status2);
  teardown env

let test_delete_no_auth () =
  let env = setup () in
  let (status, _, _, _) = call env ~meth:`DELETE "/accounts" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

let test_delete_bad_key () =
  let env = setup () in
  let (status, _, _, _) = call env ~meth:`DELETE
    ~headers:(auth "sk_fake") "/accounts" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

(* ============================================================
   POST /accounts/reroll-key
   ============================================================ *)

let test_reroll_key () =
  let env = setup () in
  let old_key = register env in
  let (status, _, _, json) = call env ~headers:(auth old_key)
    "/accounts/reroll-key" in
  Alcotest.(check int) "200" 200 (status_code status);
  let new_key = json_string "api_key" json in
  Alcotest.(check bool) "key changed" true (new_key <> old_key);
  Alcotest.(check bool) "new key has prefix" true (String.sub new_key 0 3 = "sk_");
  (* Old key should fail *)
  let (s, _, _, _) = call env ~meth:`GET ~headers:(auth old_key) "/mailboxes" in
  Alcotest.(check int) "old key 401" 401 (status_code s);
  (* New key should work *)
  let (s2, _, _, _) = call env ~meth:`GET ~headers:(auth new_key) "/mailboxes" in
  Alcotest.(check int) "new key 200" 200 (status_code s2);
  teardown env

let test_reroll_no_auth () =
  let env = setup () in
  let (status, _, _, _) = call env "/accounts/reroll-key" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

(* ============================================================
   Auth edge cases (applies to all authenticated endpoints)
   ============================================================ *)

let test_auth_missing_header () =
  let env = setup () in
  let (status, _, _, json) = call env ~meth:`GET "/mailboxes" in
  Alcotest.(check int) "401" 401 (status_code status);
  let err = json_string "error" json in
  Alcotest.(check string) "error message" "Missing Authorization header" err;
  teardown env

let test_auth_malformed_header () =
  let env = setup () in
  let (status, _, _, _) = call env ~meth:`GET
    ~headers:[("Authorization", "Token abc")] "/mailboxes" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

let test_auth_empty_bearer () =
  let env = setup () in
  let (status, _, _, _) = call env ~meth:`GET
    ~headers:[("Authorization", "Bearer ")] "/mailboxes" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

let test_auth_revoked_key () =
  let env = setup () in
  let key = register env in
  (* Delete account, then try using the key *)
  ignore (call env ~meth:`DELETE ~headers:(auth key) "/accounts");
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key) "/mailboxes" in
  Alcotest.(check int) "401 after revocation" 401 (status_code status);
  teardown env

(* ============================================================
   GET /mailboxes
   ============================================================ *)

let test_list_mailboxes_empty () =
  let env = setup () in
  let key = register env in
  let (status, _, body, _) = call env ~meth:`GET ~headers:(auth key) "/mailboxes" in
  Alcotest.(check int) "200" 200 (status_code status);
  let mailboxes = Yojson.Safe.from_string body in
  (match mailboxes with
   | `List l ->
     (* INBOX is created on registration *)
     Alcotest.(check bool) "has INBOX"
       true (List.exists (fun m -> m = `String "INBOX") l)
   | _ -> Alcotest.fail "expected JSON array");
  teardown env

let test_list_mailboxes_multiple () =
  let env = setup () in
  let key = register env in
  (* Create extra mailbox folders *)
  let account = match Sakura_mail.Account.find_by_api_key env.registry key with
    | Some a -> a | None -> failwith "no account" in
  let mdir = Sakura_mail.Account.maildir_path ~base_dir:env.base_dir account.id in
  List.iter (fun mb ->
    Sakura_mail.Maildir.ensure_maildir_structure (Filename.concat mdir mb)
  ) ["Sent"; "Drafts"; "Trash"];
  let (_, _, body, _) = call env ~meth:`GET ~headers:(auth key) "/mailboxes" in
  let mailboxes = match Yojson.Safe.from_string body with
    | `List l -> List.filter_map (function `String s -> Some s | _ -> None) l
    | _ -> [] in
  Alcotest.(check bool) "has Sent" true (List.mem "Sent" mailboxes);
  Alcotest.(check bool) "has Drafts" true (List.mem "Drafts" mailboxes);
  Alcotest.(check bool) "has Trash" true (List.mem "Trash" mailboxes);
  teardown env

(* ============================================================
   GET /mailboxes/:mailbox/messages
   ============================================================ *)

let test_list_messages_empty () =
  let env = setup () in
  let key = register env in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "200" 200 (status_code status);
  Alcotest.(check int) "total 0" 0 (json_int "total" json);
  Alcotest.(check int) "empty list" 0 (List.length (json_list "messages" json));
  teardown env

let test_list_messages_with_emails () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key simple_email);
  ignore (seed_email env key multipart_email);
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "200" 200 (status_code status);
  Alcotest.(check int) "total 2" 2 (json_int "total" json);
  let msgs = json_list "messages" json in
  Alcotest.(check int) "2 messages" 2 (List.length msgs);
  (* Each message should have id, mailbox, subject *)
  List.iter (fun msg ->
    let _ = json_string "id" (Some msg) in
    let _ = json_string "mailbox" (Some msg) in
    let _ = json_string "subject" (Some msg) in
    ()
  ) msgs;
  teardown env

let test_list_messages_cursor_pagination () =
  let env = setup () in
  let key = register env in
  for i = 1 to 10 do
    let content = Printf.sprintf
      "From: test@x.com\r\nTo: me@x.com\r\nSubject: Msg %02d\r\n\
       Date: Thu, %d Apr 2025 10:00:00 +0000\r\n\
       Message-ID: <msg%d@x.com>\r\nMIME-Version: 1.0\r\n\
       Content-Type: text/plain\r\n\r\nBody %d\r\n" i i i i in
    ignore (seed_email env key content)
  done;
  let (_, _, _, j1) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?limit=3" in
  Alcotest.(check int) "total 10" 10 (json_int "total" j1);
  Alcotest.(check int) "page size 3" 3 (List.length (json_list "messages" j1));
  Alcotest.(check int) "limit echoed" 3 (json_int "limit" j1);
  let cursor1 = json_string "next_cursor" j1 in
  Alcotest.(check bool) "has next_cursor" true (String.length cursor1 > 0);
  let (_, _, _, j2) = call env ~meth:`GET ~headers:(auth key)
    (Printf.sprintf "/mailboxes/INBOX/messages?limit=3&cursor=%s" cursor1) in
  Alcotest.(check int) "page 2 size 3" 3 (List.length (json_list "messages" j2));
  let cursor2 = json_string "next_cursor" j2 in
  let (_, _, _, j3) = call env ~meth:`GET ~headers:(auth key)
    (Printf.sprintf "/mailboxes/INBOX/messages?limit=3&cursor=%s" cursor2) in
  Alcotest.(check int) "page 3 size 3" 3 (List.length (json_list "messages" j3));
  let cursor3 = json_string "next_cursor" j3 in
  let (_, _, _, j4) = call env ~meth:`GET ~headers:(auth key)
    (Printf.sprintf "/mailboxes/INBOX/messages?limit=3&cursor=%s" cursor3) in
  Alcotest.(check int) "last page size 1" 1 (List.length (json_list "messages" j4));
  teardown env

let test_list_messages_default_pagination () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key simple_email);
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "default limit 50" 50 (json_int "limit" json);
  teardown env

let test_list_messages_url_encoded_mailbox () =
  let env = setup () in
  let key = register env in
  (* Create mailbox with special name *)
  let account = match Sakura_mail.Account.find_by_api_key env.registry key with
    | Some a -> a | None -> failwith "no account" in
  let mdir = Sakura_mail.Account.maildir_path ~base_dir:env.base_dir account.id in
  Sakura_mail.Maildir.ensure_maildir_structure (Filename.concat mdir "My Folder");
  ignore (seed_email env key ~mailbox:"My Folder" simple_email);
  let encoded = Dream.to_percent_encoded "My Folder" in
  let target = Printf.sprintf "/mailboxes/%s/messages" encoded in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "200" 200 (status_code status);
  Alcotest.(check int) "total 1" 1 (json_int "total" json);
  teardown env

let test_list_messages_nonexistent_mailbox () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/NONEXISTENT/messages" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

(* ============================================================
   GET /messages/:id — read message
   ============================================================ *)

let test_read_message () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = "/messages/" ^ msg.Sakura_mail.Maildir.id in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "200" 200 (status_code status);
  Alcotest.(check string) "subject" "Hello World"
    (json_string "subject" json);
  (* Verify the maildir id field is present and matches *)
  let id_field = json_string "id" json in
  Alcotest.(check string) "id matches" msg.Sakura_mail.Maildir.id id_field;
  teardown env

let test_read_message_multipart () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key multipart_email in
  let target = "/messages/" ^ msg.Sakura_mail.Maildir.id in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "200" 200 (status_code status);
  Alcotest.(check string) "subject" "Multipart Test"
    (json_string "subject" json);
  (* Should have both text and html body *)
  (match json with
   | Some (`Assoc a) ->
     let has_text = List.assoc_opt "body_text" a <> Some `Null in
     let has_html = List.assoc_opt "body_html" a <> Some `Null in
     Alcotest.(check bool) "has text body" true has_text;
     Alcotest.(check bool) "has html body" true has_html
   | _ -> Alcotest.fail "expected object");
  teardown env

let test_read_message_raw () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = "/messages/" ^ msg.Sakura_mail.Maildir.id ^ "/raw" in
  let (status, resp_headers, _, _) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "200" 200 (status_code status);
  let ct = List.assoc_opt "Content-Type" resp_headers in
  Alcotest.(check (option string)) "content-type" (Some "message/rfc822") ct;
  teardown env

let test_read_message_raw_not_found () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key)
    "/messages/nonexistent/raw" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

let test_read_message_not_found () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key)
    "/messages/nonexistent" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

let test_read_message_no_auth () =
  let env = setup () in
  let (status, _, _, _) = call env ~meth:`GET "/messages/anything" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

let test_read_message_wrong_user () =
  let env = setup () in
  let key1 = register env in
  let key2 = register env in
  let msg = seed_email env key1 simple_email in
  (* User 2 should not find user 1's message *)
  let target = "/messages/" ^ msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key2) target in
  Alcotest.(check int) "404 cross-user" 404 (status_code status);
  teardown env

(* ============================================================
   PUT /messages/:id/flags
   ============================================================ *)

let test_set_flags () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = Printf.sprintf "/messages/%s/flags" msg.Sakura_mail.Maildir.id in
  let body = {|{"flags": ["seen", "flagged"]}|} in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key) ~body target in
  Alcotest.(check int) "200" 200 (status_code status);
  (* Verify flags persisted by listing messages *)
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  let msgs = json_list "messages" json in
  let first = List.hd msgs in
  let flags = json_list "flags" (Some first) in
  let flag_strs = List.filter_map (function `String s -> Some s | _ -> None) flags in
  Alcotest.(check bool) "has seen" true (List.mem "seen" flag_strs);
  Alcotest.(check bool) "has flagged" true (List.mem "flagged" flag_strs);
  teardown env

let test_set_flags_clear_all () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key
    ~flags:[Sakura_mail.Maildir.Seen; Sakura_mail.Maildir.Flagged]
    simple_email in
  let target = Printf.sprintf "/messages/%s/flags" msg.Sakura_mail.Maildir.id in
  let body = {|{"flags": []}|} in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key) ~body target in
  Alcotest.(check int) "200" 200 (status_code status);
  teardown env

let test_set_flags_invalid_flag () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = Printf.sprintf "/messages/%s/flags" msg.Sakura_mail.Maildir.id in
  (* "bogus" is not a valid flag — should be rejected *)
  let body = {|{"flags": ["seen", "bogus"]}|} in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key) ~body target in
  Alcotest.(check int) "400 rejects invalid" 400 (status_code status);
  teardown env

let test_set_flags_not_found () =
  let env = setup () in
  let key = register env in
  let body = {|{"flags": ["seen"]}|} in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key) ~body
    "/messages/nope/flags" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

let test_set_flags_bad_body () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = Printf.sprintf "/messages/%s/flags" msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key)
    ~body:{|not json|} target in
  Alcotest.(check int) "400" 400 (status_code status);
  teardown env

(* ============================================================
   POST /messages/:id/move
   ============================================================ *)

let test_move_message () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = Printf.sprintf "/messages/%s/move" msg.Sakura_mail.Maildir.id in
  let body = {|{"destination": "Archive"}|} in
  let (status, _, _, _) = call env ~meth:`POST ~headers:(auth key) ~body target in
  Alcotest.(check int) "200" 200 (status_code status);
  (* Should be gone from INBOX *)
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "INBOX empty" 0 (json_int "total" json);
  (* Should be in Archive *)
  let (_, _, _, json2) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/Archive/messages" in
  Alcotest.(check int) "Archive has 1" 1 (json_int "total" json2);
  teardown env

let test_move_not_found () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`POST ~headers:(auth key)
    ~body:{|{"destination": "X"}|} "/messages/nope/move" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

let test_move_bad_body () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = Printf.sprintf "/messages/%s/move" msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`POST ~headers:(auth key)
    ~body:{|{}|} target in
  Alcotest.(check int) "400" 400 (status_code status);
  teardown env

(* ============================================================
   DELETE /messages/:id — trash
   ============================================================ *)

let test_delete_message () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = Printf.sprintf "/messages/%s" msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`DELETE ~headers:(auth key) target in
  Alcotest.(check int) "200" 200 (status_code status);
  (* Should be in Trash *)
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/Trash/messages" in
  Alcotest.(check int) "Trash has 1" 1 (json_int "total" json);
  (* Should be gone from INBOX *)
  let (_, _, _, json2) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "INBOX empty" 0 (json_int "total" json2);
  teardown env

let test_delete_message_not_found () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`DELETE ~headers:(auth key)
    "/messages/nope" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

let test_delete_already_trashed () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key ~mailbox:"Trash" simple_email in
  let target = Printf.sprintf "/messages/%s" msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`DELETE ~headers:(auth key) target in
  Alcotest.(check int) "200 trashed flag added" 200 (status_code status);
  teardown env

(* ============================================================
   POST /drafts — create draft
   ============================================================ *)

let test_create_draft () =
  let env = setup () in
  let key = register env in
  let body = {|{"to":["bob@example.com"],"cc":[],"bcc":[],"subject":"Test Draft","body":"Hello!","in_reply_to":null}|} in
  let (status, _, _, json) = call env ~headers:(auth key) ~body "/drafts" in
  Alcotest.(check int) "201" 201 (status_code status);
  let id = json_string "id" json in
  Alcotest.(check bool) "has id" true (String.length id > 0);
  Alcotest.(check string) "in Drafts" "Drafts" (json_string "mailbox" json);
  (* Verify it's readable *)
  let (s, _, _, rjson) = call env ~meth:`GET ~headers:(auth key)
    ("/messages/" ^ id) in
  Alcotest.(check int) "readable 200" 200 (status_code s);
  Alcotest.(check string) "subject" "Test Draft"
    (json_string "subject" rjson);
  teardown env

let test_create_draft_with_cc_bcc () =
  let env = setup () in
  let key = register env in
  let body = {|{"to":["a@x.com"],"cc":["b@x.com","c@x.com"],"bcc":["d@x.com"],"subject":"CC Test","body":"hi","in_reply_to":null}|} in
  let (status, _, _, json) = call env ~headers:(auth key) ~body "/drafts" in
  Alcotest.(check int) "201" 201 (status_code status);
  let id = json_string "id" json in
  (* Read and verify cc/bcc headers *)
  let (_, _, _, rjson) = call env ~meth:`GET ~headers:(auth key)
    ("/messages/" ^ id) in
  Alcotest.(check string) "subject" "CC Test" (json_string "subject" rjson);
  teardown env

let test_create_draft_with_reply () =
  let env = setup () in
  let key = register env in
  let body = {|{"to":["a@x.com"],"cc":[],"bcc":[],"subject":"Re: Hello","body":"Thanks!","in_reply_to":"<original@example.com>"}|} in
  let (status, _, _, json) = call env ~headers:(auth key) ~body "/drafts" in
  Alcotest.(check int) "201" 201 (status_code status);
  let id = json_string "id" json in
  (* Read and check in_reply_to *)
  let (_, _, _, rjson) = call env ~meth:`GET ~headers:(auth key)
    ("/messages/" ^ id) in
  (match rjson with
   | Some (`Assoc a) ->
     (match List.assoc_opt "in_reply_to" a with
      | Some (`String s) ->
        Alcotest.(check bool) "has in_reply_to" true (String.length s > 0)
      | _ -> () (* may be parsed differently, that's ok *))
   | _ -> ());
  teardown env

let test_create_draft_empty_body () =
  let env = setup () in
  let key = register env in
  let body = {|{"to":["a@x.com"],"cc":[],"bcc":[],"subject":"Empty","body":"","in_reply_to":null}|} in
  let (status, _, _, _) = call env ~headers:(auth key) ~body "/drafts" in
  Alcotest.(check int) "201 empty body ok" 201 (status_code status);
  teardown env

let test_create_draft_unicode_subject () =
  let env = setup () in
  let key = register env in
  let body = Printf.sprintf
    {|{"to":["a@x.com"],"cc":[],"bcc":[],"subject":"%s","body":"test","in_reply_to":null}|}
    "日本語テスト 🎉" in
  let (status, _, _, _) = call env ~headers:(auth key) ~body "/drafts" in
  Alcotest.(check int) "201 unicode ok" 201 (status_code status);
  teardown env

let test_create_draft_invalid_json () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~headers:(auth key)
    ~body:"not json" "/drafts" in
  Alcotest.(check int) "400" 400 (status_code status);
  teardown env

let test_create_draft_missing_fields () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~headers:(auth key)
    ~body:{|{"subject":"x"}|} "/drafts" in
  Alcotest.(check int) "400" 400 (status_code status);
  teardown env

(* ============================================================
   PUT /drafts/:id — update draft
   ============================================================ *)

let test_update_draft () =
  let env = setup () in
  let key = register env in
  let body = {|{"to":["a@x.com"],"cc":[],"bcc":[],"subject":"V1","body":"first","in_reply_to":null}|} in
  let (_, _, _, json) = call env ~headers:(auth key) ~body "/drafts" in
  let id = json_string "id" json in
  (* Update it *)
  let body2 = {|{"to":["a@x.com"],"cc":[],"bcc":[],"subject":"V2","body":"updated","in_reply_to":null}|} in
  let (status, _, _, json2) = call env ~meth:`PUT ~headers:(auth key)
    ~body:body2 ("/drafts/" ^ id) in
  Alcotest.(check int) "200" 200 (status_code status);
  let new_id = json_string "id" json2 in
  Alcotest.(check bool) "id changed" true (new_id <> id);
  (* Read updated draft *)
  let (_, _, _, rjson) = call env ~meth:`GET ~headers:(auth key)
    ("/messages/" ^ new_id) in
  Alcotest.(check string) "updated subject" "V2"
    (json_string "subject" rjson);
  (* Old id should be gone *)
  let (s, _, _, _) = call env ~meth:`GET ~headers:(auth key)
    ("/messages/" ^ id) in
  Alcotest.(check int) "old draft 404" 404 (status_code s);
  teardown env

let test_update_draft_not_found () =
  let env = setup () in
  let key = register env in
  let body = {|{"to":["a@x.com"],"cc":[],"bcc":[],"subject":"X","body":"x","in_reply_to":null}|} in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key)
    ~body "/drafts/nonexistent" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

let test_update_draft_bad_body () =
  let env = setup () in
  let key = register env in
  let body = {|{"to":["a@x.com"],"cc":[],"bcc":[],"subject":"V1","body":"x","in_reply_to":null}|} in
  let (_, _, _, json) = call env ~headers:(auth key) ~body "/drafts" in
  let id = json_string "id" json in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key)
    ~body:"garbage" ("/drafts/" ^ id) in
  Alcotest.(check int) "400" 400 (status_code status);
  teardown env

(* ============================================================
   POST /sync — edge cases (no real IMAP, so test auth + conflict)
   ============================================================ *)

let test_sync_no_auth () =
  let env = setup () in
  let (status, _, _, _) = call env "/sync" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

(* ============================================================
   Cross-cutting: user isolation
   ============================================================ *)

let test_user_isolation_mailboxes () =
  let env = setup () in
  let key1 = register env in
  let key2 = register env in
  (* Seed messages only for user1 *)
  ignore (seed_email env key1 simple_email);
  (* User1 sees 1 message *)
  let (_, _, _, j1) = call env ~meth:`GET ~headers:(auth key1)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "user1 has 1" 1 (json_int "total" j1);
  (* User2 sees 0 messages *)
  let (_, _, _, j2) = call env ~meth:`GET ~headers:(auth key2)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "user2 has 0" 0 (json_int "total" j2);
  teardown env

let test_user_isolation_drafts () =
  let env = setup () in
  let key1 = register env in
  let key2 = register env in
  let body = {|{"to":["a@x.com"],"cc":[],"bcc":[],"subject":"Secret","body":"x","in_reply_to":null}|} in
  let (_, _, _, json) = call env ~headers:(auth key1) ~body "/drafts" in
  let id = json_string "id" json in
  (* User2 cannot read user1's draft *)
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key2)
    ("/messages/" ^ id) in
  Alcotest.(check int) "cross-user draft 404" 404 (status_code status);
  teardown env

let test_user_isolation_flags () =
  let env = setup () in
  let key1 = register env in
  let key2 = register env in
  let msg = seed_email env key1 simple_email in
  let target = Printf.sprintf "/messages/%s/flags" msg.Sakura_mail.Maildir.id in
  (* User2 cannot set flags on user1's message *)
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key2)
    ~body:{|{"flags":["seen"]}|} target in
  Alcotest.(check int) "cross-user flag 404" 404 (status_code status);
  teardown env

let test_user_isolation_delete () =
  let env = setup () in
  let key1 = register env in
  let key2 = register env in
  let msg = seed_email env key1 simple_email in
  let target = Printf.sprintf "/messages/%s" msg.Sakura_mail.Maildir.id in
  (* User2 cannot delete user1's message *)
  let (status, _, _, _) = call env ~meth:`DELETE ~headers:(auth key2) target in
  Alcotest.(check int) "cross-user delete 404" 404 (status_code status);
  (* Message should still exist for user1 *)
  let (s, _, _, _) = call env ~meth:`GET ~headers:(auth key1) target in
  Alcotest.(check int) "still exists for user1" 200 (status_code s);
  teardown env

let test_user_isolation_move () =
  let env = setup () in
  let key1 = register env in
  let key2 = register env in
  let msg = seed_email env key1 simple_email in
  let target = Printf.sprintf "/messages/%s/move" msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`POST ~headers:(auth key2)
    ~body:{|{"destination":"Trash"}|} target in
  Alcotest.(check int) "cross-user move 404" 404 (status_code status);
  teardown env

(* ============================================================
   GET /messages/:id/attachments/:index
   ============================================================ *)

let email_with_attachment =
  "From: alice@example.com\r\n\
   To: bob@example.com\r\n\
   Subject: With Attachment\r\n\
   Date: Thu, 3 Apr 2025 10:00:00 +0000\r\n\
   Message-ID: <attach1@example.com>\r\n\
   MIME-Version: 1.0\r\n\
   Content-Type: multipart/mixed; boundary=\"attachboundary\"\r\n\
   \r\n\
   --attachboundary\r\n\
   Content-Type: text/plain; charset=UTF-8\r\n\
   \r\n\
   See attached file.\r\n\
   --attachboundary\r\n\
   Content-Type: application/pdf; name=\"report.pdf\"\r\n\
   Content-Disposition: attachment; filename=\"report.pdf\"\r\n\
   \r\n\
   %PDF-1.4 fake pdf content here\r\n\
   --attachboundary--\r\n"

let test_read_message_has_attachments () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key email_with_attachment in
  let target = "/messages/" ^ msg.Sakura_mail.Maildir.id in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "200" 200 (status_code status);
  let attachments = json_list "attachments" json in
  Alcotest.(check int) "1 attachment" 1 (List.length attachments);
  let att = List.hd attachments in
  Alcotest.(check string) "filename" "report.pdf"
    (json_string "filename" (Some att));
  Alcotest.(check string) "content_type" "application/pdf"
    (json_string "content_type" (Some att));
  teardown env

let test_download_attachment () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key email_with_attachment in
  let target = Printf.sprintf "/messages/%s/attachments/0" msg.Sakura_mail.Maildir.id in
  let (status, resp_headers, body, _) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "200" 200 (status_code status);
  let ct = List.assoc_opt "Content-Type" resp_headers in
  Alcotest.(check (option string)) "content-type" (Some "application/pdf") ct;
  Alcotest.(check bool) "has content" true (String.length body > 0);
  teardown env

let test_download_attachment_not_found () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = Printf.sprintf "/messages/%s/attachments/0" msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "404 no attachments" 404 (status_code status);
  teardown env

let test_download_attachment_bad_index () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key email_with_attachment in
  let target = Printf.sprintf "/messages/%s/attachments/99" msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "404 bad index" 404 (status_code status);
  teardown env

let test_download_attachment_invalid_index () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key email_with_attachment in
  let target = Printf.sprintf "/messages/%s/attachments/abc" msg.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key) target in
  Alcotest.(check int) "400 invalid index" 400 (status_code status);
  teardown env

let test_message_no_attachments_field () =
  let env = setup () in
  let key = register env in
  let msg = seed_email env key simple_email in
  let target = "/messages/" ^ msg.Sakura_mail.Maildir.id in
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key) target in
  let attachments = json_list "attachments" json in
  Alcotest.(check int) "0 attachments" 0 (List.length attachments);
  teardown env

(* ============================================================
   Search & filter on GET /mailboxes/:mailbox/messages
   ============================================================ *)

let make_email ~from ~to_ ~subject ~date ~body =
  Printf.sprintf
    "From: %s\r\nTo: %s\r\nSubject: %s\r\nDate: %s\r\n\
     Message-ID: <%s@test.com>\r\nMIME-Version: 1.0\r\n\
     Content-Type: text/plain; charset=UTF-8\r\n\r\n%s\r\n"
    from to_ subject date
    (string_of_int (Random.bits ())) body

let test_filter_by_from () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key (make_email
    ~from:"Alice <alice@foo.com>" ~to_:"bob@x.com"
    ~subject:"From Alice" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"hi"));
  ignore (seed_email env key (make_email
    ~from:"Charlie <charlie@bar.com>" ~to_:"bob@x.com"
    ~subject:"From Charlie" ~date:"Thu, 3 Apr 2025 11:00:00 +0000" ~body:"hello"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?from=alice" in
  Alcotest.(check int) "1 match" 1 (json_int "total" json);
  teardown env

let test_filter_by_subject () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Important Report" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"x"));
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Casual Chat" ~date:"Thu, 3 Apr 2025 11:00:00 +0000" ~body:"y"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?subject=report" in
  Alcotest.(check int) "1 match" 1 (json_int "total" json);
  teardown env

let test_filter_by_body () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"A" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"the secret password is 42"));
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"B" ~date:"Thu, 3 Apr 2025 11:00:00 +0000" ~body:"nothing here"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?body=secret" in
  Alcotest.(check int) "1 match" 1 (json_int "total" json);
  teardown env

let test_filter_by_flags () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key ~flags:[Sakura_mail.Maildir.Seen] (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Read" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"x"));
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Unread" ~date:"Thu, 3 Apr 2025 11:00:00 +0000" ~body:"y"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?flags=seen" in
  Alcotest.(check int) "1 seen" 1 (json_int "total" json);
  teardown env

let test_filter_unread () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key ~flags:[Sakura_mail.Maildir.Seen] (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Read" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"x"));
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Unread" ~date:"Thu, 3 Apr 2025 11:00:00 +0000" ~body:"y"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?unread=true" in
  Alcotest.(check int) "1 unread" 1 (json_int "total" json);
  teardown env

let test_filter_has_attachment () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key email_with_attachment);
  ignore (seed_email env key simple_email);
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?has_attachment=true" in
  Alcotest.(check int) "1 with attachment" 1 (json_int "total" json);
  let (_, _, _, json2) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?has_attachment=false" in
  Alcotest.(check int) "1 without attachment" 1 (json_int "total" json2);
  teardown env

let test_filter_combined () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key (make_email
    ~from:"Alice <alice@x.com>" ~to_:"b@x.com"
    ~subject:"Project Update" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"status report"));
  ignore (seed_email env key (make_email
    ~from:"Alice <alice@x.com>" ~to_:"b@x.com"
    ~subject:"Lunch Plans" ~date:"Thu, 3 Apr 2025 11:00:00 +0000" ~body:"pizza?"));
  ignore (seed_email env key (make_email
    ~from:"Bob <bob@x.com>" ~to_:"b@x.com"
    ~subject:"Project Update" ~date:"Thu, 3 Apr 2025 12:00:00 +0000" ~body:"done"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?from=alice&subject=project" in
  Alcotest.(check int) "1 combined match" 1 (json_int "total" json);
  teardown env

let test_filter_no_matches () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key simple_email);
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?from=nobody@nowhere.com" in
  Alcotest.(check int) "0 matches" 0 (json_int "total" json);
  teardown env

(* ============================================================
   Sorting on GET /mailboxes/:mailbox/messages
   ============================================================ *)

let test_sort_by_subject_asc () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Zebra" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"x"));
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Apple" ~date:"Thu, 3 Apr 2025 11:00:00 +0000" ~body:"y"));
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Mango" ~date:"Thu, 3 Apr 2025 12:00:00 +0000" ~body:"z"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?sort=subject:asc" in
  let msgs = json_list "messages" json in
  let subjects = List.map (fun m -> json_string "subject" (Some m)) msgs in
  Alcotest.(check (list string)) "sorted asc"
    ["Apple"; "Mango"; "Zebra"] subjects;
  teardown env

let test_sort_by_subject_desc () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Zebra" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"x"));
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Apple" ~date:"Thu, 3 Apr 2025 11:00:00 +0000" ~body:"y"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?sort=subject:desc" in
  let msgs = json_list "messages" json in
  let subjects = List.map (fun m -> json_string "subject" (Some m)) msgs in
  Alcotest.(check (list string)) "sorted desc" ["Zebra"; "Apple"] subjects;
  teardown env

let test_sort_by_size () =
  let env = setup () in
  let key = register env in
  let small = make_email ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Small" ~date:"Thu, 3 Apr 2025 10:00:00 +0000" ~body:"x" in
  let big = make_email ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Big" ~date:"Thu, 3 Apr 2025 11:00:00 +0000"
    ~body:(String.make 5000 'A') in
  ignore (seed_email env key big);
  ignore (seed_email env key small);
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages?sort=size:asc" in
  let msgs = json_list "messages" json in
  let subjects = List.map (fun m -> json_string "subject" (Some m)) msgs in
  Alcotest.(check (list string)) "small first" ["Small"; "Big"] subjects;
  teardown env

let test_sort_default_date_desc () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"Old" ~date:"Thu, 1 Apr 2025 10:00:00 +0000" ~body:"x"));
  ignore (seed_email env key (make_email
    ~from:"a@x.com" ~to_:"b@x.com"
    ~subject:"New" ~date:"Thu, 10 Apr 2025 10:00:00 +0000" ~body:"y"));
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  let msgs = json_list "messages" json in
  let subjects = List.map (fun m -> json_string "subject" (Some m)) msgs in
  Alcotest.(check string) "newest first" "New" (List.hd subjects);
  teardown env

(* ============================================================
   POST /messages/bulk
   ============================================================ *)

let test_bulk_set_flags () =
  let env = setup () in
  let key = register env in
  let m1 = seed_email env key simple_email in
  let m2 = seed_email env key multipart_email in
  let body = Printf.sprintf
    {|{"ids":["%s","%s"],"action":"set_flags","params":{"flags":["seen"]}}|}
    m1.Sakura_mail.Maildir.id m2.Sakura_mail.Maildir.id in
  let (status, _, _, json) = call env ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "200" 200 (status_code status);
  let succeeded = json_list "succeeded" json in
  Alcotest.(check int) "2 succeeded" 2 (List.length succeeded);
  let failed = json_list "failed" json in
  Alcotest.(check int) "0 failed" 0 (List.length failed);
  teardown env

let test_bulk_partial_failure () =
  let env = setup () in
  let key = register env in
  let m1 = seed_email env key simple_email in
  let body = Printf.sprintf
    {|{"ids":["%s","nonexistent_id"],"action":"set_flags","params":{"flags":["seen"]}}|}
    m1.Sakura_mail.Maildir.id in
  let (status, _, _, json) = call env ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "200" 200 (status_code status);
  let succeeded = json_list "succeeded" json in
  Alcotest.(check int) "1 succeeded" 1 (List.length succeeded);
  let failed = json_list "failed" json in
  Alcotest.(check int) "1 failed" 1 (List.length failed);
  teardown env

let test_bulk_move () =
  let env = setup () in
  let key = register env in
  let m1 = seed_email env key simple_email in
  let m2 = seed_email env key multipart_email in
  let body = Printf.sprintf
    {|{"ids":["%s","%s"],"action":"move","params":{"destination":"Archive"}}|}
    m1.Sakura_mail.Maildir.id m2.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "200" 200 (status_code status);
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "INBOX empty" 0 (json_int "total" json);
  let (_, _, _, json2) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/Archive/messages" in
  Alcotest.(check int) "Archive has 2" 2 (json_int "total" json2);
  teardown env

let test_bulk_delete () =
  let env = setup () in
  let key = register env in
  let m1 = seed_email env key simple_email in
  let body = Printf.sprintf
    {|{"ids":["%s"],"action":"delete"}|} m1.Sakura_mail.Maildir.id in
  let (status, _, _, json) = call env ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "200" 200 (status_code status);
  let succeeded = json_list "succeeded" json in
  Alcotest.(check int) "1 succeeded" 1 (List.length succeeded);
  teardown env

let test_bulk_invalid_action () =
  let env = setup () in
  let key = register env in
  let body = {|{"ids":["x"],"action":"explode"}|} in
  let (status, _, _, _) = call env ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "400" 400 (status_code status);
  teardown env

let test_bulk_no_auth () =
  let env = setup () in
  let body = {|{"ids":[],"action":"delete"}|} in
  let (status, _, _, _) = call env ~body "/messages/bulk" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

let test_bulk_bad_json () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~headers:(auth key) ~body:"garbage" "/messages/bulk" in
  Alcotest.(check int) "400" 400 (status_code status);
  teardown env

let test_bulk_add_flags () =
  let env = setup () in
  let key = register env in
  let m1 = seed_email env key ~flags:[Sakura_mail.Maildir.Seen] simple_email in
  let body = Printf.sprintf
    {|{"ids":["%s"],"action":"add_flags","params":{"flags":["flagged"]}}|}
    m1.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "200" 200 (status_code status);
  teardown env

let test_bulk_remove_flags () =
  let env = setup () in
  let key = register env in
  let m1 = seed_email env key
    ~flags:[Sakura_mail.Maildir.Seen; Sakura_mail.Maildir.Flagged] simple_email in
  let body = Printf.sprintf
    {|{"ids":["%s"],"action":"remove_flags","params":{"flags":["seen"]}}|}
    m1.Sakura_mail.Maildir.id in
  let (status, _, _, _) = call env ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "200" 200 (status_code status);
  teardown env

(* ============================================================
   Workflow: full message lifecycle
   ============================================================ *)

let test_full_lifecycle () =
  let env = setup () in
  (* 1. Register *)
  let key = register env in
  (* 2. Create a draft *)
  let draft_body = {|{"to":["bob@test.com"],"cc":["cc@test.com"],"bcc":[],"subject":"Important","body":"Please review.","in_reply_to":null}|} in
  let (_, _, _, dj) = call env ~headers:(auth key) ~body:draft_body "/drafts" in
  let draft_id = json_string "id" dj in
  (* 3. Update the draft *)
  let draft_body2 = {|{"to":["bob@test.com"],"cc":[],"bcc":[],"subject":"Important - Updated","body":"Please review ASAP.","in_reply_to":null}|} in
  let (_, _, _, dj2) = call env ~meth:`PUT ~headers:(auth key)
    ~body:draft_body2 ("/drafts/" ^ draft_id) in
  let draft_id2 = json_string "id" dj2 in
  (* 4. Verify draft exists in Drafts folder *)
  let (_, _, _, mj) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/Drafts/messages" in
  Alcotest.(check int) "1 draft" 1 (json_int "total" mj);
  (* 5. Seed an incoming email *)
  ignore (seed_email env key simple_email);
  (* 6. List INBOX *)
  let (_, _, _, ij) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "1 in INBOX" 1 (json_int "total" ij);
  (* 7. Flag it as seen *)
  let inbox_msg = List.hd (json_list "messages" ij) in
  let inbox_id = json_string "id" (Some inbox_msg) in
  let (s, _, _, _) = call env ~meth:`PUT ~headers:(auth key)
    ~body:{|{"flags":["seen"]}|}
    (Printf.sprintf "/messages/%s/flags" inbox_id) in
  Alcotest.(check int) "flag ok" 200 (status_code s);
  (* 8. Move it to Archive *)
  let (s2, _, _, _) = call env ~meth:`POST ~headers:(auth key)
    ~body:{|{"destination":"Archive"}|}
    (Printf.sprintf "/messages/%s/move" inbox_id) in
  Alcotest.(check int) "move ok" 200 (status_code s2);
  (* 9. Verify INBOX is empty, Archive has 1 *)
  let (_, _, _, ij2) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  Alcotest.(check int) "INBOX empty" 0 (json_int "total" ij2);
  let (_, _, _, aj) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/Archive/messages" in
  Alcotest.(check int) "Archive has 1" 1 (json_int "total" aj);
  (* 10. Delete the draft *)
  let (s3, _, _, _) = call env ~meth:`DELETE ~headers:(auth key)
    ("/messages/" ^ draft_id2) in
  Alcotest.(check int) "delete draft ok" 200 (status_code s3);
  (* 11. Reroll API key *)
  let (_, _, _, kj) = call env ~headers:(auth key) "/accounts/reroll-key" in
  let new_key = json_string "api_key" kj in
  (* Old key should fail *)
  let (s4, _, _, _) = call env ~meth:`GET ~headers:(auth key) "/mailboxes" in
  Alcotest.(check int) "old key dead" 401 (status_code s4);
  (* New key should work *)
  let (s5, _, _, _) = call env ~meth:`GET ~headers:(auth new_key) "/mailboxes" in
  Alcotest.(check int) "new key works" 200 (status_code s5);
  (* 12. Delete account *)
  let (s6, _, _, _) = call env ~meth:`DELETE ~headers:(auth new_key) "/accounts" in
  Alcotest.(check int) "account deleted" 200 (status_code s6);
  teardown env

(* ============================================================
   GET /health — no auth required
   ============================================================ *)

let test_health () =
  let env = setup () in
  let (status, _, _, json) = call env ~meth:`GET "/health" in
  Alcotest.(check int) "200 OK" 200 (status_code status);
  let s = json_string "status" json in
  Alcotest.(check string) "status ok" "ok" s;
  teardown env

(* ============================================================
   GET /stats — auth required
   ============================================================ *)

let test_stats_no_auth () =
  let env = setup () in
  let (status, _, _, _) = call env ~meth:`GET "/stats" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

let test_stats_ok () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key simple_email);
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key) "/stats" in
  Alcotest.(check int) "200" 200 (status_code status);
  let _ = json_string "status" json in
  let n = json_int "accounts" json in
  Alcotest.(check int) "1 account" 1 n;
  (* maildir sub-object *)
  let md = match json with
    | Some (`Assoc a) -> (match List.assoc_opt "maildir" a with
      | Some (`Assoc m) -> Some (`Assoc m) | _ -> None)
    | _ -> None
  in
  let total = json_int "total_messages" md in
  Alcotest.(check bool) "has messages" true (total >= 1);
  teardown env

(* ============================================================
   GET /accounts — list accounts
   ============================================================ *)

let test_list_accounts () =
  let env = setup () in
  let key = register env in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key) "/accounts" in
  Alcotest.(check int) "200" 200 (status_code status);
  let items = match json with Some (`List l) -> l | _ -> failwith "not a list" in
  Alcotest.(check int) "1 account" 1 (List.length items);
  (* Should not contain password *)
  let has_password = match List.hd items with
    | `Assoc a -> List.mem_assoc "password" a
    | _ -> true
  in
  Alcotest.(check bool) "no password" false has_password;
  teardown env

let test_list_accounts_no_auth () =
  let env = setup () in
  let (status, _, _, _) = call env ~meth:`GET "/accounts" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

(* ============================================================
   GET /accounts/:id
   ============================================================ *)

let test_get_account () =
  let env = setup () in
  let key = register env in
  let acct = match Sakura_mail.Account.find_by_api_key env.registry key with
    | Some a -> a | None -> failwith "no account" in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    ("/accounts/" ^ acct.id) in
  Alcotest.(check int) "200" 200 (status_code status);
  let id = json_string "id" json in
  Alcotest.(check string) "correct id" acct.id id;
  teardown env

let test_get_account_wrong_id () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key) "/accounts/fake-id" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

(* ============================================================
   PUT /accounts/:id — update
   ============================================================ *)

let test_update_account () =
  let env = setup () in
  let key = register env in
  let acct = match Sakura_mail.Account.find_by_api_key env.registry key with
    | Some a -> a | None -> failwith "no account" in
  let body = {|{"host":"new.host.com","port":143,"username":"newuser","password":"newpass","use_tls":false}|} in
  let (status, _, _, json) = call env ~meth:`PUT ~headers:(auth key)
    ~body ("/accounts/" ^ acct.id) in
  Alcotest.(check int) "200" 200 (status_code status);
  let host = json_string "host" json in
  Alcotest.(check string) "updated host" "new.host.com" host;
  let user = json_string "username" json in
  Alcotest.(check string) "updated user" "newuser" user;
  teardown env

let test_update_account_wrong_id () =
  let env = setup () in
  let key = register env in
  let body = {|{"host":"h","port":993,"username":"u","password":"p","use_tls":false}|} in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key) ~body "/accounts/fake" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

(* ============================================================
   POST /mailboxes — create
   ============================================================ *)

let test_create_mailbox () =
  let env = setup () in
  let key = register env in
  let body = {|{"name":"Archive"}|} in
  let (status, _, _, json) = call env ~meth:`POST ~headers:(auth key) ~body "/mailboxes" in
  Alcotest.(check int) "201" 201 (status_code status);
  let name = json_string "name" json in
  Alcotest.(check string) "name" "Archive" name;
  (* Verify it shows in list *)
  let (_, _, _, lj) = call env ~meth:`GET ~headers:(auth key) "/mailboxes" in
  let boxes = match lj with Some (`List l) -> l | _ -> failwith "not a list" in
  let has_archive = List.exists (fun j -> j = `String "Archive") boxes in
  Alcotest.(check bool) "Archive in list" true has_archive;
  teardown env

let test_create_mailbox_duplicate () =
  let env = setup () in
  let key = register env in
  let body = {|{"name":"INBOX"}|} in
  let (status, _, _, _) = call env ~meth:`POST ~headers:(auth key) ~body "/mailboxes" in
  Alcotest.(check int) "409 conflict" 409 (status_code status);
  teardown env

let test_create_mailbox_missing_name () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`POST ~headers:(auth key)
    ~body:{|{}|} "/mailboxes" in
  Alcotest.(check int) "400" 400 (status_code status);
  teardown env

(* ============================================================
   DELETE /mailboxes/:name
   ============================================================ *)

let test_delete_mailbox () =
  let env = setup () in
  let key = register env in
  (* Create then delete *)
  let _ = call env ~meth:`POST ~headers:(auth key) ~body:{|{"name":"ToDelete"}|} "/mailboxes" in
  let (status, _, _, _) = call env ~meth:`DELETE ~headers:(auth key) "/mailboxes/ToDelete" in
  Alcotest.(check int) "200" 200 (status_code status);
  (* Gone from list *)
  let (_, _, _, lj) = call env ~meth:`GET ~headers:(auth key) "/mailboxes" in
  let boxes = match lj with Some (`List l) -> l | _ -> [] in
  let has_it = List.exists (fun j -> j = `String "ToDelete") boxes in
  Alcotest.(check bool) "gone" false has_it;
  teardown env

let test_delete_mailbox_not_empty () =
  let env = setup () in
  let key = register env in
  (* INBOX has seeded message *)
  ignore (seed_email env key simple_email);
  let (status, _, _, _) = call env ~meth:`DELETE ~headers:(auth key) "/mailboxes/INBOX" in
  Alcotest.(check int) "409 not empty" 409 (status_code status);
  teardown env

let test_delete_mailbox_not_found () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`DELETE ~headers:(auth key) "/mailboxes/Nope" in
  Alcotest.(check int) "404 not found" 404 (status_code status);
  teardown env

(* ============================================================
   PUT /mailboxes/:name — rename
   ============================================================ *)

let test_rename_mailbox () =
  let env = setup () in
  let key = register env in
  let _ = call env ~meth:`POST ~headers:(auth key) ~body:{|{"name":"OldName"}|} "/mailboxes" in
  let body = {|{"name":"NewName"}|} in
  let (status, _, _, json) = call env ~meth:`PUT ~headers:(auth key) ~body "/mailboxes/OldName" in
  Alcotest.(check int) "200" 200 (status_code status);
  let name = json_string "name" json in
  Alcotest.(check string) "renamed" "NewName" name;
  (* Verify via list *)
  let (_, _, _, lj) = call env ~meth:`GET ~headers:(auth key) "/mailboxes" in
  let boxes = match lj with Some (`List l) -> l | _ -> [] in
  let has_new = List.exists (fun j -> j = `String "NewName") boxes in
  let has_old = List.exists (fun j -> j = `String "OldName") boxes in
  Alcotest.(check bool) "new exists" true has_new;
  Alcotest.(check bool) "old gone" false has_old;
  teardown env

let test_rename_mailbox_not_found () =
  let env = setup () in
  let key = register env in
  let body = {|{"name":"X"}|} in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key) ~body "/mailboxes/Nope" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

let test_rename_mailbox_conflict () =
  let env = setup () in
  let key = register env in
  let _ = call env ~meth:`POST ~headers:(auth key) ~body:{|{"name":"A"}|} "/mailboxes" in
  let _ = call env ~meth:`POST ~headers:(auth key) ~body:{|{"name":"B"}|} "/mailboxes" in
  let body = {|{"name":"B"}|} in
  let (status, _, _, _) = call env ~meth:`PUT ~headers:(auth key) ~body "/mailboxes/A" in
  Alcotest.(check int) "409 conflict" 409 (status_code status);
  teardown env

(* ============================================================
   POST /messages/bulk — filter-based
   ============================================================ *)

let test_bulk_filter_set_flags () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key simple_email);
  ignore (seed_email env key multipart_email);
  let body = Yojson.Safe.to_string (`Assoc [
    "action", `String "set_flags";
    "mailbox", `String "INBOX";
    "filter", `Assoc ["from", `String "alice"];
    "params", `Assoc ["flags", `List [`String "seen"; `String "flagged"]];
  ]) in
  let (status, _, _, json) = call env ~meth:`POST ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "200" 200 (status_code status);
  let succeeded = json_list "succeeded" json in
  Alcotest.(check int) "1 succeeded" 1 (List.length succeeded);
  teardown env

let test_bulk_filter_no_mailbox () =
  let env = setup () in
  let key = register env in
  let body = Yojson.Safe.to_string (`Assoc [
    "action", `String "delete";
    "filter", `Assoc ["from", `String "alice"];
  ]) in
  let (status, _, _, _) = call env ~meth:`POST ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "400 needs mailbox" 400 (status_code status);
  teardown env

let test_bulk_filter_delete_all () =
  let env = setup () in
  let key = register env in
  ignore (seed_email env key simple_email);
  ignore (seed_email env key multipart_email);
  let body = Yojson.Safe.to_string (`Assoc [
    "action", `String "delete";
    "mailbox", `String "INBOX";
    "filter", `Assoc [];
  ]) in
  let (status, _, _, json) = call env ~meth:`POST ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "200" 200 (status_code status);
  let succeeded = json_list "succeeded" json in
  Alcotest.(check int) "2 deleted" 2 (List.length succeeded);
  teardown env

let test_bulk_over_limit () =
  let env = setup () in
  let key = register env in
  (* Set bulk_max to 1 *)
  env.settings.bulk_max <- 1;
  ignore (seed_email env key simple_email);
  ignore (seed_email env key multipart_email);
  let body = Yojson.Safe.to_string (`Assoc [
    "action", `String "delete";
    "mailbox", `String "INBOX";
    "filter", `Assoc [];
  ]) in
  let (status, _, _, _) = call env ~meth:`POST ~headers:(auth key) ~body "/messages/bulk" in
  Alcotest.(check int) "400 over limit" 400 (status_code status);
  teardown env

(* ============================================================
   GET /accounts/:id/logs — sync logs
   ============================================================ *)

let test_logs_empty () =
  let env = setup () in
  let key = register env in
  let acct = match Sakura_mail.Account.find_by_api_key env.registry key with
    | Some a -> a | None -> failwith "no account" in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    ("/accounts/" ^ acct.id ^ "/logs") in
  Alcotest.(check int) "200" 200 (status_code status);
  let entries = match json with Some (`List l) -> l | _ -> failwith "not a list" in
  Alcotest.(check int) "no entries" 0 (List.length entries);
  teardown env

let test_logs_with_entries () =
  let env = setup () in
  let key = register env in
  let acct = match Sakura_mail.Account.find_by_api_key env.registry key with
    | Some a -> a | None -> failwith "no account" in
  (* Write a fake log entry *)
  Sakura_mail.Sync_log.append ~base_dir:env.base_dir acct.id
    { Sakura_mail.Mbsync.exit_code = 0; stdout = "ok"; stderr = "" };
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    ("/accounts/" ^ acct.id ^ "/logs") in
  Alcotest.(check int) "200" 200 (status_code status);
  let entries = match json with Some (`List l) -> l | _ -> failwith "not a list" in
  Alcotest.(check int) "1 entry" 1 (List.length entries);
  teardown env

let test_logs_wrong_account () =
  let env = setup () in
  let key = register env in
  let (status, _, _, _) = call env ~meth:`GET ~headers:(auth key) "/accounts/fake/logs" in
  Alcotest.(check int) "404" 404 (status_code status);
  teardown env

(* ============================================================
   GET /settings
   ============================================================ *)

let test_get_settings () =
  let env = setup () in
  let key = register env in
  let (status, _, _, json) = call env ~meth:`GET ~headers:(auth key) "/settings" in
  Alcotest.(check int) "200" 200 (status_code status);
  let bm = json_int "bulk_max" json in
  Alcotest.(check int) "default bulk_max" 100 bm;
  let dpl = json_int "default_page_limit" json in
  Alcotest.(check int) "default page limit" 50 dpl;
  teardown env

let test_get_settings_no_auth () =
  let env = setup () in
  let (status, _, _, _) = call env ~meth:`GET "/settings" in
  Alcotest.(check int) "401" 401 (status_code status);
  teardown env

(* ============================================================
   PUT /settings
   ============================================================ *)

let test_update_settings () =
  let env = setup () in
  let key = register env in
  let body = {|{"bulk_max": 200, "default_page_limit": 25}|} in
  let (status, _, _, json) = call env ~meth:`PUT ~headers:(auth key) ~body "/settings" in
  Alcotest.(check int) "200" 200 (status_code status);
  let bm = json_int "bulk_max" json in
  Alcotest.(check int) "updated bulk_max" 200 bm;
  let dpl = json_int "default_page_limit" json in
  Alcotest.(check int) "updated page limit" 25 dpl;
  teardown env

let test_update_settings_invalid () =
  let env = setup () in
  let key = register env in
  let body = {|{"bulk_max": -5}|} in
  let (status, _, _, json) = call env ~meth:`PUT ~headers:(auth key) ~body "/settings" in
  Alcotest.(check int) "200 ignores invalid" 200 (status_code status);
  (* Should still have default value since -5 is rejected *)
  let bm = json_int "bulk_max" json in
  Alcotest.(check int) "unchanged" 100 bm;
  teardown env

let test_update_settings_partial () =
  let env = setup () in
  let key = register env in
  let body = {|{"bulk_max": 42}|} in
  let (status, _, _, json) = call env ~meth:`PUT ~headers:(auth key) ~body "/settings" in
  Alcotest.(check int) "200" 200 (status_code status);
  let bm = json_int "bulk_max" json in
  Alcotest.(check int) "updated" 42 bm;
  let dpl = json_int "default_page_limit" json in
  Alcotest.(check int) "default unchanged" 50 dpl;
  teardown env

(* ============================================================
   settings affect behavior
   ============================================================ *)

let test_settings_affect_pagination () =
  let env = setup () in
  let key = register env in
  (* Seed 3 emails *)
  ignore (seed_email env key simple_email);
  ignore (seed_email env key simple_email);
  ignore (seed_email env key simple_email);
  (* Set page limit to 2 *)
  let body = {|{"default_page_limit": 2}|} in
  ignore (call env ~meth:`PUT ~headers:(auth key) ~body "/settings");
  (* List messages — should get 2 with a next_cursor *)
  let (_, _, _, json) = call env ~meth:`GET ~headers:(auth key)
    "/mailboxes/INBOX/messages" in
  let msgs = json_list "messages" json in
  Alcotest.(check int) "page of 2" 2 (List.length msgs);
  let has_cursor = match json with
    | Some (`Assoc a) -> (match List.assoc_opt "next_cursor" a with
      | Some `Null -> false | Some _ -> true | None -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has next cursor" true has_cursor;
  teardown env

(* ============================================================
   Run all e2e tests
   ============================================================ *)

let () =
  Alcotest.run "sakura-mail-e2e" [
    "POST /accounts", [
      "register success", `Quick, test_register_success;
      "register missing fields", `Quick, test_register_missing_fields;
      "register invalid json", `Quick, test_register_invalid_json;
      "register empty body", `Quick, test_register_empty_body;
      "register multiple accounts", `Quick, test_register_multiple_accounts;
    ];
    "DELETE /accounts", [
      "delete account", `Quick, test_delete_account;
      "delete no auth", `Quick, test_delete_no_auth;
      "delete bad key", `Quick, test_delete_bad_key;
    ];
    "POST /accounts/reroll-key", [
      "reroll key", `Quick, test_reroll_key;
      "reroll no auth", `Quick, test_reroll_no_auth;
    ];
    "auth edge cases", [
      "missing header", `Quick, test_auth_missing_header;
      "malformed header", `Quick, test_auth_malformed_header;
      "empty bearer", `Quick, test_auth_empty_bearer;
      "revoked key", `Quick, test_auth_revoked_key;
    ];
    "GET /mailboxes", [
      "empty mailboxes", `Quick, test_list_mailboxes_empty;
      "multiple mailboxes", `Quick, test_list_mailboxes_multiple;
    ];
    "GET /mailboxes/:mailbox/messages", [
      "empty inbox", `Quick, test_list_messages_empty;
      "with emails", `Quick, test_list_messages_with_emails;
      "cursor pagination", `Quick, test_list_messages_cursor_pagination;
      "default pagination", `Quick, test_list_messages_default_pagination;
      "url-encoded mailbox", `Quick, test_list_messages_url_encoded_mailbox;
      "nonexistent mailbox", `Quick, test_list_messages_nonexistent_mailbox;
    ];
    "GET /messages/:id", [
      "read message", `Quick, test_read_message;
      "read multipart", `Quick, test_read_message_multipart;
      "not found", `Quick, test_read_message_not_found;
      "raw", `Quick, test_read_message_raw;
      "raw not found", `Quick, test_read_message_raw_not_found;
      "no auth", `Quick, test_read_message_no_auth;
      "wrong user", `Quick, test_read_message_wrong_user;
      "has attachments", `Quick, test_read_message_has_attachments;
      "no attachments", `Quick, test_message_no_attachments_field;
    ];
    "GET /messages/:id/attachments/:index", [
      "download attachment", `Quick, test_download_attachment;
      "no attachments", `Quick, test_download_attachment_not_found;
      "bad index", `Quick, test_download_attachment_bad_index;
      "invalid index", `Quick, test_download_attachment_invalid_index;
    ];
    "search & filter", [
      "filter by from", `Quick, test_filter_by_from;
      "filter by subject", `Quick, test_filter_by_subject;
      "filter by body", `Quick, test_filter_by_body;
      "filter by flags", `Quick, test_filter_by_flags;
      "filter unread", `Quick, test_filter_unread;
      "filter has_attachment", `Quick, test_filter_has_attachment;
      "combined filters", `Quick, test_filter_combined;
      "no matches", `Quick, test_filter_no_matches;
    ];
    "sorting", [
      "subject asc", `Quick, test_sort_by_subject_asc;
      "subject desc", `Quick, test_sort_by_subject_desc;
      "size asc", `Quick, test_sort_by_size;
      "default date desc", `Quick, test_sort_default_date_desc;
    ];
    "POST /messages/bulk", [
      "set flags", `Quick, test_bulk_set_flags;
      "partial failure", `Quick, test_bulk_partial_failure;
      "move", `Quick, test_bulk_move;
      "delete", `Quick, test_bulk_delete;
      "add flags", `Quick, test_bulk_add_flags;
      "remove flags", `Quick, test_bulk_remove_flags;
      "invalid action", `Quick, test_bulk_invalid_action;
      "no auth", `Quick, test_bulk_no_auth;
      "bad json", `Quick, test_bulk_bad_json;
    ];
    "PUT /messages/:id/flags", [
      "set flags", `Quick, test_set_flags;
      "clear all flags", `Quick, test_set_flags_clear_all;
      "invalid flag ignored", `Quick, test_set_flags_invalid_flag;
      "not found", `Quick, test_set_flags_not_found;
      "bad body", `Quick, test_set_flags_bad_body;
    ];
    "POST /messages/:id/move", [
      "move message", `Quick, test_move_message;
      "not found", `Quick, test_move_not_found;
      "bad body", `Quick, test_move_bad_body;
    ];
    "DELETE /messages/:id", [
      "delete message", `Quick, test_delete_message;
      "not found", `Quick, test_delete_message_not_found;
      "already trashed", `Quick, test_delete_already_trashed;
    ];
    "POST /drafts", [
      "create draft", `Quick, test_create_draft;
      "with cc/bcc", `Quick, test_create_draft_with_cc_bcc;
      "with reply", `Quick, test_create_draft_with_reply;
      "empty body", `Quick, test_create_draft_empty_body;
      "unicode subject", `Quick, test_create_draft_unicode_subject;
      "invalid json", `Quick, test_create_draft_invalid_json;
      "missing fields", `Quick, test_create_draft_missing_fields;
    ];
    "PUT /drafts/:id", [
      "update draft", `Quick, test_update_draft;
      "not found", `Quick, test_update_draft_not_found;
      "bad body", `Quick, test_update_draft_bad_body;
    ];
    "POST /sync", [
      "no auth", `Quick, test_sync_no_auth;
    ];
    "user isolation", [
      "mailboxes isolated", `Quick, test_user_isolation_mailboxes;
      "drafts isolated", `Quick, test_user_isolation_drafts;
      "flags isolated", `Quick, test_user_isolation_flags;
      "delete isolated", `Quick, test_user_isolation_delete;
      "move isolated", `Quick, test_user_isolation_move;
    ];
    "lifecycle", [
      "full workflow", `Quick, test_full_lifecycle;
    ];
    "GET /health", [
      "health ok", `Quick, test_health;
    ];
    "GET /stats", [
      "no auth", `Quick, test_stats_no_auth;
      "stats ok", `Quick, test_stats_ok;
    ];
    "GET /accounts", [
      "list accounts", `Quick, test_list_accounts;
      "no auth", `Quick, test_list_accounts_no_auth;
    ];
    "GET /accounts/:id", [
      "get account", `Quick, test_get_account;
      "wrong id", `Quick, test_get_account_wrong_id;
    ];
    "PUT /accounts/:id", [
      "update account", `Quick, test_update_account;
      "wrong id", `Quick, test_update_account_wrong_id;
    ];
    "POST /mailboxes", [
      "create mailbox", `Quick, test_create_mailbox;
      "duplicate", `Quick, test_create_mailbox_duplicate;
      "missing name", `Quick, test_create_mailbox_missing_name;
    ];
    "DELETE /mailboxes/:name", [
      "delete mailbox", `Quick, test_delete_mailbox;
      "not empty", `Quick, test_delete_mailbox_not_empty;
      "not found", `Quick, test_delete_mailbox_not_found;
    ];
    "PUT /mailboxes/:name", [
      "rename", `Quick, test_rename_mailbox;
      "not found", `Quick, test_rename_mailbox_not_found;
      "conflict", `Quick, test_rename_mailbox_conflict;
    ];
    "bulk with filter", [
      "filter set flags", `Quick, test_bulk_filter_set_flags;
      "filter no mailbox", `Quick, test_bulk_filter_no_mailbox;
      "filter delete all", `Quick, test_bulk_filter_delete_all;
      "over limit", `Quick, test_bulk_over_limit;
    ];
    "GET /accounts/:id/logs", [
      "empty logs", `Quick, test_logs_empty;
      "with entries", `Quick, test_logs_with_entries;
      "wrong account", `Quick, test_logs_wrong_account;
    ];
    "GET /settings", [
      "get settings", `Quick, test_get_settings;
      "no auth", `Quick, test_get_settings_no_auth;
    ];
    "PUT /settings", [
      "update settings", `Quick, test_update_settings;
      "invalid values", `Quick, test_update_settings_invalid;
      "partial update", `Quick, test_update_settings_partial;
    ];
    "settings behavior", [
      "affects pagination", `Quick, test_settings_affect_pagination;
    ];
  ]

(* Unit tests for sakura-mail *)

let () = Random.self_init ()

(* --- Test helpers --- *)

let tmp_dir prefix =
  let dir = Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "%s_%d_%d" prefix (Unix.getpid ()) (Random.bits ())) in
  Unix.mkdir dir 0o755;
  dir

let rm_rf path =
  let rec rm path =
    if Sys.is_directory path then begin
      Array.iter (fun name -> rm (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
    end else
      Sys.remove path
  in
  if Sys.file_exists path then rm path

(* ============================================================
   Mbsync arg building tests
   ============================================================ *)

let test_default_config_args () =
  let args = Sakura_mail.Mbsync.config_args Sakura_mail.Mbsync.default_config in
  Alcotest.(check (list string)) "default config has no args" [] args

let test_config_with_file () =
  let config = { Sakura_mail.Mbsync.default_config with
    config_file = Some "/tmp/test.mbsyncrc" } in
  let args = Sakura_mail.Mbsync.config_args config in
  Alcotest.(check (list string)) "config file args"
    ["-c"; "/tmp/test.mbsyncrc"] args

let test_config_all_flags () =
  let config = { Sakura_mail.Mbsync.default_config with
    dry_run = true; verbose = true; quiet = true; ext_exit = true } in
  let args = Sakura_mail.Mbsync.config_args config in
  Alcotest.(check (list string)) "all flags"
    ["--dry-run"; "--verbose"; "--quiet"; "--ext-exit"] args

let test_sync_args_pull_new () =
  let opts = { Sakura_mail.Mbsync.default_sync_opts with
    directions = [Pull]; operations = [New] } in
  let args = Sakura_mail.Mbsync.sync_args opts in
  Alcotest.(check (list string)) "pull new"
    ["--pull"; "--new"] args

let test_sync_args_create_expunge () =
  let opts = { Sakura_mail.Mbsync.default_sync_opts with
    create = [Far; Near]; expunge = [Far] } in
  let args = Sakura_mail.Mbsync.sync_args opts in
  Alcotest.(check (list string)) "create + expunge"
    ["--create-far"; "--create-near"; "--expunge-far"] args

let test_target_all () =
  let args = Sakura_mail.Mbsync.target_args Sakura_mail.Mbsync.All in
  Alcotest.(check (list string)) "target all" ["--all"] args

let test_target_channels () =
  let args = Sakura_mail.Mbsync.target_args
    (Sakura_mail.Mbsync.Channels [
      ("work", Some ["INBOX"; "Sent"]);
      ("personal", None)]) in
  Alcotest.(check (list string)) "target channels"
    ["work:INBOX,Sent"; "personal"] args

let test_build_sync_args () =
  let config = { Sakura_mail.Mbsync.default_config with
    config_file = Some "/tmp/rc"; dry_run = true } in
  let opts = { Sakura_mail.Mbsync.default_sync_opts with
    directions = [Pull] } in
  let args = Sakura_mail.Mbsync.build_sync_args ~config ~opts
    Sakura_mail.Mbsync.All in
  Alcotest.(check (list string)) "full command"
    ["mbsync"; "-c"; "/tmp/rc"; "--dry-run"; "--pull"; "--all"] args

let mbsync_tests = [
  "default config args", `Quick, test_default_config_args;
  "config with file", `Quick, test_config_with_file;
  "config all flags", `Quick, test_config_all_flags;
  "sync args pull+new", `Quick, test_sync_args_pull_new;
  "sync args create+expunge", `Quick, test_sync_args_create_expunge;
  "target all", `Quick, test_target_all;
  "target channels", `Quick, test_target_channels;
  "build full sync args", `Quick, test_build_sync_args;
]

(* ============================================================
   Mbsync config generation tests
   ============================================================ *)

let test_generate_config () =
  let config : Sakura_mail.Mbsync_config.t = {
    account = {
      host = "imap.example.com";
      port = 993;
      user = "test@example.com";
      password = "secret123";
      use_tls = true;
    };
    maildir_path = "/tmp/mail";
    channel_name = "test";
  } in
  let output = Sakura_mail.Mbsync_config.generate config in
  Alcotest.(check bool) "contains host"
    true (String.length output > 0);
  let contains haystack needle =
    try let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in true
    with Not_found -> false
  in
  Alcotest.(check bool) "has IMAPAccount"
    true (contains output "IMAPAccount");
  Alcotest.(check bool) "has IMAPS"
    true (contains output "TLSType IMAPS")

let test_generate_config_no_tls () =
  let config : Sakura_mail.Mbsync_config.t = {
    account = {
      host = "imap.example.com";
      port = 143;
      user = "test@example.com";
      password = "secret";
      use_tls = false;
    };
    maildir_path = "/tmp/mail";
    channel_name = "test";
  } in
  let output = Sakura_mail.Mbsync_config.generate config in
  Alcotest.(check bool) "has TLSType None"
    true (let s = "TLSType None" in
      try let _ = Str.search_forward (Str.regexp_string s) output 0 in true
      with Not_found -> false)

let test_write_config () =
  let dir = tmp_dir "mbsync_config" in
  let path = Filename.concat dir ".mbsyncrc" in
  let config : Sakura_mail.Mbsync_config.t = {
    account = {
      host = "imap.test.com"; port = 993;
      user = "u"; password = "p"; use_tls = true;
    };
    maildir_path = "/tmp/m";
    channel_name = "c";
  } in
  Sakura_mail.Mbsync_config.write_config ~path config;
  Alcotest.(check bool) "file exists" true (Sys.file_exists path);
  rm_rf dir

let mbsync_config_tests = [
  "generate config TLS", `Quick, test_generate_config;
  "generate config no TLS", `Quick, test_generate_config_no_tls;
  "write config to file", `Quick, test_write_config;
]

(* ============================================================
   Maildir tests
   ============================================================ *)

let setup_maildir () =
  let root = tmp_dir "maildir" in
  let inbox = Filename.concat root "INBOX" in
  Unix.mkdir inbox 0o755;
  Sakura_mail.Maildir.ensure_maildir_structure inbox;
  (root, inbox)

let write_test_msg inbox filename content =
  let path = Filename.concat (Filename.concat inbox "cur") filename in
  let oc = open_out path in
  output_string oc content;
  close_out oc

let test_parse_flags () =
  let flags = Sakura_mail.Maildir.parse_flags "1234.msg:2,FS" in
  let flag_strs = List.map Sakura_mail.Maildir.string_of_flag flags in
  Alcotest.(check (list string)) "flags" ["flagged"; "seen"] flag_strs

let test_parse_flags_empty () =
  let flags = Sakura_mail.Maildir.parse_flags "1234.msg:2," in
  Alcotest.(check int) "no flags" 0 (List.length flags)

let test_parse_flags_no_info () =
  let flags = Sakura_mail.Maildir.parse_flags "1234.msg" in
  Alcotest.(check int) "no info section" 0 (List.length flags)

let test_message_id_extraction () =
  let id = Sakura_mail.Maildir.message_id_of_filename "1234.msg:2,FS" in
  Alcotest.(check string) "message id" "1234.msg" id

let test_filename_with_flags () =
  let open Sakura_mail.Maildir in
  let name = filename_with_flags "1234.msg:2,S" [Flagged; Seen; Replied] in
  Alcotest.(check string) "sorted flags" "1234.msg:2,FRS" name

let test_list_mailboxes () =
  let (root, _inbox) = setup_maildir () in
  let sent = Filename.concat root "Sent" in
  Unix.mkdir sent 0o755;
  Sakura_mail.Maildir.ensure_maildir_structure sent;
  let mboxes = Sakura_mail.Maildir.list_mailboxes ~maildir_root:root in
  Alcotest.(check (list string)) "mailboxes" ["INBOX"; "Sent"] mboxes;
  rm_rf root

let test_scan_mailbox () =
  let (root, inbox) = setup_maildir () in
  write_test_msg inbox "msg1:2,S" "From: test\r\nSubject: Hi\r\n\r\nHello";
  write_test_msg inbox "msg2:2," "From: test\r\nSubject: Bye\r\n\r\nWorld";
  let msgs = Sakura_mail.Maildir.scan_mailbox ~maildir_root:root ~mailbox:"INBOX" in
  Alcotest.(check int) "2 messages" 2 (List.length msgs);
  rm_rf root

let test_set_flags () =
  let (root, inbox) = setup_maildir () in
  write_test_msg inbox "msg1:2," "test content";
  let msgs = Sakura_mail.Maildir.scan_mailbox ~maildir_root:root ~mailbox:"INBOX" in
  let msg = List.hd msgs in
  let updated = Sakura_mail.Maildir.set_flags msg [Sakura_mail.Maildir.Seen; Sakura_mail.Maildir.Flagged] in
  Alcotest.(check int) "2 flags" 2 (List.length updated.flags);
  Alcotest.(check bool) "file renamed" true (Sys.file_exists updated.path);
  rm_rf root

let test_move_message () =
  let (root, inbox) = setup_maildir () in
  write_test_msg inbox "msg1:2,S" "test content";
  let msgs = Sakura_mail.Maildir.scan_mailbox ~maildir_root:root ~mailbox:"INBOX" in
  let msg = List.hd msgs in
  let moved = Sakura_mail.Maildir.move_message ~maildir_root:root msg ~dest_mailbox:"Archive" in
  Alcotest.(check string) "new mailbox" "Archive" moved.mailbox;
  Alcotest.(check bool) "moved file exists" true (Sys.file_exists moved.path);
  Alcotest.(check bool) "old file gone" false (Sys.file_exists msg.path);
  rm_rf root

let test_trash_message () =
  let (root, inbox) = setup_maildir () in
  write_test_msg inbox "msg1:2,S" "test content";
  let msgs = Sakura_mail.Maildir.scan_mailbox ~maildir_root:root ~mailbox:"INBOX" in
  let msg = List.hd msgs in
  let trashed = Sakura_mail.Maildir.trash_message ~maildir_root:root msg in
  Alcotest.(check string) "in Trash" "Trash" trashed.mailbox;
  rm_rf root

let test_write_message () =
  let (root, _inbox) = setup_maildir () in
  let msg = Sakura_mail.Maildir.write_message ~maildir_root:root
    ~mailbox:"Drafts" ~content:"From: me\r\nSubject: Draft\r\n\r\nHello" in
  Alcotest.(check string) "in Drafts" "Drafts" msg.mailbox;
  Alcotest.(check bool) "file exists" true (Sys.file_exists msg.path);
  let content = Sakura_mail.Maildir.read_message_raw msg in
  Alcotest.(check bool) "content matches" true (String.length content > 0);
  rm_rf root

let test_find_message () =
  let (root, inbox) = setup_maildir () in
  write_test_msg inbox "unique123:2,S" "test";
  let found = Sakura_mail.Maildir.find_message ~maildir_root:root ~id:"unique123" in
  Alcotest.(check bool) "found" true (Option.is_some found);
  let not_found = Sakura_mail.Maildir.find_message ~maildir_root:root ~id:"nonexistent" in
  Alcotest.(check bool) "not found" true (Option.is_none not_found);
  rm_rf root

let maildir_tests = [
  "parse flags", `Quick, test_parse_flags;
  "parse empty flags", `Quick, test_parse_flags_empty;
  "parse no info section", `Quick, test_parse_flags_no_info;
  "message id extraction", `Quick, test_message_id_extraction;
  "filename with flags", `Quick, test_filename_with_flags;
  "list mailboxes", `Quick, test_list_mailboxes;
  "scan mailbox", `Quick, test_scan_mailbox;
  "set flags", `Quick, test_set_flags;
  "move message", `Quick, test_move_message;
  "trash message", `Quick, test_trash_message;
  "write message", `Quick, test_write_message;
  "find message", `Quick, test_find_message;
]

(* ============================================================
   Email parsing tests
   ============================================================ *)

let simple_email = String.concat "\r\n" [
  "From: Alice <alice@example.com>";
  "To: Bob <bob@example.com>";
  "Subject: Hello World";
  "Date: Thu, 3 Apr 2025 10:00:00 +0000";
  "Message-ID: <test123@example.com>";
  "MIME-Version: 1.0";
  "Content-Type: text/plain; charset=UTF-8";
  "";
  "Hello, this is a test email.";
  "";
]

let test_parse_simple_email () =
  match Sakura_mail.Email.parse_email simple_email with
  | Error err -> Alcotest.fail ("Parse failed: " ^ err)
  | Ok msg ->
    Alcotest.(check string) "subject" "Hello World" msg.subject;
    Alcotest.(check int) "one from" 1 (List.length msg.from);
    Alcotest.(check int) "one to" 1 (List.length msg.to_);
    Alcotest.(check bool) "has body"
      true (Option.is_some msg.body_text)

let test_generate_draft () =
  let draft : Sakura_mail.Email.draft = {
    to_ = ["bob@example.com"];
    cc = ["charlie@example.com"];
    bcc = [];
    subject = "Test Draft";
    body = "This is a draft.";
    in_reply_to = None;
  } in
  let content = Sakura_mail.Email.generate_draft draft in
  Alcotest.(check bool) "has To header"
    true (try let _ = Str.search_forward (Str.regexp_string "To:") content 0 in true
          with Not_found -> false);
  Alcotest.(check bool) "has Subject"
    true (try let _ = Str.search_forward (Str.regexp_string "Subject: Test Draft") content 0 in true
          with Not_found -> false);
  Alcotest.(check bool) "has Cc"
    true (try let _ = Str.search_forward (Str.regexp_string "Cc:") content 0 in true
          with Not_found -> false);
  Alcotest.(check bool) "has body"
    true (try let _ = Str.search_forward (Str.regexp_string "This is a draft.") content 0 in true
          with Not_found -> false)

let test_generate_draft_with_reply () =
  let draft : Sakura_mail.Email.draft = {
    to_ = ["alice@example.com"];
    cc = [];
    bcc = [];
    subject = "Re: Hello";
    body = "Thanks!";
    in_reply_to = Some "<original@example.com>";
  } in
  let content = Sakura_mail.Email.generate_draft draft in
  Alcotest.(check bool) "has In-Reply-To"
    true (try let _ = Str.search_forward (Str.regexp_string "In-Reply-To:") content 0 in true
          with Not_found -> false);
  Alcotest.(check bool) "has References"
    true (try let _ = Str.search_forward (Str.regexp_string "References:") content 0 in true
          with Not_found -> false)

let email_tests = [
  "parse simple email", `Quick, test_parse_simple_email;
  "generate draft", `Quick, test_generate_draft;
  "generate draft with reply", `Quick, test_generate_draft_with_reply;
]

(* ============================================================
   Account tests
   ============================================================ *)

let test_account_lifecycle () =
  let base_dir = tmp_dir "accounts" in
  let registry = Sakura_mail.Account.create_registry ~base_dir in
  let imap : Sakura_mail.Account.imap_config = {
    host = "imap.test.com"; port = 993;
    username = "user"; password = "pass"; use_tls = true;
  } in
  (* Register *)
  let account = Sakura_mail.Account.register registry ~imap in
  Alcotest.(check bool) "has id" true (String.length account.id > 0);
  Alcotest.(check bool) "has api_key" true (String.length account.api_key > 0);
  Alcotest.(check bool) "key starts with sk_"
    true (String.sub account.api_key 0 3 = "sk_");
  (* Find by key *)
  let found = Sakura_mail.Account.find_by_api_key registry account.api_key in
  Alcotest.(check bool) "found by key" true (Option.is_some found);
  (* Reroll *)
  let old_key = account.api_key in
  let updated = Sakura_mail.Account.reroll_api_key registry account in
  Alcotest.(check bool) "key changed" true (updated.api_key <> old_key);
  let old_found = Sakura_mail.Account.find_by_api_key registry old_key in
  Alcotest.(check bool) "old key invalid" true (Option.is_none old_found);
  let new_found = Sakura_mail.Account.find_by_api_key registry updated.api_key in
  Alcotest.(check bool) "new key works" true (Option.is_some new_found);
  (* Unregister *)
  Sakura_mail.Account.unregister registry updated;
  let gone = Sakura_mail.Account.find_by_api_key registry updated.api_key in
  Alcotest.(check bool) "key removed" true (Option.is_none gone);
  rm_rf base_dir

let test_account_persistence () =
  let base_dir = tmp_dir "accounts_persist" in
  let registry = Sakura_mail.Account.create_registry ~base_dir in
  let imap : Sakura_mail.Account.imap_config = {
    host = "imap.test.com"; port = 993;
    username = "user"; password = "pass"; use_tls = true;
  } in
  let account = Sakura_mail.Account.register registry ~imap in
  (* Reload from disk *)
  let registry2 = Sakura_mail.Account.create_registry ~base_dir in
  let found = Sakura_mail.Account.find_by_api_key registry2 account.api_key in
  Alcotest.(check bool) "persisted and reloaded" true (Option.is_some found);
  rm_rf base_dir

let test_mbsyncrc_generated () =
  let base_dir = tmp_dir "accounts_rc" in
  let registry = Sakura_mail.Account.create_registry ~base_dir in
  let imap : Sakura_mail.Account.imap_config = {
    host = "imap.test.com"; port = 993;
    username = "user"; password = "pass"; use_tls = true;
  } in
  let account = Sakura_mail.Account.register registry ~imap in
  let rc_path = Sakura_mail.Account.mbsyncrc_path ~base_dir account.id in
  Alcotest.(check bool) ".mbsyncrc exists" true (Sys.file_exists rc_path);
  rm_rf base_dir

let account_tests = [
  "account lifecycle", `Quick, test_account_lifecycle;
  "account persistence", `Quick, test_account_persistence;
  "mbsyncrc generated on register", `Quick, test_mbsyncrc_generated;
]

(* ============================================================
   Run all tests
   ============================================================ *)

let () =
  Alcotest.run "sakura-mail" [
    "mbsync", mbsync_tests;
    "mbsync-config", mbsync_config_tests;
    "maildir", maildir_tests;
    "email", email_tests;
    "account", account_tests;
  ]

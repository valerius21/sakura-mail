(** Account management — user registration, API keys, credential storage. *)

type imap_config = {
  host : string;
  port : int;
  username : string;
  password : string;
  use_tls : bool;
} [@@deriving yojson]

type t = {
  id : string;
  api_key : string;
  imap : imap_config;
  created_at : float;
} [@@deriving yojson]

let generate_id () =
  Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ())

let generate_api_key () =
  let prefix = "sk_" in
  let random_part = Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ()) in
  prefix ^ random_part

let account_dir ~base_dir account_id =
  Filename.concat base_dir account_id

let account_file ~base_dir account_id =
  Filename.concat (account_dir ~base_dir account_id) "account.json"

let maildir_path ~base_dir account_id =
  Filename.concat (account_dir ~base_dir account_id) "Maildir"

let mbsyncrc_path ~base_dir account_id =
  Filename.concat (account_dir ~base_dir account_id) ".mbsyncrc"

let ensure_dir path =
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o700

let save ~base_dir (account : t) =
  let dir = account_dir ~base_dir account.id in
  ensure_dir dir;
  let json = to_yojson account in
  let content = Yojson.Safe.pretty_to_string json in
  let path = account_file ~base_dir account.id in
  let oc = open_out path in
  output_string oc content;
  close_out oc

let load ~base_dir account_id =
  let path = account_file ~base_dir account_id in
  if not (Sys.file_exists path) then
    None
  else
    let ic = open_in path in
    let n = in_channel_length ic in
    let content = really_input_string ic n in
    close_in ic;
    match Yojson.Safe.from_string content |> of_yojson with
    | Ok account -> Some account
    | Error _ -> None

let delete ~base_dir account_id =
  let dir = account_dir ~base_dir account_id in
  if Sys.file_exists dir then begin
    let rec rm path =
      if Sys.is_directory path then begin
        Array.iter (fun name ->
          rm (Filename.concat path name)
        ) (Sys.readdir path);
        Unix.rmdir path
      end else
        Sys.remove path
    in
    rm dir
  end

type registry = {
  base_dir : string;
  mutable accounts : t list;
  mutable by_api_key : (string, t) Hashtbl.t;
}

let create_registry ~base_dir =
  ensure_dir base_dir;
  let accounts = ref [] in
  let by_api_key = Hashtbl.create 16 in
  if Sys.file_exists base_dir && Sys.is_directory base_dir then begin
    Array.iter (fun name ->
      let dir = Filename.concat base_dir name in
      if Sys.is_directory dir then
        match load ~base_dir name with
        | Some account ->
          accounts := account :: !accounts;
          Hashtbl.replace by_api_key account.api_key account
        | None -> ()
    ) (Sys.readdir base_dir)
  end;
  { base_dir; accounts = !accounts; by_api_key }

let find_by_api_key registry key =
  Hashtbl.find_opt registry.by_api_key key

let imap_configs_equal a b =
  a.host = b.host
  && a.port = b.port
  && a.username = b.username

exception Duplicate_account of string

let register registry ~imap =
  let existing =
    List.find_opt (fun a ->
      imap_configs_equal a.imap imap
    ) registry.accounts
  in
  match existing with
  | Some _ -> raise (Duplicate_account imap.username)
  | None ->
  let id = generate_id () in
  let api_key = generate_api_key () in
  let account = { id; api_key; imap; created_at = Unix.gettimeofday () } in
  save ~base_dir:registry.base_dir account;
  let mbsync_conf : Mbsync_config.t = {
    account = {
      host = imap.host;
      port = imap.port;
      user = imap.username;
      password = imap.password;
      use_tls = imap.use_tls;
    };
    maildir_path = maildir_path ~base_dir:registry.base_dir id;
    channel_name = "sakura";
  } in
  Mbsync_config.write_config
    ~path:(mbsyncrc_path ~base_dir:registry.base_dir id)
    mbsync_conf;
  let mdir = maildir_path ~base_dir:registry.base_dir id in
  ensure_dir mdir;
  Maildir.ensure_maildir_structure (Filename.concat mdir "INBOX");
  registry.accounts <- account :: registry.accounts;
  Hashtbl.replace registry.by_api_key account.api_key account;
  account

let reroll_api_key registry (account : t) =
  Hashtbl.remove registry.by_api_key account.api_key;
  let new_key = generate_api_key () in
  let updated = { account with api_key = new_key } in
  save ~base_dir:registry.base_dir updated;
  registry.accounts <- List.map (fun a ->
    if a.id = account.id then updated else a
  ) registry.accounts;
  Hashtbl.replace registry.by_api_key new_key updated;
  updated

let find_by_id registry id =
  List.find_opt (fun a -> a.id = id) registry.accounts

let accounts_for_key registry key =
  match find_by_api_key registry key with
  | None -> []
  | Some account ->
    List.filter (fun a -> a.api_key = account.api_key || a.id = account.id)
      registry.accounts

let list_for_account (_registry : registry) (account : t) =
  [account]

let update registry (account : t) ~imap =
  let updated = { account with imap } in
  save ~base_dir:registry.base_dir updated;
  let mbsync_conf : Mbsync_config.t = {
    account = {
      host = imap.host;
      port = imap.port;
      user = imap.username;
      password = imap.password;
      use_tls = imap.use_tls;
    };
    maildir_path = maildir_path ~base_dir:registry.base_dir account.id;
    channel_name = "sakura";
  } in
  Mbsync_config.write_config
    ~path:(mbsyncrc_path ~base_dir:registry.base_dir account.id)
    mbsync_conf;
  Hashtbl.remove registry.by_api_key account.api_key;
  registry.accounts <- List.map (fun a ->
    if a.id = account.id then updated else a
  ) registry.accounts;
  Hashtbl.replace registry.by_api_key updated.api_key updated;
  updated

let account_to_json (account : t) =
  `Assoc [
    "id", `String account.id;
    "host", `String account.imap.host;
    "port", `Int account.imap.port;
    "username", `String account.imap.username;
    "use_tls", `Bool account.imap.use_tls;
    "created_at", `String (let t = Unix.gmtime account.created_at in
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
        (1900 + t.Unix.tm_year) (t.Unix.tm_mon + 1) t.Unix.tm_mday
        t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec);
  ]

let unregister registry (account : t) =
  Hashtbl.remove registry.by_api_key account.api_key;
  registry.accounts <- List.filter (fun a -> a.id <> account.id) registry.accounts;
  delete ~base_dir:registry.base_dir account.id

let () =
  let base_dir =
    match Sys.getenv_opt "SAKURA_MAILDIR" with
    | Some dir -> dir
    | None ->
      let home = Sys.getenv "HOME" in
      Filename.concat home ".sakura-mail"
  in
  let port =
    match Sys.getenv_opt "SAKURA_PORT" with
    | Some p -> (match int_of_string_opt p with Some n -> n | None -> 8080)
    | None -> 8080
  in
  Printf.printf "sakura-mail starting on port %d\n" port;
  Printf.printf "Data directory: %s\n%!" base_dir;

  if not (Sys.file_exists base_dir) then
    Unix.mkdir base_dir 0o700;

  let settings = Sakura_mail.Settings.create () in
  let registry = Sakura_mail.Account.create_registry ~base_dir in
  let sync_scheduler = Sakura_mail.Sync.create ~interval_sec:settings.sync_interval_sec () in

  Sakura_mail.Sync.start sync_scheduler ~base_dir
    ~get_accounts:(fun () -> registry.accounts);

  let routes = Sakura_mail.Api.routes ~registry ~sync_scheduler ~base_dir ~settings in

  Dream.run ~port
  @@ Dream.logger
  @@ Dream.router routes

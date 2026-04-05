(** Sync scheduler — background timer + per-user mutex with 409 on conflict. *)

type user_sync = {
  mutex : Lwt_mutex.t;
  mutable last_sync : float option;
}

type t = {
  syncs : (string, user_sync) Hashtbl.t;
  interval_sec : float;
  mutable running : bool;
  mutable background_task : unit Lwt.t option;
}

let create ?(interval_sec = 300.0) () =
  { syncs = Hashtbl.create 16;
    interval_sec;
    running = false;
    background_task = None }

let get_user_sync t account_id =
  match Hashtbl.find_opt t.syncs account_id with
  | Some us -> us
  | None ->
    let us = { mutex = Lwt_mutex.create (); last_sync = None } in
    Hashtbl.replace t.syncs account_id us;
    us

let is_syncing t account_id =
  match Hashtbl.find_opt t.syncs account_id with
  | Some us -> Lwt_mutex.is_locked us.mutex
  | None -> false

let sync_account ~base_dir (account : Account.t) =
  let config_file = Account.mbsyncrc_path ~base_dir account.id in
  let config = { Mbsync.default_config with config_file = Some config_file } in
  let opts = { Mbsync.default_sync_opts with
    create = [Far; Near];
    expunge = [Far; Near];
  } in
  Mbsync.sync ~config ~opts Mbsync.All

let try_sync t ~base_dir (account : Account.t) =
  let us = get_user_sync t account.id in
  if Lwt_mutex.is_locked us.mutex then
    Lwt.return_error `Already_syncing
  else begin
    let%lwt result = Lwt_mutex.with_lock us.mutex (fun () ->
      let%lwt r = sync_account ~base_dir account in
      us.last_sync <- Some (Unix.gettimeofday ());
      Sync_log.append ~base_dir account.id r;
      Lwt.return r
    ) in
    if result.Mbsync.exit_code = 0 then
      Lwt.return_ok result
    else
      Lwt.return_error (`Sync_failed result)
  end

let last_sync_info t account_id =
  match Hashtbl.find_opt t.syncs account_id with
  | Some us -> us.last_sync
  | None -> None

let start t ~base_dir ~get_accounts =
  t.running <- true;
  let task =
    let rec loop () =
      let%lwt () = Lwt_unix.sleep t.interval_sec in
      if not t.running then Lwt.return_unit
      else begin
        let accounts = get_accounts () in
        let%lwt _results = Lwt_list.map_p (fun account ->
          let us = get_user_sync t account.Account.id in
          if Lwt_mutex.is_locked us.mutex then
            Lwt.return_unit
          else
            Lwt.catch
              (fun () ->
                let%lwt _result = Lwt_mutex.with_lock us.mutex (fun () ->
                  let%lwt r = sync_account ~base_dir account in
                  us.last_sync <- Some (Unix.gettimeofday ());
                  Lwt.return r
                ) in
                Lwt.return_unit)
              (fun _exn -> Lwt.return_unit)
        ) accounts in
        loop ()
      end
    in
    loop ()
  in
  t.background_task <- Some task

let stop t =
  t.running <- false;
  match t.background_task with
  | Some task -> Lwt.cancel task; t.background_task <- None
  | None -> ()

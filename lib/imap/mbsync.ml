(** mbsync CLI wrapper — shells out to mbsync and captures results. *)

type direction =
  | Pull
  | Push

type operation =
  | New
  | Gone
  | Flags
  | Upgrade

type side =
  | Far
  | Near

type target =
  | All
  | Channels of (string * string list option) list
  | Groups of string list

type config = {
  mbsync_path : string;
  config_file : string option;
  dry_run : bool;
  verbose : bool;
  quiet : bool;
  ext_exit : bool;
}

type sync_opts = {
  directions : direction list;
  operations : operation list;
  create : side list;
  remove : side list;
  expunge : side list;
}

type result = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

let default_config = {
  mbsync_path = "mbsync";
  config_file = None;
  dry_run = false;
  verbose = false;
  quiet = false;
  ext_exit = false;
}

let default_sync_opts = {
  directions = [];
  operations = [];
  create = [];
  remove = [];
  expunge = [];
}

let string_of_side = function
  | Far -> "far"
  | Near -> "near"

let string_of_direction = function
  | Pull -> "--pull"
  | Push -> "--push"

let string_of_operation = function
  | New -> "--new"
  | Gone -> "--gone"
  | Flags -> "--flags"
  | Upgrade -> "--upgrade"

let config_args config =
  let args = [] in
  let args = match config.config_file with
    | Some f -> args @ ["-c"; f]
    | None -> args
  in
  let args = if config.dry_run then args @ ["--dry-run"] else args in
  let args = if config.verbose then args @ ["--verbose"] else args in
  let args = if config.quiet then args @ ["--quiet"] else args in
  let args = if config.ext_exit then args @ ["--ext-exit"] else args in
  args

let sync_args opts =
  let args = List.map string_of_direction opts.directions in
  let args = args @ List.map string_of_operation opts.operations in
  let side_flags prefix sides =
    List.map (fun s -> prefix ^ "-" ^ string_of_side s) sides
  in
  let args = args @ side_flags "--create" opts.create in
  let args = args @ side_flags "--remove" opts.remove in
  let args = args @ side_flags "--expunge" opts.expunge in
  args

let target_args = function
  | All -> ["--all"]
  | Channels chans ->
    List.map (fun (name, boxes) ->
      match boxes with
      | None | Some [] -> name
      | Some bs -> name ^ ":" ^ String.concat "," bs
    ) chans
  | Groups gs -> gs

let read_all ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  Buffer.contents buf

let exec args =
  let cmd = String.concat " " (List.map Filename.quote args) in
  let env = Unix.environment () in
  let (ic_out, oc_in, ic_err) = Unix.open_process_full cmd env in
  close_out oc_in;
  let stdout = read_all ic_out in
  let stderr = read_all ic_err in
  let status = Unix.close_process_full (ic_out, oc_in, ic_err) in
  let exit_code = match status with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n -> 128 + n
    | Unix.WSTOPPED n -> 128 + n
  in
  { exit_code; stdout; stderr }

let exec_lwt args =
  Lwt_preemptive.detach exec args

let build_sync_args ?(config = default_config) ?(opts = default_sync_opts) target =
  [config.mbsync_path]
  @ config_args config
  @ sync_args opts
  @ target_args target

let sync ?(config = default_config) ?(opts = default_sync_opts) target =
  let args = build_sync_args ~config ~opts target in
  exec_lwt args

let list_mailboxes ?(config = default_config) target =
  let args =
    [config.mbsync_path]
    @ config_args config
    @ ["--list"]
    @ target_args target
  in
  let%lwt result = exec_lwt args in
  if result.exit_code = 0 then
    let boxes =
      String.split_on_char '\n' result.stdout
      |> List.filter (fun s -> String.length s > 0)
    in
    Lwt.return_ok boxes
  else
    Lwt.return_error result

let list_stores ?(config = default_config) stores =
  let args =
    [config.mbsync_path]
    @ config_args config
    @ ["--list-stores"]
    @ stores
  in
  let%lwt result = exec_lwt args in
  if result.exit_code = 0 then
    let stores =
      String.split_on_char '\n' result.stdout
      |> List.filter (fun s -> String.length s > 0)
    in
    Lwt.return_ok stores
  else
    Lwt.return_error result

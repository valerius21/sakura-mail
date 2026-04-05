(** Maildir filesystem operations — read, flag, move, delete messages. *)

(** Flags encoded in filenames after ":2," — e.g. "msg:2,FRS" = Flagged, Replied, Seen *)
type flag =
  | Draft
  | Flagged
  | Replied
  | Seen
  | Trashed

type message_info = {
  id : string;
  path : string;
  mailbox : string;
  flags : flag list;
  is_new : bool;
  size : int;
}

let char_of_flag = function
  | Draft -> 'D'
  | Flagged -> 'F'
  | Replied -> 'R'
  | Seen -> 'S'
  | Trashed -> 'T'

let flag_of_char = function
  | 'D' -> Some Draft
  | 'F' -> Some Flagged
  | 'R' -> Some Replied
  | 'S' -> Some Seen
  | 'T' -> Some Trashed
  | _ -> None

let string_of_flag = function
  | Draft -> "draft"
  | Flagged -> "flagged"
  | Replied -> "replied"
  | Seen -> "seen"
  | Trashed -> "trashed"

let flag_of_string = function
  | "draft" -> Some Draft
  | "flagged" -> Some Flagged
  | "replied" -> Some Replied
  | "seen" -> Some Seen
  | "trashed" -> Some Trashed
  | _ -> None

let parse_flags filename =
  match String.split_on_char ':' filename with
  | [_; info] ->
    (match String.split_on_char ',' info with
     | [_; flags_str] ->
       let flags = ref [] in
       String.iter (fun c ->
         match flag_of_char c with
         | Some f -> flags := f :: !flags
         | None -> ()
       ) flags_str;
       List.rev !flags
     | _ -> [])
  | _ -> []

let message_id_of_filename filename =
  match String.split_on_char ':' filename with
  | id :: _ -> id
  | [] -> filename

let filename_with_flags filename flags =
  let id = message_id_of_filename filename in
  let sorted_flags =
    List.sort (fun a b -> Char.compare (char_of_flag a) (char_of_flag b)) flags
  in
  let flag_str = String.init (List.length sorted_flags) (fun i ->
    char_of_flag (List.nth sorted_flags i)
  ) in
  id ^ ":2," ^ flag_str

let list_mailboxes ~maildir_root =
  let entries = Sys.readdir maildir_root |> Array.to_list in
  entries
  |> List.filter (fun name ->
    let path = Filename.concat maildir_root name in
    Sys.is_directory path
    && (Sys.file_exists (Filename.concat path "cur")
        || Sys.file_exists (Filename.concat path "new")))
  |> List.sort String.compare

let scan_mailbox ~maildir_root ~mailbox =
  let mailbox_path = Filename.concat maildir_root mailbox in
  let scan_subdir subdir is_new =
    let dir = Filename.concat mailbox_path subdir in
    if Sys.file_exists dir && Sys.is_directory dir then
      Sys.readdir dir
      |> Array.to_list
      |> List.filter (fun f -> f.[0] <> '.')
      |> List.map (fun filename ->
        let path = Filename.concat dir filename in
        let flags = parse_flags filename in
        let id = message_id_of_filename filename in
        let size = try (Unix.stat path).Unix.st_size with _ -> 0 in
        { id; path; mailbox; flags; is_new; size })
    else
      []
  in
  let cur_msgs = scan_subdir "cur" false in
  let new_msgs = scan_subdir "new" true in
  new_msgs @ cur_msgs

let read_message_raw msg =
  let ic = open_in msg.path in
  let n = in_channel_length ic in
  let content = really_input_string ic n in
  close_in ic;
  content

let find_message ~maildir_root ~id =
  let mailboxes = list_mailboxes ~maildir_root in
  let rec search = function
    | [] -> None
    | mb :: rest ->
      let msgs = scan_mailbox ~maildir_root ~mailbox:mb in
      (match List.find_opt (fun m -> m.id = id) msgs with
       | Some msg -> Some msg
       | None -> search rest)
  in
  search mailboxes

let set_flags msg new_flags =
  let old_filename = Filename.basename msg.path in
  let new_filename = filename_with_flags old_filename new_flags in
  let dir = Filename.dirname msg.path in
  let new_path = Filename.concat dir new_filename in
  if msg.path <> new_path then
    Sys.rename msg.path new_path;
  { msg with path = new_path; flags = new_flags }

let add_flags msg flags_to_add =
  let new_flags =
    List.sort_uniq (fun a b ->
      Char.compare (char_of_flag a) (char_of_flag b))
      (msg.flags @ flags_to_add)
  in
  set_flags msg new_flags

let remove_flags msg flags_to_remove =
  let new_flags =
    List.filter (fun f -> not (List.mem f flags_to_remove)) msg.flags
  in
  set_flags msg new_flags

let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let ensure_maildir_structure path =
  mkdir_p path;
  List.iter (fun sub ->
    let dir = Filename.concat path sub in
    if not (Sys.file_exists dir) then
      Unix.mkdir dir 0o755
  ) ["cur"; "new"; "tmp"]

let move_message ~maildir_root msg ~dest_mailbox =
  let dest_dir = Filename.concat maildir_root dest_mailbox in
  ensure_maildir_structure dest_dir;
  let filename = Filename.basename msg.path in
  let dest_path = Filename.concat (Filename.concat dest_dir "cur") filename in
  Sys.rename msg.path dest_path;
  { msg with path = dest_path; mailbox = dest_mailbox; is_new = false }

let trash_message ~maildir_root msg =
  if msg.mailbox = "Trash" then
    add_flags msg [Trashed]
  else
    move_message ~maildir_root msg ~dest_mailbox:"Trash"

let create_mailbox ~maildir_root ~name =
  let path = Filename.concat maildir_root name in
  if Sys.file_exists path then
    Error (Printf.sprintf "Mailbox '%s' already exists" name)
  else begin
    ensure_maildir_structure path;
    Ok name
  end

let delete_mailbox ~maildir_root ~name =
  let path = Filename.concat maildir_root name in
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "Mailbox '%s' not found" name)
  else
    let msgs = scan_mailbox ~maildir_root ~mailbox:name in
    if msgs <> [] then
      Error (Printf.sprintf "Mailbox '%s' is not empty (%d messages)" name (List.length msgs))
    else begin
      let rec rm p =
        if Sys.is_directory p then begin
          Array.iter (fun n -> rm (Filename.concat p n)) (Sys.readdir p);
          Unix.rmdir p
        end else
          Sys.remove p
      in
      rm path;
      Ok ()
    end

let rename_mailbox ~maildir_root ~name ~new_name =
  let path = Filename.concat maildir_root name in
  let new_path = Filename.concat maildir_root new_name in
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "Mailbox '%s' not found" name)
  else if Sys.file_exists new_path then
    Error (Printf.sprintf "Mailbox '%s' already exists" new_name)
  else begin
    Sys.rename path new_path;
    Ok new_name
  end

let mailbox_stats ~maildir_root =
  let mailboxes = list_mailboxes ~maildir_root in
  let total_messages = ref 0 in
  let total_size = ref 0 in
  List.iter (fun mb ->
    let msgs = scan_mailbox ~maildir_root ~mailbox:mb in
    total_messages := !total_messages + List.length msgs;
    List.iter (fun m -> total_size := !total_size + m.size) msgs
  ) mailboxes;
  (!total_messages, !total_size)

let write_message ~maildir_root ~mailbox ~content =
  let mailbox_path = Filename.concat maildir_root mailbox in
  ensure_maildir_structure mailbox_path;
  let timestamp = Unix.gettimeofday () in
  let pid = Unix.getpid () in
  let hostname = Unix.gethostname () in
  let unique_id = Printf.sprintf "%.0f.M%dP%d.%s" timestamp (Random.bits ()) pid hostname in
  let filename = unique_id ^ ":2," in
  let tmp_path = Filename.concat (Filename.concat mailbox_path "tmp") filename in
  let cur_path = Filename.concat (Filename.concat mailbox_path "cur") filename in
  let oc = open_out tmp_path in
  output_string oc content;
  close_out oc;
  Sys.rename tmp_path cur_path;
  { id = unique_id; path = cur_path; mailbox; flags = []; is_new = false;
    size = String.length content }

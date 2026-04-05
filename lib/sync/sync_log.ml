type entry = {
  timestamp : string;
  status : string;
  exit_code : int;
  output : string;
}

let log_path ~base_dir account_id =
  Filename.concat
    (Filename.concat base_dir account_id)
    "sync.log.json"

let iso_now () =
  let t = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (1900 + t.Unix.tm_year) (t.Unix.tm_mon + 1) t.Unix.tm_mday
    t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let entry_to_json e =
  `Assoc [
    "timestamp", `String e.timestamp;
    "status", `String e.status;
    "exit_code", `Int e.exit_code;
    "output", `String e.output;
  ]

let max_entries = 50

let read_log ~base_dir account_id =
  let path = log_path ~base_dir account_id in
  if not (Sys.file_exists path) then []
  else
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;
      match Yojson.Safe.from_string content with
      | `List entries ->
        List.filter_map (fun j ->
          match j with
          | `Assoc a ->
            let str key = match List.assoc_opt key a with
              | Some (`String s) -> s | _ -> "" in
            let int_ key = match List.assoc_opt key a with
              | Some (`Int n) -> n | _ -> 0 in
            Some { timestamp = str "timestamp"; status = str "status";
                   exit_code = int_ "exit_code"; output = str "output" }
          | _ -> None
        ) entries
      | _ -> []
    with _ -> []

let append ~base_dir account_id (result : Mbsync.result) =
  let entries = read_log ~base_dir account_id in
  let entry = {
    timestamp = iso_now ();
    status = (if result.exit_code = 0 then "success" else "failed");
    exit_code = result.exit_code;
    output = String.trim (result.stderr ^ result.stdout);
  } in
  let updated = entries @ [entry] in
  let trimmed =
    let len = List.length updated in
    if len > max_entries then
      List.filteri (fun i _ -> i >= len - max_entries) updated
    else updated
  in
  let json = `List (List.map entry_to_json trimmed) in
  let path = log_path ~base_dir account_id in
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string json);
  close_out oc

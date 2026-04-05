type t = {
  mutable bulk_max : int;
  mutable sync_interval_sec : float;
  mutable default_page_limit : int;
}

let create () = {
  bulk_max = (match Sys.getenv_opt "SAKURA_BULK_MAX" with
    | Some s -> (match int_of_string_opt s with Some n -> n | None -> 100)
    | None -> 100);
  sync_interval_sec = 300.0;
  default_page_limit = 50;
}

let to_json t =
  `Assoc [
    "bulk_max", `Int t.bulk_max;
    "sync_interval_sec", `Float t.sync_interval_sec;
    "default_page_limit", `Int t.default_page_limit;
  ]

let update t json =
  (match json with
   | `Assoc a ->
     (match List.assoc_opt "bulk_max" a with
      | Some (`Int n) when n > 0 -> t.bulk_max <- n
      | _ -> ());
     (match List.assoc_opt "sync_interval_sec" a with
      | Some (`Float f) when f > 0.0 -> t.sync_interval_sec <- f
      | Some (`Int n) when n > 0 -> t.sync_interval_sec <- Float.of_int n
      | _ -> ());
     (match List.assoc_opt "default_page_limit" a with
      | Some (`Int n) when n > 0 -> t.default_page_limit <- n
      | _ -> ())
   | _ -> ());
  t

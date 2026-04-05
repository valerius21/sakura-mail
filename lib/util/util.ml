let contains_ci haystack needle =
  let h = String.lowercase_ascii haystack in
  let n = String.lowercase_ascii needle in
  let nl = String.length n in
  let hl = String.length h in
  if nl > hl then false
  else
    let found = ref false in
    for i = 0 to hl - nl do
      if not !found && String.sub h i nl = n then found := true
    done;
    !found

let parse_date_iso s =
  try
    Scanf.sscanf s "%4d-%2d-%2d" (fun y m d ->
      let tm = { Unix.tm_sec = 0; tm_min = 0; tm_hour = 0;
                 tm_mday = d; tm_mon = m - 1; tm_year = y - 1900;
                 tm_wday = 0; tm_yday = 0; tm_isdst = false } in
      Some (fst (Unix.mktime tm)))
  with _ -> None

let parse_iso8601_epoch s =
  try
    Scanf.sscanf s "%4d-%2d-%2dT%2d:%2d:%2d"
      (fun y m d hr mn sc ->
        let tm = { Unix.tm_sec = sc; tm_min = mn; tm_hour = hr;
                   tm_mday = d; tm_mon = m - 1; tm_year = y - 1900;
                   tm_wday = 0; tm_yday = 0; tm_isdst = false } in
        Some (fst (Unix.mktime tm)))
  with _ -> None

open Batteries

type event = {
  summary : string option;
  description : string option;
  dtstart : string option;
  dtend : string option;
  rrule : string option;
}

type events = event list

let string_of_event ev =
  Printf.sprintf "%s: %s - %s"
    (BatOption.default "" ev.summary)
    (BatOption.default "" ev.dtstart)
    (BatOption.default "" ev.dtend)

type ical_field = Field of string * string

let pr lst =
  print_endline @@ String.concat "," @@ BatList.map (uncurry @@ Printf.sprintf "(%s,%s)") lst

(** Reades lines from an input channel and returns them in reverse order. *)
let read_lines_rev i = 
  let res = ref [] in
    try
      while true do
        let line = BatString.strip ~chars:"\r\n" @@ Pervasives.input_line i in
          res := line :: !res
      done;
      !res
    with
        End_of_file -> !res

(** Adjusts lines, i.e. every line that begins with a space is concated with the
  previous (or as it is a reverse list, next) line. *)
let rec adjust_lines_rev lines = match lines with
  | [] -> []
  | x :: y :: xs ->
      if x.[0] = ' ' then
        adjust_lines_rev @@ (y ^ BatString.trim x) :: xs
      else
        x :: adjust_lines_rev (y :: xs)
  | x :: xs -> x :: adjust_lines_rev xs

(** Splits a line by ":" or ";" and returns
  the two parts.

  @exception Not_found if line does not contain ':' and ';'. *)
let convert_ical (line : string) : string * string =
  let p =
    try
      let (field,value) = BatString.split line ";" in
        (field,value)
    with
      | Not_found ->
          let (field,value) = BatString.split line ":" in
            (field,value)
  in
    BatTuple.Tuple2.mapn BatString.trim p

let assoc_option key lst =
  try
    Some(BatList.assoc key lst)
  with
    | Not_found -> None

let read_event lst = 
  {
    summary = assoc_option "SUMMARY" lst;
    description = assoc_option "DESCRIPTION" lst;
    dtstart  = assoc_option "DTSTART" lst;
    dtend  = assoc_option "DTEND" lst;
    rrule = assoc_option "RRULE" lst;
  }

let rec read_events ical : events = match ical with
  | [] -> []
  | ("BEGIN","VEVENT") :: xs ->
      let p_end_event (k,v) =
        let res = not (k = "END" && v = "VEVENT") in
        res in
      let (eventlst,rest) = BatList.span p_end_event xs in
      let event = read_event eventlst in
        event :: read_events rest
  | _ :: xs -> 
      read_events xs

(** Reads an ical entry. *)
let read_ical (i : in_channel) =
  let lines_rev = read_lines_rev i in
  let adj_lines = BatList.rev @@ adjust_lines_rev lines_rev in
  let lst = BatList.map convert_ical adj_lines in
  let events = read_events lst in
    print_endline @@ String.concat "\n" @@ BatList.map string_of_event events

let date : Netdate.t = failwith "Test"

let main () =
  let ical = read_ical @@ Pervasives.stdin in
    ()
;;

main ()

(*

/^RRULE:FREQ=(DAILY|WEEKLY|MONTHLY|YEARLY)/ {
    # get the d, w, m or y value
    freq = tolower(gensub(/.*FREQ=(.).*/, "\\1", $0))
    # get the interval, and use 1 if none specified
    interval =  $2 ~ /INTERVAL=/ ? gensub(/.*INTERVAL=([0-9]+);.*/, "\\1", $2) : 1
    # get the enddate of the rule and use "" if none specified
    rrend = $2 ~ /UNTIL=/ ? datestring(gensub(/.*UNTIL=([0-9]{8}).*/, "\\1", $2)) : ""
    # build the repetitor vale as understood by org
    intfreq =  " +" interval freq
    # if the repetition is daily, and there is an end date, drop the repetitor
    # as that is the default
    if (intfreq == " +1d" && time2 =="" && rrend != "")
        intfreq = ""
}
   *)

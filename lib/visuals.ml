open Corestats

type t = string list list

(* gets what bin index an element should be in *)
let bin_index (min : float) (max : float) (x : float) (bins : int) =
  let bin_width = (max -. min) /. float_of_int bins in
  if x = max then bins - 1 else int_of_float ((x -. min) /. bin_width)

let histogram_values (table : Csv.t) (name : string) (bins : int) =
  let hist = Array.make bins 0 in
  let max = max_col table name in
  let min = min_col table name in
  List.iter
    (fun x ->
      hist.(bin_index min max x bins) <- hist.(bin_index min max x bins) + 1)
    (List.map float_of_string (get_column_by_name table name));
  Array.to_list hist

let histogram_bins (table : Csv.t) (name : string) (bins : int) =
  let hist = Array.make bins (0., 0.) in
  let max = max_col table name in
  let min = min_col table name in
  let bin_width = (max -. min) /. float_of_int bins in
  Array.iteri
    (fun i x ->
      hist.(i) <-
        ( min +. (bin_width *. float_of_int i),
          min +. (bin_width *. float_of_int (i + 1)) ))
    hist;
  Array.to_list hist

(* gets the percentile (0.15, i.e.) of a particular number for a particular
   column, will raise InvalidColumn if needed*)
let get_percentile (table : Csv.t) (value : float) (col : string) (max : float)
    (min : float) =
  let range = max -. min in
  let p = (value -. min) /. range in
  (* edge case for max value - we need to round down *)
  if p = 1. then 0.99 else p

(* round float to 2 decimal places *)
let roundfloat2 n = Float.round (n *. 100.0) /. 100.0

(* adds some empty characters to the end of a string so it reaches a given
   length*)
let string_formatter n str =
  try
    let len = String.length str in
    str ^ String.make (n - len) ' '
  with Invalid_argument s -> str

let scatter (table : Csv.t) (xcol : string) (ycol : string) =
  (* this is really a 20 x 20 matrix, extra 2 rows/cols are for formatting *)
  let plot = Array.make_matrix 22 22 "  " in
  try
    (* gets a float list of values *)
    let xs = List.map float_of_string (get_column_by_name table xcol) in
    let ys = List.map float_of_string (get_column_by_name table ycol) in
    let xmax = max_col table xcol in
    let xmin = min_col table xcol in
    let ymax = max_col table ycol in
    let ymin = min_col table ycol in
    let _ =
      (* maps the percentiles for each to an array index *)
      List.map2
        (fun x y ->
          plot.(20 - Float.to_int (get_percentile table y ycol ymax ymin *. 20.)).(
          Float.to_int (get_percentile table x xcol xmax xmin *. 20.) + 2) <-
            "◍ ")
        xs ys
    in
    (* making axes, etc. *)
    for i = 0 to 21 do
      plot.(i).(1) <- " |";
      plot.(i).(0) <- "    "
    done;
    for j = 0 to 21 do
      plot.(20).(j) <- "--";
      plot.(21).(j) <- "  "
    done;
    plot.(20).(1) <- " ●";
    plot.(20).(21) <- "-|";
    plot.(0).(1) <- " T";
    (* adding axes labels *)
    let miny = roundfloat2 (min_col table ycol) in
    let minx = roundfloat2 (min_col table xcol) in
    let maxy = roundfloat2 (max_col table ycol) in
    let maxx = roundfloat2 (max_col table xcol) in
    plot.(20).(0) <- string_formatter 4 (string_of_float miny);
    plot.(0).(0) <- string_formatter 4 (string_of_float maxy);
    plot.(21).(1) <- "   " ^ string_of_float minx;
    plot.(21).(21) <- string_of_float maxx;
    plot
  with _ -> raise InvalidColumn

let heatmap (table : Csv.t) (xcol : string) (ycol : string) =
  (* this is really a 10 x 10 matrix, extra 2 rows/cols are for formatting *)
  let plot = Array.make_matrix 12 12 0. in
  try
    (* gets a float list of values *)
    let xs = List.map float_of_string (get_column_by_name table xcol) in
    let ys = List.map float_of_string (get_column_by_name table ycol) in
    let xmax = max_col table xcol in
    let xmin = min_col table xcol in
    let ymax = max_col table ycol in
    let ymin = min_col table ycol in
    let total = get_len_col table xcol in
    let _ =
      (* maps the percentiles for each to an array index *)
      List.map2
        (fun x y ->
          let col_index =
            10 - Float.to_int (get_percentile table y ycol ymax ymin *. 10.)
          in
          let row_index =
            Float.to_int (get_percentile table x xcol xmax xmin *. 10.) + 2
          in

          plot.(col_index).(row_index) <-
            plot.(col_index).(row_index) +. roundfloat2 (1. /. total))
        xs ys
    in
    let max_val_len =
      Array.fold_left
        (fun acc x ->
          max acc
            (Array.fold_left
               (fun acc' y -> max acc' (String.length (string_of_float y)))
               0 x))
        0 plot
    in
    let max_val =
      Array.fold_left
        (fun acc x -> max acc (Array.fold_left (fun acc' y -> max acc' y) 0. x))
        0. plot
    in
    let plot =
      Array.map
        (fun x ->
          Array.map
            (fun y -> string_formatter max_val_len (string_of_float y))
            x)
        plot
    in
    (* making axes, etc. *)
    for i = 0 to 11 do
      plot.(i).(1) <- " |";
      plot.(i).(0) <- "     "
    done;
    for j = 0 to 11 do
      plot.(10).(j) <- "-----";
      plot.(11).(j) <- string_formatter max_val_len "     "
    done;
    plot.(10).(1) <- " ●";
    plot.(10).(11) <- "-|";
    plot.(0).(1) <- " T";
    (* adding axes labels *)
    let miny = roundfloat2 (min_col table ycol) in
    let minx = roundfloat2 (min_col table xcol) in
    let maxy = roundfloat2 (max_col table ycol) in
    let maxx = roundfloat2 (max_col table xcol) in
    plot.(10).(0) <- string_formatter 4 (string_of_float miny);
    plot.(0).(0) <- string_formatter 4 (string_of_float maxy);
    plot.(11).(1) <- string_formatter max_val_len (string_of_float minx);
    plot.(11).(11) <- string_formatter max_val_len (string_of_float maxx);
    (plot, max_val)
  with _ -> raise InvalidColumn

let box_and_whiskers (table : Csv.t) (col : string) =
  (* this is really a 11 x 11 matrix, extra 2 rows/cols are for formatting *)
  let plot = Array.make_matrix 13 12 "  " in
  try
    (* gets a float list of values *)
    let xs =
      List.sort Stdlib.compare
        (List.map float_of_string (get_column_by_name table col))
    in
    let median = List.nth xs (List.length xs / 2) in
    let q1 = List.nth xs (List.length xs / 4) in
    let q3 = List.nth xs (3 * List.length xs / 4) in
    let max = max_col table col in
    let min = min_col table col in

    for
      j = 1 to Float.to_int (get_percentile table q1 col max min *. 11.) + 1
    do
      plot.(5).(j) <- "--"
    done;
    plot.(5).(1) <- "●-";

    plot.(4).(Float.to_int (get_percentile table q1 col max min *. 11.) + 1) <-
      "| ";
    plot.(5).(Float.to_int (get_percentile table q1 col max min *. 11.) + 1) <-
      "| ";
    plot.(6).(Float.to_int (get_percentile table q1 col max min *. 11.) + 1) <-
      "| ";

    for
      j = Float.to_int (get_percentile table q1 col max min *. 11.) + 1
      to Float.to_int (get_percentile table q3 col max min *. 11.) + 1
    do
      plot.(3).(j) <- "--";
      plot.(7).(j) <- "--"
    done;

    plot.(4).(Float.to_int (get_percentile table median col max min *. 11.) + 1) <-
      " | ";
    plot.(5).(Float.to_int (get_percentile table median col max min *. 11.) + 1) <-
      " | ";
    plot.(6).(Float.to_int (get_percentile table median col max min *. 11.) + 1) <-
      " | ";

    for
      j = Float.to_int (get_percentile table q3 col max min *. 11.) + 1 to 11
    do
      plot.(5).(j) <- "--"
    done;

    plot.(4).(Float.to_int (get_percentile table q3 col max min *. 11.) + 1) <-
      "|";
    plot.(5).(Float.to_int (get_percentile table q3 col max min *. 11.) + 1) <-
      "|";
    plot.(6).(Float.to_int (get_percentile table q3 col max min *. 11.) + 1) <-
      "|";

    plot.(5).(11) <- "-●";
    (* making axes, etc. *)
    for i = 0 to 12 do
      plot.(i).(0) <- " |"
    done;
    for j = 0 to 11 do
      plot.(11).(j) <- "--";
      plot.(12).(j) <- " "
    done;
    plot.(11).(0) <- " ●";
    plot.(11).(11) <- "-|";
    plot.(0).(0) <- " T";
    (* adding labels *)
    plot.(12).(1) <-
      string_formatter 4 (string_of_float (roundfloat2 (min_col table col)));
    plot.(12).(Float.to_int (get_percentile table median col max min *. 11.) + 1) <-
      string_formatter 4 ("  " ^ string_of_float (roundfloat2 median));
    plot.(12).(11) <-
      string_formatter 4
        ("   " ^ string_of_float (roundfloat2 (max_col table col)));
    plot
  with _ -> raise InvalidColumn

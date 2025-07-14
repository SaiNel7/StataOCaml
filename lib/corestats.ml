type t = string list list

exception ColumnNotFound
exception InvalidColumn

(* the columns are the first row of data in the table *)
let get_columns (table : Csv.t) = List.nth table 0

let get_col_index (table : Csv.t) (name : string) =
  let col_index = List.find_index (fun x -> x = name) (get_columns table) in
  match col_index with
  | None -> raise ColumnNotFound
  | Some i -> i

let get_column_by_name (table : Csv.t) (col : string) =
  let index = get_col_index table col in
  List.tl (List.nth (Csv.transpose table) index)

(* [get_mean_list lst] is the arithmetic mean of the list [lst]. Raises:
   [InvalidColumn] if [lst] contains strings that cannot be parsed to floats.
   [Division_by_zero] if the list length will result in a divison by 0 error. *)
let get_mean_list (lst : string list) =
  if List.length lst > 0 then
    try
      List.fold_left ( +. ) 0. (List.map float_of_string lst)
      /. float_of_int (List.length lst)
    with Failure x -> raise InvalidColumn
  else raise Division_by_zero

(* [get_std_list lst] is the sample standard deviation of the list [lst].
   Raises: [InvalidColumn] if [lst] contains strings that cannot be parsed to
   floats. [Division_by_zero] if the list length will result in a divison by 0
   error.*)
let get_std_list (lst : string list) =
  if List.length lst > 1 then
    try
      let mean = get_mean_list lst in
      (List.fold_left ( +. ) 0.
         (List.map (fun x -> (float_of_string x -. mean) ** 2.) lst)
      /. (float_of_int (List.length lst) -. 1.))
      ** 0.5
    with Failure x -> raise InvalidColumn
  else raise Division_by_zero

let get_cov_list (xs : string list) (ys : string list) =
  if List.length xs > 1 && List.length ys > 1 then
    try
      let xbar = get_mean_list xs in
      let ybar = get_mean_list ys in
      List.fold_left ( +. ) 0.
        (List.map2
           (fun x y ->
             (float_of_string x -. xbar) *. (float_of_string y -. ybar))
           xs ys)
      /. (float_of_int (List.length xs) -. 1.)
    with Failure x -> raise InvalidColumn
  else raise Division_by_zero

(* [col_operation op t n] performs a list operation on a column [n] in table
   [t], exlcuding the column name.*)
let col_operation (operation : string list -> float) (table : Csv.t)
    (name : string) =
  operation (get_column_by_name table name)

let get_mean_col = col_operation get_mean_list
let get_std_col = col_operation get_std_list

(* [float_length lst] is just the length of [lst] converted to a float.*)
let float_length lst = float_of_int (List.length lst)
let get_len_col = col_operation float_length

let get_cov_col1_col2 (table : Csv.t) (col1_name : string) (col2_name : string)
    =
  get_cov_list
    (get_column_by_name table col1_name)
    (get_column_by_name table col2_name)

let max_col (table : Csv.t) (name : string) =
  try
    List.fold_left max neg_infinity
      (List.map float_of_string (get_column_by_name table name))
  with
  | ColumnNotFound -> raise ColumnNotFound
  | _ -> raise InvalidColumn

let min_col (table : Csv.t) (name : string) =
  try
    List.fold_left min infinity
      (List.map float_of_string (get_column_by_name table name))
  with
  | ColumnNotFound -> raise ColumnNotFound
  | _ -> raise InvalidColumn

let head (table : Csv.t) (name : string) =
  List.filteri (fun i _ -> i < 5) (get_column_by_name table name)

let median table name =
  try
    let lst =
      List.sort Stdlib.compare
        (List.map float_of_string (get_column_by_name table name))
    in
    List.nth lst (List.length lst / 2)
  with _ -> raise InvalidColumn

let mode table name =
  let lst = get_column_by_name table name in
  let set = Hashtbl.create (List.length lst) in
  let _ =
    List.map
      (fun x -> Hashtbl.add set x (1 + try Hashtbl.find set x with _ -> 1))
      lst
  in
  let maxvalue =
    List.fold_left
      (fun acc x -> max (snd x) acc)
      0
      (List.of_seq (Hashtbl.to_seq set))
  in
  List.map fst
    (List.find_all
       (fun x -> snd x = maxvalue)
       (List.of_seq (Hashtbl.to_seq set)))

let iqr table name =
  try
    let lst =
      List.sort Stdlib.compare
        (List.map float_of_string (get_column_by_name table name))
    in
    ( List.nth lst 0,
      List.nth lst (List.length lst / 4),
      List.nth lst (2 * List.length lst / 4),
      List.nth lst (3 * List.length lst / 4),
      List.nth lst (List.length lst - 1) )
  with _ -> raise InvalidColumn

let get_corr_col1_col2 table col1 col2 =
  get_cov_col1_col2 table col1 col2
  /. (get_std_col table col1 *. get_std_col table col2)

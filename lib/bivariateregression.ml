type t = string list list

(* helper function to get the column by name *)
let get_column_by_name (table : Csv.t) (col : string) =
  let index = Corestats.get_col_index table col in
  List.tl (List.nth (Csv.transpose table) index)

let b1 t xcol ycol =
  Corestats.get_cov_col1_col2 t xcol ycol /. (Corestats.get_std_col t xcol ** 2.)

let b0 t xcol ycol =
  Corestats.get_mean_col t ycol
  -. (Corestats.get_mean_col t xcol *. b1 t xcol ycol)

(* helper function that gets a list of error terms based on regression
   parameters *)
let errors t xcol ycol =
  let beta_1 = b1 t xcol ycol in
  let beta_0 = b0 t xcol ycol in
  let true_x = get_column_by_name t xcol in
  let true_y = get_column_by_name t ycol in
  List.map2
    (fun x y -> float_of_string y -. (beta_0 +. (beta_1 *. float_of_string x)))
    true_x true_y

let mean_absolute_error t xcol ycol =
  let lst = errors t xcol ycol in
  List.fold_left (fun acc x -> acc +. abs_float x) 0. lst
  /. float_of_int (List.length lst)

let mean_squared_error t xcol ycol =
  let lst = errors t xcol ycol in
  List.fold_left ( +. ) 0. (List.map (fun x -> x ** 2.) lst)
  /. float_of_int (List.length lst)

let total_sum_of_squares t ycol =
  let true_y = get_column_by_name t ycol in
  let ybar = Corestats.get_mean_col t ycol in
  List.fold_left ( +. ) 0.
    (List.map (fun y -> (float_of_string y -. ybar) ** 2.) true_y)

let sum_of_squared_residuals t xcol ycol =
  float_of_int (List.length (get_column_by_name t xcol))
  *. mean_squared_error t xcol ycol

let r_squared t xcol ycol =
  1. -. (sum_of_squared_residuals t xcol ycol /. total_sum_of_squares t ycol)

let standard_error t xcol ycol =
  let n = float_of_int (List.length (get_column_by_name t xcol)) in
  (sum_of_squared_residuals t xcol ycol
  /. (n -. 1.)
  /. ((n -. 1.) *. (Corestats.get_std_col t xcol ** 2.)))
  ** 0.5

let confidence_interval t xcol ycol =
  ( b1 t xcol ycol -. (2. *. standard_error t xcol ycol),
    b1 t xcol ycol +. (2. *. standard_error t xcol ycol) )

let b1' t (xcol : string list) (xcolname : string) (ycolname : string) =
  Corestats.get_cov_list xcol (Corestats.get_column_by_name t ycolname)
  /. (Corestats.get_std_col t xcolname ** 2.)

let tvalue t xcol ycol = b1 t xcol ycol /. standard_error t xcol ycol

let pvalue t xcol ycol =
  let xlist = Corestats.get_column_by_name t xcol in
  let b1_true = b1 t xcol ycol in
  let coef_arr = Array.make 1000 0. in
  for x = 0 to 999 do
    let xshuffle = BatList.shuffle xlist in
    let b1hat = b1' t xshuffle xcol ycol in
    coef_arr.(x) <- b1hat
  done;
  Array.sort Stdlib.compare coef_arr;
  let index = Array.find_index (fun x -> x > b1_true) coef_arr in
  match index with
  | None -> 0.
  | Some i ->
      let i' = float_of_int i in
      if i' < 500. then 2. *. i' /. 1000. else 2. *. (1000. -. i') /. 1000.

let predict t xcol ycol elem =
  try (b1 t xcol ycol *. float_of_string elem) +. b0 t xcol ycol
  with Failure _ -> raise Corestats.InvalidColumn

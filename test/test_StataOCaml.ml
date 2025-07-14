open OUnit2
open StataOCaml

(* [cmp_func] is a helper function for various tests. It sorts the lists and
   compares the sorted versions to make the initial order of lists not matter
   for comparisons.*)
let cmp_func input1 input2 =
  if List.equal ( = ) (List.sort compare input1) (List.sort compare input2) then
    true
  else false

(* test column getter *)
let get_columns_test name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Corestats.get_columns input) ~cmp:cmp_func
    ~printer:(fun x -> String.concat "," x)

(* test mean getter *)
let get_mean_col_test name table col expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Corestats.get_mean_col table col)
    ~cmp:cmp_float ~printer:string_of_float

(* test std getter *)
let get_std_col_test name table col expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Corestats.get_std_col table col)
    ~cmp:cmp_float ~printer:string_of_float

(* test cov getter *)
let get_cov_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Corestats.get_cov_col1_col2 table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(* test len getter *)
let get_len_col_test name table col expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Corestats.get_len_col table col)
    ~cmp:cmp_float ~printer:string_of_float

(* test b0 getter *)
let b0_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.b0 table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(* test b1 getter *)
let b1_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.b1 table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(* test MAE getter *)
let mae_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.mean_absolute_error table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(* test MSE getter *)
let mse_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.mean_squared_error table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(* test TSS getter *)
let tss_test name table col expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.total_sum_of_squares table col)
    ~cmp:cmp_float ~printer:string_of_float

(* test SSR getter *)
let ssr_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.sum_of_squared_residuals table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(* test R^2 getter *)
let r_squared_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.r_squared table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

let col_list =
  [ "BA"; "OBP"; "SLG"; "OPS"; "GP"; "PA"; "AB"; "R"; "H"; "2B"; "3B"; "HR" ]

let self_r_squared_test =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.int_range 0 (List.length col_list - 1))
    (fun x ->
      1.
      = Bivariateregression.r_squared
          (Csv.load "../data/baseball.csv")
          (List.nth col_list x) (List.nth col_list x))
    ~print:QCheck.Print.int

(* test SE getter *)
let se_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.standard_error table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(*test min getter*)
let get_min_col_test name table col expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Corestats.min_col table col)
    ~cmp:cmp_float ~printer:string_of_float

(*test max getter*)
let get_max_col_test name table col expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Corestats.max_col table col)
    ~cmp:cmp_float ~printer:string_of_float

(* test that the end value is max (min is easily true) *)
let hist_bin_max_test name table col bins =
  name >:: fun _ ->
  let result = Visuals.histogram_bins table col bins in
  let max = snd (List.nth result (List.length result - 1)) in
  assert_equal
    (Corestats.max_col table col)
    max ~cmp:cmp_float ~printer:string_of_float

(* helper for below qcheck test *)
let hist_bin_max_test_helper table col bins =
  let result = Visuals.histogram_bins table col bins in
  let max = snd (List.nth result (List.length result - 1)) in
  Float.to_int (Corestats.max_col table col) = Float.to_int max

let q_hist_bin_max_test =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.pair
       (QCheck2.Gen.int_range 0 (List.length col_list - 1))
       QCheck2.Gen.small_nat)
    (fun (x, y) ->
      hist_bin_max_test_helper
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x) (y + 1))
    ~print:(QCheck.Print.pair QCheck.Print.int QCheck.Print.int)

(* test that a given bin has the right number of elements *)
let hist_count_test name table col bins bin_index expected_output =
  let result = Visuals.histogram_values table col bins in
  name >:: fun _ -> assert_equal expected_output (List.nth result bin_index)

(* test head *)
let head_test name table col expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Corestats.head table col) ~printer:(fun x ->
      String.concat "," x)

(* tests browse helper function taking the right elements (quasi-test of main
   method function)*)
let browse_test name input start len expected_output =
  name >:: fun _ ->
  if start < 0 || len < 0 then invalid_arg "list_slice"
  else
    let rec drop n l =
      if n <= 0 then l
      else
        match l with
        | [] -> []
        | _ :: t -> drop (n - 1) t
    in
    let rec take n l =
      if n <= 0 then []
      else
        match l with
        | [] -> []
        | h :: t -> h :: take (n - 1) t
    in
    let output = take len (drop start input) in
    assert_equal expected_output output ~printer:(String.concat ";")

(* test scatter x-axis property *)
let scatter_test_x table col1 col2 =
  Array.for_all (fun x -> x <> "◍") (Visuals.scatter table col1 col2).(20)

let q_scatter_test_x =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.pair
       (QCheck2.Gen.int_range 0 (List.length col_list - 1))
       (QCheck2.Gen.int_range 0 (List.length col_list - 1)))
    (fun (x, y) ->
      scatter_test_x
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x) (List.nth col_list y))
    ~print:(QCheck.Print.pair QCheck.Print.int QCheck.Print.int)

(* test scatter y-axis property *)
let scatter_test_y table col1 col2 =
  Array.for_all (fun x -> x.(1) <> " ◍") (Visuals.scatter table col1 col2)

let q_scatter_test_y =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.pair
       (QCheck2.Gen.int_range 0 (List.length col_list - 1))
       (QCheck2.Gen.int_range 0 (List.length col_list - 1)))
    (fun (x, y) ->
      scatter_test_y
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x) (List.nth col_list y))
    ~print:(QCheck.Print.pair QCheck.Print.int QCheck.Print.int)

(* test heatmap for max value property functioning properly *)
let heat_test table col1 col2 = 1. > snd (Visuals.heatmap table col1 col2)

let q_heat_test =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.pair
       (QCheck2.Gen.int_range 0 (List.length col_list - 1))
       (QCheck2.Gen.int_range 0 (List.length col_list - 1)))
    (fun (x, y) ->
      heat_test
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x) (List.nth col_list y))
    ~print:(QCheck.Print.pair QCheck.Print.int QCheck.Print.int)

(* test heatmap x-axis property *)
let heatmap_test_x table col1 col2 =
  Array.for_all (fun x -> x <> " ") (fst (Visuals.heatmap table col1 col2)).(10)

let q_heatmap_test_x =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.pair
       (QCheck2.Gen.int_range 0 (List.length col_list - 1))
       (QCheck2.Gen.int_range 0 (List.length col_list - 1)))
    (fun (x, y) ->
      heatmap_test_x
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x) (List.nth col_list y))
    ~print:(QCheck.Print.pair QCheck.Print.int QCheck.Print.int)

(* test boxwhisker x-axis property *)
let box_and_whiskers_test_x table col1 =
  Array.for_all (fun x -> x <> " ") (Visuals.box_and_whiskers table col1).(11)

let q_box_and_whiskers_test_x =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.int_range 0 (List.length col_list - 1))
    (fun x ->
      box_and_whiskers_test_x
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x))
    ~print:QCheck.Print.int

(* test boxwhisker y-axis property *)
let box_and_whiskers_test_y table col1 =
  Array.for_all
    (fun x -> x.(0) = " " || x.(0) = " |" || x.(0) = " ●" || x.(0) = " T")
    (Visuals.box_and_whiskers table col1)

let q_box_and_whiskers_test_y =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.int_range 0 (List.length col_list - 1))
    (fun x ->
      box_and_whiskers_test_y
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x))
    ~print:QCheck.Print.int

(* test boxwhisker min property *)
let box_and_whiskers_test_min table col1 =
  if (Visuals.box_and_whiskers table col1).(5).(1) = "●-" then true
  else
    let a, b, _, _, _ = Corestats.iqr table col1 in
    a = b

let q_box_and_whiskers_test_min =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.int_range 0 (List.length col_list - 1))
    (fun x ->
      box_and_whiskers_test_min
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x))
    ~print:QCheck.Print.int

(* test boxwhisker max property *)
let box_and_whiskers_test_max table col1 =
  (Visuals.box_and_whiskers table col1).(5).(11) = "-●"

let q_box_and_whiskers_test_max =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.int_range 0 (List.length col_list - 1))
    (fun x ->
      box_and_whiskers_test_min
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x))
    ~print:QCheck.Print.int

(*test lower confidence interval bound*)
let ci_test_lower name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (fst (Bivariateregression.confidence_interval table col1 col2))
    ~cmp:cmp_float ~printer:string_of_float

(*test upper confidence interval bound*)
let ci_test_upper name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (snd (Bivariateregression.confidence_interval table col1 col2))
    ~cmp:cmp_float ~printer:string_of_float

(*test t-stat*)
let t_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.tvalue table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(*test p-value*)
let p_value_test name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.pvalue table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(*test prediction*)
let prediction_value_test name table col1 col2 value expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Bivariateregression.predict table col1 col2 value)
    ~cmp:cmp_float ~printer:string_of_float

(*test prediction for x=0 [should just be intercept]*)
let prediction_value_intercept_test table col1 col2 =
  Bivariateregression.b0 table col1 col2
  = Bivariateregression.predict table col1 col2 "0"

let intercept_test =
  QCheck2.Test.make ~count:100
    (QCheck2.Gen.pair
       (QCheck2.Gen.int_range 0 (List.length col_list - 1))
       (QCheck2.Gen.int_range 0 (List.length col_list - 1)))
    (fun (x, y) ->
      prediction_value_intercept_test
        (Csv.load "../data/baseball.csv")
        (List.nth col_list x) (List.nth col_list y))
    ~print:(QCheck.Print.pair QCheck.Print.int QCheck.Print.int)

(*test mode in data*)
let test_mode_col name table col1 expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Corestats.mode table col1) ~cmp:cmp_func
    ~printer:(fun x -> String.concat "," x)

(*test median*)
let test_median name table col1 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Corestats.median table col1)
    ~cmp:cmp_float ~printer:string_of_float

(*test iqr*)
let test_iqr name table col1 expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Corestats.iqr table col1)
    ~printer:(fun (a, b, c, d, e) ->
      string_of_float a ^ string_of_float b ^ string_of_float c
      ^ string_of_float d ^ string_of_float e)

(*test corr*)
let test_corr name table col1 col2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Corestats.get_corr_col1_col2 table col1 col2)
    ~cmp:cmp_float ~printer:string_of_float

(* assert a function raises some error *)
let failure_test name func table col failure =
  name >:: fun _ -> assert_raises failure (fun () -> func table col)

(* assert a (high-level) function raises some error *)
let failure_test_high_level name func table col1 col2 failure =
  name >:: fun _ -> assert_raises failure (fun () -> func table col1 col2)

(* reused data for below tests *)
let data = Csv.load "../data/baseball.csv"
let simple_data = Csv.load "../data/simple.csv"
let stress_data = Csv.load "../data/olympics.csv"
let (edge_case_data : Csv.t) = [ [ "colA" ]; [ "1" ] ]
let column_data = Corestats.get_column_by_name data "BA"
let column_data_simple = Corestats.get_column_by_name simple_data "A"

let tests =
  "test suite"
  >::: [
         (* basic tests with normal-sized data *)
         (* note that formatting is done automatically by guidelines *)
         get_columns_test "sample data columns test" data
           [
             "Player";
             "Team";
             "POS";
             "BA";
             "OBP";
             "SLG";
             "OPS";
             "GP";
             "PA";
             "AB";
             "R";
             "H";
             "2B";
             "3B";
             "HR";
           ];
         get_columns_test "edge case data columns test" edge_case_data
           [ "colA" ];
         get_mean_col_test "sample data col mean test" data "GP" 39.0172;
         get_mean_col_test "edge case col mean test" edge_case_data "colA" 1.;
         get_mean_col_test "sample data col mean test" data "BA" 0.2841034483;
         get_mean_col_test "sample data col mean test" data "OBP" 0.3785;
         get_std_col_test "sample data col std test" data "BA" 0.0492261839;
         get_std_col_test "sample data col std test" data "GP" 4.414852839;
         get_len_col_test "sample data col len test" data "GP" 58.;
         get_len_col_test "edge case col len test" data "BA" 58.;
         get_cov_test "sample data cov test" data "HR" "SLG" 0.46117332;
         test_corr "sample data corr test" data "AB" "R" 0.604008001;
         test_corr "correlation with itself should be 1" data "AB" "AB" 1.;
         test_corr "correlation with itself should be 1" data "OBP" "OBP" 1.;
         test_median "sample data median test" data "BA" 0.285;
         test_mode_col "sample data mode test" data "HR" [ "1" ];
         test_mode_col "simple data mode test" simple_data "A" [ "1"; "2" ];
         test_iqr "sample data iqr full test" data "AB"
           (87., 134., 146., 162., 213.);
         b0_test "sample data b0 test" data "HR" "SLG" 0.3275232;
         b1_test "sample data b1 test" data "HR" "SLG" 0.0248794;
         mae_test "sample data mae test" data "HR" "SLG" 0.048937;
         mse_test "sample data mse test" data "HR" "SLG" 0.0040448;
         tss_test "sample data tss test" data "SLG" 0.88860;
         ssr_test "sample data ssr test" data "HR" "SLG" 0.234600;
         r_squared_test "sample data b0 test" data "HR" "SLG" 0.7359899;
         r_squared_test "r^2 of itself should be 1" data "HR" "HR" 1.;
         r_squared_test "r^2 of itself should be 1" data "SLG" "SLG" 1.;
         se_test "sample data b0 test" data "HR" "SLG" 0.00197368;
         get_mean_col_test "summary mean test AB" data "AB" 145.4827586;
         get_std_col_test "summary std dev test AB" data "AB" 26.5100058;
         get_min_col_test "summary min test AB" data "AB" 87.;
         get_max_col_test "summary max test AB" data "AB" 213.;
         get_len_col_test "summary length test AB" data "AB" 58.;
         hist_bin_max_test "histogram max from BA column" data "BA" 10;
         hist_bin_max_test "histogram max from OPS column" data "OPS" 5;
         hist_bin_max_test "histogram max from SLG column" data "SLG" 1;
         hist_count_test "histogram count from SLG column" data "SLG" 1 0 58;
         hist_count_test "histogram count from OPS column" data "OPS" 5 0 3;
         failure_test "get non-existant column" Corestats.get_column_by_name
           data "AVG" Corestats.ColumnNotFound;
         failure_test "get max for faulty column" Corestats.max_col data "Team"
           Corestats.InvalidColumn;
         failure_test "get max for faulty column" Corestats.min_col data
           "Player" Corestats.InvalidColumn;
         failure_test "get mean for faulty column" Corestats.get_mean_col data
           "Player" Corestats.InvalidColumn;
         failure_test "get std for faulty column" Corestats.get_std_col data
           "Player" Corestats.InvalidColumn;
         failure_test_high_level "get high-level command for faulty column"
           Bivariateregression.b1 data "OPS" "Player" Corestats.InvalidColumn;
         head_test "get head for Player column" data "Player"
           [
             "Ben Rounds";
             "John Quinlan";
             "Mark Quatrani";
             "Wyatt Henseler";
             "Max Jensen";
           ];
         ci_test_lower "lower bound of confidence interval" data "HR" "SLG"
           0.0209321;
         ci_test_upper "upper bound of confidence interval" data "HR" "SLG"
           0.0288268;
         t_test "t-stat for regression coefficient" data "HR" "SLG" 12.605590;
         p_value_test "p-value for regression coefficient" data "HR" "SLG" 0.;
         p_value_test "p-value for regression coefficient (2)" data "GP" "SLG"
           0.022;
         prediction_value_test "prediction value for data" data "GP" "SLG"
           "100." 0.97147055;
         (* stress tests with large (30k+) data *)
         get_columns_test "sample data columns test" stress_data
           [
             "ID";
             "Name";
             "Sex";
             "Age";
             "Height";
             "Weight";
             "Team";
             "NOC";
             "Games";
             "Year";
             "Season";
             "City";
             "Sport";
             "Event";
             "Medal";
           ];
         get_mean_col_test "stress data col mean test" stress_data "Age"
           25.42901162983334;
         get_std_col_test "stress data col std test" stress_data "Age"
           5.049684088225812;
         get_len_col_test "stress data col len test" stress_data "Age" 30181.;
         get_cov_test "stress data cov test" stress_data "Height" "Weight"
           131.43399128988258;
         get_cov_test "stress data cov test" stress_data "Height" "Weight"
           131.43399128988258;
         test_corr "stress data corr test" stress_data "Height" "Weight"
           0.801830825;
         test_median "stress data median test" stress_data "Height" 178.;
         test_mode_col "stress data mode test" stress_data "Age" [ "23" ];
         test_iqr "stress data iqr test" stress_data "Age"
           (13., 22., 25., 28., 66.);
         b0_test "stress data b0 test" stress_data "Height" "Weight" (-121.8946);
         b1_test "stress data b1 test" stress_data "Height" "Weight" 1.1013600;
         mae_test "stress data mae test" stress_data "Height" "Weight"
           6.313625032619486;
         mse_test "stress data mse test" stress_data "Height" "Weight"
           80.39096753751804;
         tss_test "stress data tss test" stress_data "Weight" 6795020.4313806705;
         ssr_test "stress data ssr test" stress_data "Height" "Weight"
           2426279.79125;
         r_squared_test "stress data b0 test" stress_data "Height" "Weight"
           0.6429326716892829;
         se_test "stress data b0 test" stress_data "Height" "Weight" 0.004724567;
         hist_bin_max_test "histogram max from Height column" stress_data
           "Height" 10;
         hist_bin_max_test "histogram max from Weight column" stress_data
           "Weight" 5;
         hist_bin_max_test "histogram max from Age column" stress_data "Age" 1;
         head_test "get head for Games column" stress_data "Games"
           [
             "2014 Winter";
             "1948 Summer";
             "1948 Summer";
             "1948 Summer";
             "1948 Summer";
           ];
         ci_test_lower "stress lower bound of confidence interval" stress_data
           "Height" "Weight" 1.0919109;
         ci_test_upper "stress upper bound of confidence interval" stress_data
           "Height" "Weight" 1.11080921;
         t_test "t-stat for regression coefficient" stress_data "Height"
           "Weight" 233.11343;
         prediction_value_test "stress value for regression coefficient"
           stress_data "Height" "Weight" "300." 208.5133;
         browse_test "browse first 5 elements of BA" column_data 0 5
           [ "0.403"; "0.378"; "0.362"; "0.36"; "0.358" ];
         browse_test "browse elements 10-15 of BA" column_data 10 5
           [ "0.32"; "0.319"; "0.319"; "0.318"; "0.315" ];
         browse_test "browse last 3 elements of BA" column_data 55 3
           [ "0.195"; "0.188"; "0.156" ];
         browse_test "browse all elements of simple data" column_data_simple 0
           10
           [ "1"; "2"; "1"; "2"; "3" ];
         browse_test "browse with empty result (start beyond length)"
           column_data 100 5 [];
         browse_test "browse with zero chunk size" column_data 5 0 [];
       ]
       @ [
           QCheck_runner.to_ounit2_test self_r_squared_test;
           QCheck_runner.to_ounit2_test intercept_test;
           QCheck_runner.to_ounit2_test q_scatter_test_x;
           QCheck_runner.to_ounit2_test q_scatter_test_y;
           QCheck_runner.to_ounit2_test q_heat_test;
           QCheck_runner.to_ounit2_test q_heatmap_test_x;
           QCheck_runner.to_ounit2_test q_box_and_whiskers_test_x;
           QCheck_runner.to_ounit2_test q_box_and_whiskers_test_y;
           QCheck_runner.to_ounit2_test q_box_and_whiskers_test_min;
           QCheck_runner.to_ounit2_test q_box_and_whiskers_test_max;
           QCheck_runner.to_ounit2_test q_hist_bin_max_test;
         ]

let _ = run_test_tt_main tests

open StataOCaml

(* helper function that splits a user inputted string to trimmed list*)
let input_to_list str =
  List.filter
    (fun x -> String.length x > 0)
    (List.map String.trim (String.split_on_char ' ' str))

(* [list_slice] returns sublist starting from the index to a certain number of
   elements*)
let rec list_slice lst start len =
  if start < 0 || len < 0 then failwith "list_slice"
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
    take len (drop start lst)

(* adds some empty characters to the start of a string so it reaches a given
   length*)
let string_formatter_prepend n str =
  try
    let len = String.length str in
    String.make (n - len) ' ' ^ str
  with Invalid_argument s -> str

let string_formatter_append n str =
  try
    let len = String.length str in
    String.make (n - len) ' ' ^ str
  with Invalid_argument s -> str

(* removes some trailing characters from the end of a string so it reaches a
   given length, if needed*)
let cutoff_string_formatter n str =
  try String.sub str 0 n
  with Invalid_argument _ -> string_formatter_append n str

(* round float to n decimal places *)
let roundfloatn flt n = Float.round (flt *. (10. ** n)) /. (10. ** n)

(* round float to 2 decimal places *)
let roundfloat2 n = roundfloatn 2. n

(* gets the length of the largest float in the list, when rounded to 2 decimal
   points *)
let get_max_string_len (lst : string list) =
  List.fold_left (fun acc x -> max acc (String.length x)) 0 lst

(* matches the heatmap values to print colors *)
let match_color (str : string) (max : float) =
  try
    let x = float_of_string (String.trim str) in
    if x <= max /. 4. then
      ANSITerminal.print_string
        [ ANSITerminal.white; ANSITerminal.on_black ]
        (" " ^ str)
    else if x <= 2. *. max /. 4. then
      ANSITerminal.print_string
        [ ANSITerminal.yellow; ANSITerminal.on_black ]
        (" " ^ str)
    else if x <= 3. *. max /. 4. then
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.on_black ]
        (" " ^ str)
    else
      ANSITerminal.print_string
        [ ANSITerminal.magenta; ANSITerminal.on_black ]
        (" " ^ str)
  with _ ->
    ANSITerminal.print_string [ ANSITerminal.default; ANSITerminal.default ] str

let rec get_input (command : string list) (data : Csv.t) =
  try
    (* match the command with various options *)
    match command with
    | "help" :: [] ->
        print_endline "Run any of the following commands:";
        print_endline "---------------------------------------";
        print_endline "columns : gives the columns in your dataset";
        print_endline "---------------------------------------";
        print_endline "mean [col] : gives the mean of the specified column";
        print_endline "---------------------------------------";
        print_endline "median [col] : gives the median of the specified column";
        print_endline "---------------------------------------";
        print_endline "mode [col] : gives the mode of the specified column";
        print_endline "---------------------------------------";
        print_endline
          "iqr [col] : gives the iqr percentiles of the specified column";
        print_endline "---------------------------------------";
        print_endline
          "std [col] : gives the sample standard deviation of the specified \
           column";
        print_endline "---------------------------------------";
        print_endline
          "len [col] : gives the number of observations in the specified column";
        print_endline "---------------------------------------";
        print_endline
          "head [col] : gives up to the first five elements in the column";
        print_endline "---------------------------------------";
        print_endline
          "browse [col]: displays the full column with options if over 100 rows";
        print_endline "---------------------------------------";
        print_endline
          "cov [col1] [col2] : gives the sample covaraince of the two \
           specified columns";
        print_endline "---------------------------------------";
        print_endline
          "corr [col1] [col2] : gives the correlation coefficient of the two \
           specified columns";
        print_endline "---------------------------------------";
        print_endline
          "regress [col1] [col2] : performs a linear regression on the \
           specified columns, [col1] being the dependent variable";
        print_endline "---------------------------------------";
        print_endline
          "predict [col1] [col2] [x]: predicts the corresponding y-hat value \
           for [x] based on the linear regression on the specified columns, \
           [col1] being the dependent variable";
        print_endline "---------------------------------------";
        print_endline
          "summary [col]: gives you a summary of key information like \
           observations, mean, standard deviation, min/max values. ";
        print_endline "---------------------------------------";
        print_endline
          "hist [col] [bins] : outputs a histogram of the column with a given \
           number of bins";
        print_endline "---------------------------------------";
        print_endline
          "scatter [col1] [col2] : outputs a scatterplot of the [col1] on the \
           x-axis and [col2] on the y-axis";
        print_endline "---------------------------------------";
        print_endline
          "heatmap [col1] [col2] : outputs a heatmap of the [col1] on the \
           x-axis and [col2] on the y-axis, in the form of a desnity plot with \
           colorings";
        print_endline "---------------------------------------";
        print_endline
          "boxwhisker [col1] : outputs a box-and-whiskers plot of the [col1]";
        print_endline "---------------------------------------";
        print_endline "quit : quits the program";
        print_endline "---------------------------------------";
        let input = read_line () in
        get_input (input_to_list input) data
    | "columns" :: [] ->
        print_endline "";
        print_endline "Columns are:";
        print_endline "-------------------";
        List.iter print_endline (Corestats.get_columns data);
        let input = read_line () in
        get_input (input_to_list input) data
    | "quit" :: [] -> print_endline "Quitting program..."
    | [ "head"; x ] ->
        print_endline "-------------------";
        List.iter print_endline (Corestats.head data x);
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "browse"; x ] ->
        print_endline "-------------------";
        let col_data = Corestats.get_column_by_name data x in
        let total_len = List.length col_data in
        let rec show_col lst shown =
          let remaining = List.length lst - shown in
          let next_chunk = min 50 remaining in
          list_slice lst shown next_chunk
          |> List.iteri (fun i val_ ->
                 Printf.printf "%4d: %s\n" (shown + i + 1) val_);
          if shown + next_chunk < List.length lst then (
            print_endline
              "\nType [more] to see 50 more rows, or [quit] to stop.";
            match String.trim (read_line ()) with
            | "more" -> show_col lst (shown + next_chunk)
            | _ -> ())
        in
        if total_len <= 50 then
          List.iteri (fun i v -> Printf.printf "%4d: %s\n" (i + 1) v) col_data
        else (
          list_slice col_data 0 15
          |> List.iteri (fun i v -> Printf.printf "%4d: %s\n" (i + 1) v);
          print_endline "     ...";
          list_slice col_data (total_len - 15) 15
          |> List.iteri (fun i v ->
                 Printf.printf "%4d: %s\n" (total_len - 15 + i + 1) v);
          print_endline
            "\n\
             Type [more] to see the full column in sections of 50 rows or \
             [quit] to exit browse.";
          match String.trim (read_line ()) with
          | "more" -> show_col col_data 0
          | _ -> ());
        print_endline "\nExiting browse mode.";
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "mean"; x ] ->
        print_endline "-------------------";
        print_endline (string_of_float (Corestats.get_mean_col data x));
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "median"; x ] ->
        print_endline "-------------------";
        print_endline (string_of_float (Corestats.median data x));
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "mode"; x ] ->
        print_endline "-------------------";
        let modes = Array.of_list (Corestats.mode data x) in
        for i = 0 to Array.length modes - 1 do
          print_endline modes.(i)
        done;
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "iqr"; x ] ->
        print_endline "-------------------";
        let a, b, c, d, e = Corestats.iqr data x in

        let len_a = String.length (string_of_float a) in
        let len_b = String.length (string_of_float b) in
        let len_c = String.length (string_of_float c) in
        let len_d = String.length (string_of_float d) in
        let len_e = String.length (string_of_float e) in

        print_endline
          (string_formatter_prepend len_a "     Min"
          ^ string_formatter_prepend len_b "     25%"
          ^ string_formatter_prepend len_c "     Median"
          ^ string_formatter_prepend len_d "     75%"
          ^ string_formatter_prepend len_e "     Max");

        print_endline
          "---------------------------------------------------------------------";

        print_endline
          (string_formatter_prepend 8 (string_of_float a)
          ^ string_formatter_prepend 8 (string_of_float b)
          ^ string_formatter_prepend 11 (string_of_float c)
          ^ string_formatter_prepend 8 (string_of_float d)
          ^ string_formatter_prepend 8 (string_of_float e));

        let input = read_line () in
        get_input (input_to_list input) data
    | [ "std"; x ] ->
        print_endline "-------------------";
        Printf.printf "%.5f\n" (Corestats.get_std_col data x);
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "len"; x ] ->
        print_endline "-------------------";
        print_endline
          (string_of_int (Float.to_int (Corestats.get_len_col data x)));
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "cov"; x; y ] ->
        print_endline "-------------------";
        Printf.printf "%.5f\n" (Corestats.get_cov_col1_col2 data x y);
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "corr"; x; y ] ->
        print_endline "-------------------";
        Printf.printf "%.5f\n" (Corestats.get_corr_col1_col2 data x y);
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "regress"; y; x ] ->
        print_endline "-------------------";
        let b1 =
          string_of_float (roundfloatn (Bivariateregression.b1 data x y) 4.)
        in
        let b0 =
          string_of_float (roundfloatn (Bivariateregression.b0 data x y) 4.)
        in
        let mae =
          string_of_float
            (roundfloatn (Bivariateregression.mean_absolute_error data x y) 4.)
        in
        let mse =
          string_of_float
            (roundfloatn (Bivariateregression.mean_squared_error data x y) 4.)
        in
        let r2 =
          string_of_float
            (roundfloatn (Bivariateregression.r_squared data x y) 4.)
        in
        let se =
          string_of_float
            (roundfloatn (Bivariateregression.standard_error data x y) 3.)
        in
        let lowci, highci = Bivariateregression.confidence_interval data x y in
        let t =
          string_of_float (roundfloatn (Bivariateregression.tvalue data x y) 3.)
        in
        let p =
          string_of_float (roundfloatn (Bivariateregression.pvalue data x y) 4.)
        in

        let lenb1 = String.length b1 in
        let lenb0 = String.length b0 in
        let lenmae = String.length mae in
        let lenmse = String.length mse in
        let lenr2 = String.length r2 in
        let lense = String.length se in
        let lenlowci = String.length (string_of_float (roundfloatn lowci 4.)) in
        let lenhighci =
          String.length (string_of_float (roundfloatn highci 4.))
        in
        let lent = String.length t in
        let lenp = String.length p in

        print_endline
          ("                                           "
          ^ string_formatter_prepend lenmae "        MAE"
          ^ string_formatter_prepend lenmse "        MSE"
          ^ string_formatter_prepend lenr2 "        R^2");

        print_endline
          ("                                           "
          ^ string_formatter_prepend 11 mae
          ^ string_formatter_prepend 11 mse
          ^ string_formatter_prepend 11 r2);

        print_endline
          "============================================================================";

        print_endline
          (cutoff_string_formatter 9 y
          ^ " |"
          ^ string_formatter_prepend (max lenb1 lenb0) "     Coef"
          ^ string_formatter_prepend lense "     Std. Err."
          ^ string_formatter_prepend lent "        T"
          ^ string_formatter_prepend lenp "     P>|T|"
          ^ string_formatter_prepend lenlowci "     [0.025"
          ^ string_formatter_prepend lenhighci "     0.975]");

        print_endline
          "----------------------------------------------------------------------------";

        print_endline
          (cutoff_string_formatter 9 x
          ^ " |"
          ^ string_formatter_prepend (max 9 lenb0) b1
          ^ string_formatter_prepend 14 se
          ^ string_formatter_prepend 9 t
          ^ string_formatter_prepend 10 p
          ^ string_formatter_prepend 11 (string_of_float (roundfloatn lowci 4.))
          ^ string_formatter_prepend 11
              (string_of_float (roundfloatn highci 4.)));

        let input = read_line () in
        get_input (input_to_list input) data
    | [ "predict"; y; x; i ] ->
        Printf.printf "%.5f\n" (Bivariateregression.predict data x y i);
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "summary"; x ] ->
        let obs = Int.to_string (Float.to_int (Corestats.get_len_col data x)) in
        let mean =
          Float.to_string (roundfloatn (Corestats.get_mean_col data x) 3.)
        in
        let std =
          Float.to_string (roundfloatn (Corestats.get_std_col data x) 3.)
        in
        let min = Float.to_string (roundfloatn (Corestats.min_col data x) 3.) in
        let max = Float.to_string (roundfloatn (Corestats.max_col data x) 3.) in

        let len_var = String.length x in
        let len_obs = String.length obs in
        let len_mean = String.length mean in
        let len_std = String.length std in
        let len_min = String.length min in
        let len_max = String.length max in

        print_endline
          (string_formatter_prepend len_var "     Variable"
          ^ "|"
          ^ string_formatter_prepend len_obs "     Obs"
          ^ string_formatter_prepend len_mean "     Mean"
          ^ string_formatter_prepend len_std "     Std. dev."
          ^ string_formatter_prepend len_min "     Min"
          ^ string_formatter_prepend len_max "     Max");

        print_endline
          "---------------------------------------------------------------------";

        print_endline
          (string_formatter_prepend 13 x
          ^ "|"
          ^ string_formatter_prepend 8 obs
          ^ string_formatter_prepend 9 mean
          ^ string_formatter_prepend 14 std
          ^ string_formatter_prepend 8 min
          ^ string_formatter_prepend 8 max);

        let input = read_line () in
        get_input (input_to_list input) data
    | [ "hist"; colname; bins ] ->
        let hist_bins =
          Visuals.histogram_bins data colname (int_of_string bins)
        in
        let hist_counts =
          Visuals.histogram_values data colname (int_of_string bins)
        in
        let hist =
          List.map
            (fun (lo, hi) -> Printf.sprintf "%.2f - %.2f|" lo hi)
            hist_bins
        in
        let max_len = get_max_string_len hist in
        let hist = List.map (string_formatter_prepend max_len) hist in
        let max_count =
          String.length
            (string_of_int
               (List.fold_left
                  (fun acc a -> max acc a)
                  (Float.to_int neg_infinity)
                  hist_counts))
        in
        let hist =
          List.map2
            (fun bin count ->
              bin
              ^ string_formatter_prepend (max_count + 5) (string_of_int count)
              ^ " | " ^ String.make count '#')
            hist hist_counts
        in
        print_endline
          (string_formatter_prepend (max_len - 1) colname
          ^ "|"
          ^ string_formatter_prepend (max_count + 5) "Freq."
          ^ " | ");
        print_endline "--------------------------------------";
        let output = String.concat "\n" hist in
        print_endline output;
        print_endline "--------------------------------------";
        print_endline
          (string_formatter_prepend (max_len - 1) "Total"
          ^ "|"
          ^ string_formatter_prepend (max_count + 5)
              (string_of_int
                 (Float.to_int (Corestats.get_len_col data colname)))
          ^ " | ");

        let input = read_line () in
        get_input (input_to_list input) data
    | [ "scatter"; x; y ] ->
        let plot = Visuals.scatter data x y in
        print_endline "-------------------";
        print_endline ("Scatter plot for " ^ x ^ " against " ^ y);
        for x = 0 to 21 do
          for y = 0 to 21 do
            print_string plot.(x).(y)
          done;
          print_endline ""
        done;
        print_endline ("X-axis: " ^ x);
        print_endline ("Y-axis: " ^ y);
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "heatmap"; x; y ] ->
        let plot, max = Visuals.heatmap data x y in
        print_endline "-------------------";
        print_endline ("Heatmap for " ^ x ^ " against " ^ y);
        for x = 0 to 11 do
          for y = 0 to 11 do
            match_color plot.(x).(y) max
          done;
          print_endline ""
        done;
        print_endline ("X-axis: " ^ x);
        print_endline ("Y-axis: " ^ y);
        let input = read_line () in
        get_input (input_to_list input) data
    | [ "boxwhisker"; x ] ->
        let plot = Visuals.box_and_whiskers data x in
        print_endline "-------------------";
        print_endline ("Box and whiskers plot for " ^ x ^ ":");

        for x = 0 to 12 do
          for y = 0 to 11 do
            print_string plot.(x).(y)
          done;
          print_endline ""
        done;
        let input = read_line () in
        get_input (input_to_list input) data
    (* unknown command *)
    | _ ->
        print_endline
          "Unknown command, please ensure your inputs are correct and try \
           again.";
        let input = read_line () in
        get_input (input_to_list input) data
  with
  (* check various raises clauses that may occur in process *)
  | Division_by_zero ->
      print_endline
        "A division by zero occured. Please check the data and try again.";
      let input = read_line () in
      get_input (input_to_list input) data
  | Corestats.ColumnNotFound ->
      print_endline
        "The column name was not found. Please check the data and try again.";
      let input = read_line () in
      get_input (input_to_list input) data
  | Corestats.InvalidColumn ->
      print_endline
        "A column is invalid (likely non-float values in the column). Please \
         check the data and try again.";
      let input = read_line () in
      get_input (input_to_list input) data
  | Invalid_argument str ->
      print_endline
        "Please check your input and try again. Ensure arguements are positive.";
      let input = read_line () in
      get_input (input_to_list input) data
  | Failure _ ->
      print_endline
        "Please check your input and try again. Ensure arguements are valid.";
      let input = read_line () in
      get_input (input_to_list input) data

let () =
  try
    (* if there is a csv provided *)
    if Array.length Sys.argv = 2 then (
      let data = Csv.load Sys.argv.(1) in
      print_endline ("Reading in CSV at " ^ Sys.argv.(1) ^ "...\nDone.");
      let _ =
        if not (Csv.is_square data) then (
          print_endline "Please enter a rectangular CSV.";
          failwith "rectangular")
        else if data = [] then (
          print_endline "Please enter a non-empty CSV.";
          failwith "empty")
      in
      print_endline
        "\nEnter a command or type [help] to see possible commands...";
      let input = read_line () in
      get_input (input_to_list input) data)
    (* if there is not a csv provided *)
      else
      print_endline
        "A correct number of arguments was not supplied (likely missing .csv \
         file). Please try again."
  with
  (* csv read-in error *)
  | Sys_error str -> print_endline "Unable to read file. Please try again."
  (* case when non-valid csv [rectangular, empty, etc.] *)
  | Failure _ -> print_endline "Exiting program."
  | _ ->
      (* catch all case *)
      print_endline "Invalid input. Please use the program correctly."

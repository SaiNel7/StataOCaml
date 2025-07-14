type t = string list list
(** The type of a CSV that has certain "rectangular" data, including column
    names. *)

exception ColumnNotFound
(** Raised when an operation is attempted on a column name that does not exist
    in the CSV. *)

exception InvalidColumn
(** Rasied when a numerical operation is attempted on a column that is not all
    numeric. *)

val get_columns : t -> string list
(** [get_columns t] is a list of columns in the CSV [t].*)

val get_column_by_name : t -> string -> string list
(** [get_column_by_name t n] returns the data stored in the column named [n] in
    table [t], excluding the column name*)

val col_operation : (string list -> float) -> t -> string -> float
(** [col_operation op t n] performs a list operation on a column [n] in table
    [t].*)

val get_col_index : t -> string -> int
(** [get_col_index t n] gets the index of the column in table [t] with name [n].
    Raises: [ColumnNotFound] if no column has name [n].*)

val head : t -> string -> string list
(** [head t n] is up to the first five elements of the column named [n] in table
    [t]. Raises: [ColumnNotFound] if no column has name [n].*)

val get_mean_col : t -> string -> float
(** [get_mean_col t c] is the arithmetic mean of the column [c] in the CSV [t].
    Raises: [ColumnNotFound] if column [c] is not present within [t],
    [InvalidColumn] if the column has non-numeric data, and [Division_by_zero]
    if column [c] does not have at least 1 observation. *)

val get_std_col : t -> string -> float
(** [get_std_col t c] is the sample standard deviation of the column [c] in the
    CSV [t]. Raises: [ColumnNotFound] if column [c] is not present within [t],
    [InvalidColumn] if the column has non-numeric data, and [Division_by_zero]
    if column [c] does not have at least 2 observations.*)

val get_cov_list : string list -> string list -> float
(** [get_cov_list lst1 lst2] is the sample covariance of the list [lst1] and
    [lst2]. Raises: [InvalidColumn] if [lst] contains strings that cannot be
    parsed to floats. [Division_by_zero] if the list length will result in a
    divison by 0 error.*)

val get_cov_col1_col2 : t -> string -> string -> float
(** [get_cov_col1_col2 t c1 c2] is the sample covaraince of the column [c1] and
    column [c2] in the CSV [t]. Raises: [ColumnNotFound] if column [c] is not
    present within [t], [InvalidColumn] if the column has non-numeric data, and
    [Division_by_zero] if column [c] does not have at least 2 observations.*)

val get_len_col : t -> string -> float
(** [get_len_col t c] is the length of the column [c] in the CSV [t]. Raises:
    [ColumnNotFound] if column [c] is not present within [t].*)

val max_col : t -> string -> float
(** [max_col t n] is the maximum value in the column named [n] in the table [t].
    Raises: [ColumnNotFound] if column [n] is not present within [t],
    [InvalidColumn] if the column has non-numeric data*)

val min_col : t -> string -> float
(** [min_col t n] is the minimum value in the column named [n] in the table [t].
    Raises: [ColumnNotFound] if column [n] is not present within [t],
    [InvalidColumn] if the column has non-numeric data*)

val median : t -> string -> float
(** [median t c] is the median of the column [c] in the CSV [t]. Raises:
    [ColumnNotFound] if column [c] is not present within [t], and
    [InvalidColumn] if the column has non-numeric data.*)

val mode : t -> string -> string list
(** [mode t c] is the mode (or modes) of the column [c] in the CSV [t]. Raises:
    [ColumnNotFound] if column [c] is not present within [t].*)

val iqr : t -> string -> float * float * float * float * float
(** [iqr t c] is the 0%, 25%, 50%, 75%, and 100% values of the column [c] in the
    CSV [t]. Raises: [ColumnNotFound] if column [c] is not present within [t],
    and [InvalidColumn] if the column has non-numeric data.*)

val get_corr_col1_col2 : t -> string -> string -> float
(** [get_corr_col1_col2 t c1 c2] is the correlation of the column [c1] and
    column [c2] in the CSV [t]. Raises: [ColumnNotFound] if column [c] is not
    present within [t], [InvalidColumn] if the column has non-numeric data, and
    [Division_by_zero] if column [c] does not have at least 2 observations. *)

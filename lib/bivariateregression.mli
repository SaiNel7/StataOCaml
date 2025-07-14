type t = string list list
(** The type of a CSV that has certain "rectangular" data, including column
    names. *)

val b0 : t -> string -> string -> float
(** [b0 t x y] is the intercept of the bivariate regression of column named [y]
    against [x] in the table [t]*)

val b1 : t -> string -> string -> float
(** [b0 t x y] is the coefficient of the bivariate regression of column named
    [y] against [x] in the table [t]*)

val mean_absolute_error : t -> string -> string -> float
(** [mean_absolute_error t x y] is the mean absolute error of the bivariate
    regression of column named [y] against [x] in the table [t]*)

val mean_squared_error : t -> string -> string -> float
(** [mean_squared_error t x y] is the mean squared error of the bivariate
    regression of column named [y] against [x] in the table [t]*)

val total_sum_of_squares : t -> string -> float
(** [total_sum_of_squares t y] is the total sum of squares of column named [y]
    in the table [t]*)

val sum_of_squared_residuals : t -> string -> string -> float
(** [sum_of_squared_residuals t x y] is the sum of squared residuals of the
    bivariate regression of column named [y] against [x] in the table [t]*)

val r_squared : t -> string -> string -> float
(** [r_squared t x y] is the R^2 value of the bivariate regression of column
    named [y] against [x] in the table [t]*)

val standard_error : t -> string -> string -> float
(** [standard_error t x y] is the standard error of the coefficient value in the
    bivariate regression of column named [y] against [x] in the table [t]*)

val tvalue : t -> string -> string -> float
(** [tvalue t x y] is the t-value of the coefficient value in the bivariate
    regression of column named [y] against [x] in the table [t]*)

val confidence_interval : t -> string -> string -> float * float
(** [confidence_interval t x y] is the 95% confidence interval pair of the
    coefficient value in the bivariate regression of column named [y] against
    [x] in the table [t]*)

val pvalue : t -> string -> string -> float
(** [pvalue t x y] is the 5% signifiance p-value of the coefficient value in the
    bivariate regression of column named [y] against [x] in the table [t]*)

val predict : t -> string -> string -> string -> float
(** [predict t x y i] is the predicted y-hat value for the input [i] in the
    bivariate regression of column named [y] against [x] in the table [t]*)

type t = string list list
(** The type of a CSV that has certain "rectangular" data, including column
    names. *)

val histogram_bins : t -> string -> int -> (float * float) list
(** [histogram_bins t n b] is a list of float tuples that correspond to the
    lower and upper bins for a histogram of column named [n] in table [t] with
    [b] bins. Raises: [Corestats.Invalid_argument] if [b] is not positive.*)

val histogram_values : t -> string -> int -> int list
(** [histogram_values t n b] is a list of counts of the number of values in a
    bin for a histogram of column named [n] in table [t] with [b] bins. Raises:
    [Invalid_argument] if [b] is not positive.*)

val scatter : t -> string -> string -> string array array
(** [scatter t c1 c2] is a 2D matrix (array of arrays) that represents a
    "pixelated" scatterplot of the data in [c1] (x) against the data in [c2]
    (y). Raises: [Invalid_argument] if [c1] or [c2] contains non-numeric data.*)

val heatmap : t -> string -> string -> string array array * float
(** [heatmap t c1 c2] is a 2D heatmap (array of arrays) that represents a
    "pixelated" heatmap of the data in [c1] (x) against the data in [c2] (y).
    The second value is the maximum value in the heatmap. Raises:
    [Invalid_argument] if [c1] or [c2] contains non-numeric data.*)

val box_and_whiskers : t -> string -> string array array
(** [box_and_whiskers t c1] is a 2D box and whiskers plot (array of arrays) that
    represents a "pixelated" box-and-whiskers plot of the data in [c1]. Raises:
    [Invalid_argument] if [c1] contains non-numeric data.*)

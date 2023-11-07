#' Create an S7 object of class "cases"
#'
#' @description
#' A `cases` object contains numbers of individuals, typically
#' representing a public health outcome. This can for example
#' represent the number of Persons Who Inject Drugs (PWID) or the number
#' of people living with type 2 Diabetes.
#'
#' @param numerator A non-negative [numeric] vector of numbers of individuals.
#' @param denominator A non-negative [numeric] vector of numbers of individuals.
#'
#' @seealso [suppress]
#'
#' @returns
#' An S7 object of class "cases".
#'
#' @examples
#' library(suppress)
#'
#' cases(10) # 10 cases
#' cases(10, 100) # 10 cases out of a possible 100
#'
#' numerator <- sample.int(10, 30, TRUE)
#' denominator <- 100
#'
#' cases(numerator, denominator)
#'
#' @export
cases <- S7::new_class("cases",
                       properties = list(
                         numerator = S7::class_numeric,
                         denominator = S7::class_numeric
                       ),
                       constructor = function(numerator = integer(),
                                              denominator = integer()){
                         out <- S7::new_object(S7::S7_object(),
                                               numerator = numerator,
                                               denominator = denominator)
                         num_len <- length(numerator)
                         den_len <- length(denominator)
                         if (num_len != 0 && den_len != 0){
                           if (num_len == 1){
                             S7::prop(out, "numerator") <-
                               rep_len(numerator, den_len)
                           }
                           if (den_len == 1){
                             S7::prop(out, "denominator") <-
                               rep_len(denominator, num_len)
                           }
                         }
                         out
                       },
                       validator = function(self) {
                         invalid_denominator <-
                           isTRUE(any(S7::prop(self, "numerator") >
                                        S7::prop(self, "denominator")))
                         valid_lengths  <- (
                           (
                             length(S7::prop(self, "numerator")) ==
                               length(S7::prop(self, "denominator"))
                           )
                           ||
                             (
                               length(S7::prop(self, "numerator")) == 1 ||
                                 length(S7::prop(self, "denominator")) == 1
                             )
                           ||
                             (
                               length(S7::prop(self, "numerator")) == 0 ||
                                 length(S7::prop(self, "denominator")) == 0
                             )
                         )
                           if (invalid_denominator){
                           "@numerator must be less than @denominator"
                         } else if (!valid_lengths){
                           "@numerator and @denominator must be of equal length, either of length zero, or either of length one"
                         }
                       }
)


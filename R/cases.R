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
#' denominator <- rep(100, 30)
#'
#' cases(numerator, denominator)
#'
#' @export
cases <- S7::new_class("cases",
                       properties = list(
                         numerator = S7::class_numeric,
                         denominator = S7::class_numeric
                       ),
                       validator = function(self) {
                           if (isTRUE(any(S7::prop(self, "numerator") >
                                        S7::prop(self, "denominator")))){
                           "@numerator must be less than @denominator"
                         } else if ((
                           length(S7::prop(self, "numerator")) !=
                             length(S7::prop(self, "denominator"))
                         ) && !(
                           length(S7::prop(self, "numerator")) == 0 ||
                           length(S7::prop(self, "denominator")) == 0
                         )){
                           "@numerator and @denominator must be of equal length or either of length zero"
                         }
                       }
)



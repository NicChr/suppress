#' Suppress sensitive numbers and percentages
#'
#' @returns
#' A data frame of suppressed numerator, denominator and percentage.
#'
#' @param x A cases object.
#' @param ... Additional arguments passed onto the 3 suppress functions:
#' `suppress_numerator`, `suppress_denominator`, and `suppress_percentage`.
#'
#' @seealso [suppress_numerator]
#'
#' @examples
#' library(suppress)
#'
#' # Create an S7 object of class sensitive
#' cases <- cases(0:10, rep(20, 11))
#'
#' suppress(cases)
#' @export
suppress <- S7::new_generic("suppress", "x")

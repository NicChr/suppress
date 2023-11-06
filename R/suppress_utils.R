#' Suppress sensitive numbers and percentages
#'
#' @returns
#' A vector the length of numerator or denominator, depending on which
#' method is called.
#'
#' @param x A cases object.
#' @param threshold A number such that any values less or equal to this are
#' regarded as sensitive and replaced with an appropriate value. \cr
#' The default is 5.
#' To set globally use `options(suppress.threshold)`
#' @param replace A value to replace numbers <= threshold. \cr
#' The default is `NA`. To set globally use `options(suppress.replace)`
#' @param digits Number of digits to round the percentages to. \cr
#' The default is 2. To set globally use `options(suppress.digits)`
#' @param ... Additional arguments.
#'
#' @seealso [suppress]
#'
#' @details
#' `dummy()` was created to temporarily satisfy the R CMD check. Don't use it.
#'
#' @examples
#' library(suppress)
#'
#' # Create an S7 object of class sensitive
#' cases <- cases(0:10, rep(20, 11))
#'
#' # Suppress numbers <= 5
#' suppress_numerator(cases)
#' suppress_numerator(cases, threshold = 7, replace = "*")
#' suppress_denominator(cases)
#'
#' ##### Reporting with  percentages #####
#'
#' # If we reported denominator and percentages
#' # We could easily calculate the underlying numerators
#'
#' p <- proportion(cases)
#'
#' # A user could calculate the numerator this way
#' p * denominator(cases)
#'
#' # Use suppress_percentage to
#' # ensure percentages are also suppressed
#' suppress_proportion(cases)
#' suppress_percentage(cases, replace = "*")
#'
#' # Finally
#'
#' secret_data <- data.frame(secret = numerator(cases),
#'                           denominator = denominator(cases),
#'                           percentage = percentage(cases))
#'
#' published_data <- data.frame(secret = suppress_numerator(cases, replace = "*"),
#'                              denominator = suppress_denominator(cases),
#'                              percentage = suppress_percentage(cases, replace = "*"))
#' secret_data
#' published_data
#' @rdname suppress_utils
#' @export
numerator <- S7::new_generic("numerator", "x")
#' @rdname suppress_utils
#' @export
denominator <- S7::new_generic("denominator", "x")
#' @rdname suppress_utils
#' @export
suppress_numerator <- S7::new_generic("suppress_numerator", "x")
#' @rdname suppress_utils
#' @export
suppress_denominator <- S7::new_generic("suppress_denominator", "x")
#' @rdname suppress_utils
#' @export
proportion <- S7::new_generic("proportion", "x")
#' @rdname suppress_utils
#' @export
percentage <- S7::new_generic("percentage", "x")
#' @rdname suppress_utils
#' @export
suppress_proportion <- S7::new_generic("suppress_proportion", "x")
#' @rdname suppress_utils
#' @export
suppress_percentage <- S7::new_generic("suppress_percentage", "x")

S7::method(numerator, cases) <- function(x) {
  S7::prop(x, "numerator")
}
S7::method(denominator, cases) <- function(x) {
  S7::prop(x, "denominator")
}
S7::method(suppress, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                        replace = getOption("suppress.replace", NA),
                                        digits = getOption("suppress.digits", 2)) {
  numerator <- suppress_numerator(x, threshold = threshold, replace = replace)
  denominator <- suppress_denominator(x, threshold = threshold, replace = replace)
  percentage <- suppress_percentage(x, threshold = threshold,
                                    replace = replace, digits = digits)
  numer_size <- length(numerator)
  denom_size <- length(denominator)
  percent_size <- length(percentage)
  all_length_zero <- max(numer_size, denom_size, percent_size) == 0
  args <- list()
  if (all_length_zero || numer_size > 0){
    args <- c(args, list(numerator = numerator))
  }
  if (all_length_zero || denom_size > 0){
    args <- c(args, list(denominator = denominator))
  }
  if (all_length_zero || percent_size > 0){
    args <- c(args, list(percentage = percentage))
  }
  do.call(data.frame, args)
}
S7::method(suppress_numerator, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                                  replace = getOption("suppress.replace", NA)) {
  out <- S7::prop(x, "numerator")
  out[out <= threshold] <- replace
  out
}
S7::method(suppress_denominator, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                                    replace = getOption("suppress.replace", NA)) {
  out <- S7::prop(x, "denominator")
  out[out <= threshold] <- replace
  out
}
S7::method(proportion, cases) <- function(x) {
  S7::prop(x, "numerator") / S7::prop(x, "denominator")
}
S7::method(percentage, cases) <- function(x, digits = getOption("suppress.digits", 2)) {
  out <- (S7::prop(x, "numerator") / S7::prop(x, "denominator")) * 100
  # Round halves up
  out <- trunc(abs(out) * 10^digits + 0.5 + sqrt(.Machine$double.eps)) / 10^digits *
    sign(as.numeric(out))
  stringr::str_c(out, "%")
}
S7::method(suppress_proportion, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                                   replace = getOption("suppress.replace", NA)) {
  numerator <- S7::prop(x, "numerator")
  denominator <- S7::prop(x, "denominator")
  out <- numerator / denominator
  out[(out * denominator) <= threshold] <- replace
  out
}
S7::method(suppress_percentage, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                                   replace = getOption("suppress.replace", NA),
                                                   digits = getOption("suppress.digits", 2)) {
  numerator <- S7::prop(x, "numerator")
  denominator <- S7::prop(x, "denominator")
  out <- numerator / denominator
  is_sensitive <- (out * denominator) <= threshold
  out <- out * 100
  out <- trunc(abs(out) * 10^digits + 0.5 + sqrt(.Machine$double.eps)) / 10^digits *
    sign(as.numeric(out))
  out <- stringr::str_c(out, "%")
  out[is_sensitive] <- replace
  out
}
#' @rdname suppress_utils
#' @export
dummy <- function(x, threshold = NULL, replace = NULL, digits = NULL){
  invisible(x)
}

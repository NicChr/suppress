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
#' When only 1 value is suppressed, secondary suppression is applied to
#' the next smallest unsuppressed value. \cr
#'
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
S7::method(suppress_numerator, S7::class_numeric) <- function(x,
                                                    threshold = getOption("suppress.threshold", 5),
                                                    replace = getOption("suppress.replace", NA),
                                                    digits = getOption("suppress.digits", 2)) {
  out <- x
  lte_threshold <- out <= threshold
  not_zero <- out != 0
  is_sensitive <- lte_threshold & not_zero
  out[is_sensitive] <- replace
  # Secondary suppression
  if (sum(is_sensitive, na.rm = TRUE) == 1 && length(out) > 1){
    out[match(min(out[!lte_threshold & not_zero], na.rm = TRUE), out)[1L]] <- replace
  }
  out

}
S7::method(suppress_numerator, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                                  replace = getOption("suppress.replace", NA)) {
  out <- S7::prop(x, "numerator")
  lte_threshold <- out <= threshold
  not_zero <- out != 0
  is_sensitive <- lte_threshold & not_zero
  out[is_sensitive] <- replace
  # Secondary suppression
  if (sum(is_sensitive, na.rm = TRUE) == 1 && length(out) > 1){
    out[match(min(out[!lte_threshold & not_zero], na.rm = TRUE), out)[1L]] <- replace
  }
  out
}
S7::method(suppress_denominator, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                                    replace = getOption("suppress.replace", NA)) {
  out <- S7::prop(x, "denominator")
  out[out <= threshold & out != 0] <- replace
  out
}
S7::method(proportion, cases) <- function(x) {
  S7::prop(x, "numerator") / S7::prop(x, "denominator")
}
S7::method(percentage, cases) <- function(x, digits = getOption("suppress.digits", 2)) {
  out <- (S7::prop(x, "numerator") / S7::prop(x, "denominator")) * 100
  # Round halves up
  out <- round2(out, digits = digits)
  stringr::str_c(out, "%")
}
S7::method(suppress_proportion, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                                   replace = getOption("suppress.replace", NA)) {
  numerator <- S7::prop(x, "numerator")
  denominator <- S7::prop(x, "denominator")
  out <- numerator / denominator
  numerator <- out * denominator
  lte_threshold <- numerator <= threshold
  not_zero <- numerator != 0
  is_sensitive <- lte_threshold & not_zero
  out[is_sensitive] <- replace
  if (sum(is_sensitive, na.rm = TRUE) == 1 && length(out) > 1){
    out[match(min(numerator[!lte_threshold & not_zero], na.rm = TRUE),
              numerator)[1L]] <- replace
  }
  out
}
S7::method(suppress_percentage, cases) <- function(x, threshold = getOption("suppress.threshold", 5),
                                                   replace = getOption("suppress.replace", NA),
                                                   digits = getOption("suppress.digits", 2)) {
  numerator <- S7::prop(x, "numerator")
  denominator <- S7::prop(x, "denominator")
  out <- numerator / denominator
  numerator <- out * denominator
  lte_threshold <- numerator <= threshold
  not_zero <- numerator != 0
  is_sensitive <- lte_threshold & not_zero
  out <- out * 100
  out <- round2(out, digits = digits)
  out <- stringr::str_c(out, "%")
  out[is_sensitive] <- replace
  # Secondary suppression
  if (sum(is_sensitive, na.rm = TRUE) == 1 && length(out) > 1){
    out[match(min(numerator[!lte_threshold & not_zero], na.rm = TRUE),
              numerator)[1L]] <- replace
  }
  out
}
#' @rdname suppress_utils
#' @export
dummy <- function(x, threshold = NULL, replace = NULL, digits = NULL){
  invisible(x)
}
# recycle_args <- function (...){
#   out <- list(...)
#   lens <- lengths(out, use.names = FALSE)
#   uniq_lens <- length(unique(lens))
#   recycle_length <- max(lens)
#   recycle_length <- recycle_length * (!any(lens == 0L))
#   recycle <- lens != recycle_length
#   out[recycle] <- lapply(out[recycle], function(x) rep_len(x, recycle_length))
#   out
# }

# Round halves up
round2 <- function(x, digits = 0){
  trunc(abs(x) * 10^digits + 0.5 + 1e-10) / 10^digits * sign(x)
}

.onLoad <- function(...) {
  S7::methods_register()
}
.onAttach <- function(...) {
  options(suppress.threshold = getOption("suppress.threshold", 5),
          suppress.replace = getOption("suppress.replace", NA),
          suppress.digits = getOption("suppress.digits", 2))
}

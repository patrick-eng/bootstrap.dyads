#' Call and Store Dyad-Ratios Functions
#'
#' Collects and assigns functions required to run the Stimson dyad-ratios model.
#'
#' @param filepath Character vector indicating the location and filename of the dyad-ratios functions. Defula is load_function.R
#'
#' @details Simple function to source and assign all dyad-ratios model functions into the global environment.
#'
#' Takes filepath argument, which by default looks in the working directory.
#'
#' Sigificant changes have been made to the extract function (non-calculation elements) which are documented in the main package documentation.
#'
#' @export call.dr.code

call.dr.code <- function(filepath="load_functions.R"){

  source(filepath)

}

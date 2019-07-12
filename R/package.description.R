#' @import rapportools
#' @import ggplot2
#'
#' @details This package provides a bootstrapped estimation for the variable `loading scores' estimated within the extract function of the R code for Professor James Stimson's dyad-ratios model.
#'
#' The bootstrap model removes a pre-defined proportion of random variables for each estimation, extracts the variable loading scores, and averages them across all trials.
#'
#' The bootstrapping deals with a potential problem in terms of item validity in which, on a single estimation of the model, each variable's validity is assumed to be equal to its common variance with the items around it. By randomising the items which appear around it in multiple replications, a much sterner test of the item's relationship with the latent concept is carried out which accounts for incidental, artefactual, and ultimately inaccurate reports of coveriance. It allows us to be more confident that the items selected actually, as individuals and the sum of their parts, connect to the latent concept estimated by the dyad-ratios calculator.
#'
#' Changes have been made to Stimson's original R code in order for the bootstrap model to be ran. This also requires some different approaches by the user. Most importantly, the data to be analysed needs to be specified as a seperate data and then character vector calls. E.g. data=data, varname="VARIABLE".
#'
#' Firstly, additional arguments have been added to the function itself including: data (define the object which holds the vectors), print (setting to FALSE allows the users to surpress all messages returned to the console), log (default is true, generates a log file containing diagnostic information onto the user's hard-drive), and filename (character vector for where the log should be written to, default is "dyad-ratios-log.txt" under the working directory).
#'
#' Secondly, code has been added to count the number of variables present within each identified period (days, quarters, years, or other), which is stored in the list object generated when the results of the function are assigned and also sent to the log.
#'
#' Thirdly, convergence estimates are now stored iteratively into a dataframe and reported as a group, rather than printed as text after each iteration. This is stored in the list object generated when the results of the function are assigned and also sent to the log.
#'
#' Finally, a bug in which quarterly estimations would fail if a given record held certain dates (e.g. 30th May in any year) has been patched.
#'
#' Note: the log is constructed using base R's sink function, and works best when specified as a .txt file.
#'
#' @keywords internal
#'
"_PACKAGE"
#> [1] "_PACKAGE"

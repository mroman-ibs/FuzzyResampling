#' A vector containing names of all resampling methods.
#'
#' @description
#' `resamplingMethods` is a vector containing names of all resampling methods.
#'
#' @return This function returns a vector of strings.
#'
#'
#' @examples
#'
#' # check the names of the available resampling methods
#'
#' resamplingMethods
#'
#' @export

resamplingMethods <- c("ClassicalBootstrap", "VAMethod", "EWMethod",
                       "VAFMethod", "DMethod", "WMethod")

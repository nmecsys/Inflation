#' @description
#'
#' @param
#' @return
#'
#' @keywords
#'
#' @import dymlm
#' @export
#' @examples
#'
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


infl.bias_test <- function(index,core){

    coeffs <- summary(dynlm(index ~ 1 + core))$coefficients
}

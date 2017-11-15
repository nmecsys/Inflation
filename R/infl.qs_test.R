#' @description
#'
#' @param
#' @return
#'
#' @keywords
#' @export
#' @examples
#'
#' @importFrom seasonal seas qs
#'
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


infl.qs_test <- function(index, core, nms = NULL){

    stats <- data.frame(matrix(ncol = 2, nrow = 2))

    indx.qs <- qs(seas(index))
    core.qs <- qs(seas(core))

    if(!is.null(nms)){
        rownames(stats) <- nms
    } else {
        rownames(stats) <- c("Index","Core")
    }

    colnames(stats) <- c("Statistic","P.Value")


}

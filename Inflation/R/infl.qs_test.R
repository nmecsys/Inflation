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

    suppressMessages(indx.qs <- qs(seas(index)))
    suppressMessages(core.qs <- qs(seas(core)))

    stats[1,] <- indx.qs[1,]
    stats[2,] <- core.qs[1,]

    if(!is.null(nms)){
        rownames(stats) <- nms
    } else {
        rownames(stats) <- c("Index","Core")
    }

    colnames(stats) <- c("Statistic","P.Value")


}

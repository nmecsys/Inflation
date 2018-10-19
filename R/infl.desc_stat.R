#' @description
#'
#' @param
#' @return
#'
#' @keywords
#'
#' @importFrom Metrics rmse
#' @importFrom forecast ma
#' @importFrom stats mean median frequency
#' @export
#' @examples
#'
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

infl.desc_stat <- function(index, core, nms = NULL, smooth.ord = 12){

    stats <- data.frame(matrix(nrow = 2, ncol = 4))

    stats[,1] <- c(mean(index),mean(core))
    stats[,2] <- c(median(index),median(core))
    stats[,3] <- c(sd(index),sd(core))

    se.indx <- rmse(index, na.omit(ma(index,smooth.ord)))
    se.core <- rmse(core, na.omit(ma(core,smooth.ord)))

    stats[,4] <- c(se.indx, se.core)

    if(!is.null(nms)){
        rownames(stats) <- nms
    } else {
        rownames(stats) <- c("Index","Core")
    }

    colnames(stats) <- c("Mean","Median","Std.Div","RMSE")

    return(stats)
}

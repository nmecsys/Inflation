 #' Computes the core inflation using the subitem exclusion method
 #'
#' @param subits.var A \code{ts}. Inflation subitems' variation.
#' @param weights A \code{ts}. Each subitem corresponding weights. If missing, all items get the same weight.
#' @param info A \code{data.frame}. Subitem metadata table containing their codes and descriptions.
#' @param n.blocks An \code{integer}. Partitions' number inside the temporal window.
#' @param alpha An \code{integer}. Significance level in percentage.
#' @keywords core exclusion
#' @encoding utf8
#' @export
#' @examples
#' \dontrun{
#' ipca <- Inflation::ipca_sub
#' ipc.ex1 <- Inflation::INFL.core_ex(subits.var = ipca$ipca_ts,
#'                                    weights = ipca$weights_ts,
#'                                    info = ipca$cod,
#'                                    n.blocks = 4,
#'                                    alpha = 2)
#' }
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}, Fernando Teixeira \email{fernando.teixeira@fgv.br}, Daiane Marcolino \email{daiane.marcolino@fgv.br}

INFL.core_ex <- function(subits.var, weights, info, n.blocks = 4, alpha = 2){

    # Compute the volatility matrix
    vol.m <- vol.mat(subits.var, info, n.blocks, alpha)

    # Get the codes of the items that must be removed
    to.rm <- rownames(vol.m[vol.m$SAI1 | vol.m$SAI2, ])

    # The weights of excluded items are set to zero
    weights[,colnames(weights) %in% to.rm] <- 0

    # Weights are rebalanced
    weights <- (weights/rowSums(weights, na.rm = TRUE))*100

    # subits.var variation is multiplied by their weights
    ts(rowSums(subits.var*weights, na.rm = TRUE)/100, start = start(subits.var), frequency = 12)
}


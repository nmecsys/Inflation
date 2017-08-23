 #' Computes the core inflation using the subitem exclusion method
 #'
#' @param subitems A \code{ts}. Subitems' variation.
#' @param weights A \code{ts}. Each subitem corresponding weights. If missing, all items get the same weight.
#' @param info A \code{data.frame}. Subitem metadata table containing their codes and descriptions.
#' @param part An \code{integer}. Partitions' number inside the temporal window.
#' @param alpha An \code{integer}. Significance level in percentage.
#' @keywords core exclusion
#' @encoding utf8
#' @export

INFL.core_ex <- function(subitems, weights, info, n.blocks = 4, alpha = 2){

    # Compute the volatility matrix
    vol.m <- vol.mat(subitems, info, n.blocks, alpha)

    # Get the codes of the items that must be removed
    to.rm <- rownames(vol.m[vol.m$SAI1 | vol.m$SAI2, ])

    # The weights of excluded items are set to zero
    weights[,colnames(weights) %in% to.rm] <- 0

    # Weights are rebalanced
    weights <- (weights/rowSums(weights, na.rm = TRUE))*100

    # Subitems variation is multiplied by their weights
    ts(rowSums(subitems*weights, na.rm = TRUE)/100, start = start(subitems), frequency = 12)
}


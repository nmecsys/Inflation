#' Computes the trimmed means core inflation
#'
#' @param subits.var A \code{ts}. Subitems' variation.
#' @param weights A \code{ts}. Each subitem corresponding weights. If missing, all items get the same weight.
#' @param inf An \code{integer}. Percentage lower tail cut. Predefined as 20.
#' @param sup An \code{integer}. Percentage upper tail cut. Predefined as 20.
#' @param smoo A \code{vector}. List of codes to be smoothed. If missing, no item will be smoothed.
#' @param wind An \code{integer}. The volatility's window size.
#'
#' @return A \code{list} object. The list contains two time-series (\code{ts} objects). The computed core
#' and the variables that were used to calculate the means.
#'
#' @keywords core trimmed mean average moving
#' @export
#' @examples
#' ipca_sub <- Inflation::ipca_sub
#' nuc <- Inflation::INFL.core_tm(subits.var = ipca_sub$ipca_ts, weights = ipca_sub$weights_ts)


INFL.core_tm <- function(subits.var, weights, smoo, inf = 20, sup = 20, wind = 12){

    if (missing(subits.var)){
        stop("Specify the subitems to be used in computations.")
    }

    # If weights are not provided, set all weights to
    # 100 / (number of subitems), so that their sum is 100
    if (missing(weights)){
        w <- matrix(100/ncol(subits.var), nrow = dim(subits.var)[1], ncol = dim(subits.var)[2])
        colnames(w) <- colnames(subits.var)
    }

    # Create a dim(weights) matrix
    new.weights <- weights*0

    if(!missing(smoo)){

        # Get items to be smoothed
        smoo <- paste0("cod_",smoo)
        subits.smoo <- subits.var[,colnames(subits.var) %in% smoo]

        # Transform percentages in multipliers
        # Take 'wind'th root of these multipliers
        subits.smoo <- (subits.smoo/100 + 1)^(1/wind)

        # Smoothing process
        # At each step, move the 'wind'-sized window and multiply elements in it
        filt <- subits.smoo
        for(i in wind:nrow(subits.smoo)){
            mults <- apply(subits.smoo[(i-wind+1):i,], MARGIN = 2, FUN = prod)
            masc <- !is.na(mults)
            filt[i,masc] <- mults[masc]
        }

        # Transform multipliers back in percentages
        filt <- (filt-1)*100

        # Replace series for its smoothed values
        subits.var[,colnames(subits.var) %in% smoo] <- filt
    }

    # Create a dim(subits.var) mts
    chosen <- subits.var*NA

    for(i in 1:nrow(subits.var)){

        data <- data.frame(x = subits.var[i,], weights = weights[i,])
        # Order values by variation
        data <- data[order(data$x),]
        # Compute the cumulative sums of the weights
        data$cum.weights <- cumsum(data$weights)

        # Calculate the position of the item before which elements must be removed
        pos.inf <- min(which(data$cum.weights > inf))
        # Calculate the position of the item after which elments must be removed
        pos.sup <- min(which(data$cum.weights > 100-sup))

        # dados_aux1 <- data[1:(pos.inf-1),]
        # dados_aux3 <- data[(pos.inf+1):(pos.sup-1),]
        # dados_aux5 <- data[(pos.sup+1):nrow(data),]
        #
        # peso_sai_inf <- inf - data[pos.inf - 1, "cum.weights"]
        # peso_entra_inf <- data[pos.inf, "cum.weights"] - inf
        # peso_entra_sup <- (100-sup) - data[pos.sup-1, "cum.weights"]
        # peso_sai_sup <- data[pos.sup, "cum.weights"] - (100-sup)
        #
        # dados_aux2 <- rbind(data[pos.inf,],data[pos.inf,])
        # dados_aux4 <- rbind(data[pos.sup,],data[pos.sup,])
        #
        # dados_aux2$weights <- c(peso_sai_inf, peso_entra_inf)
        # dados_aux4$weights <- c(peso_entra_sup, peso_sai_sup)
        #
        # rownames(dados_aux2) <-  c(paste0(rownames(data[pos.inf,]),"_sai"),
        #                            rownames(data[pos.inf,]))
        #
        # rownames(dados_aux4) <-  c(rownames(data[pos.sup,]),
        #                            paste0(rownames(data[pos.sup,]),"_sai"))
        #
        # dados_aux <- rbind(dados_aux1, dados_aux2,
        #                    dados_aux3, dados_aux4, dados_aux5)
        # dados_aux$cum.weights <- cumsum(dados_aux$weights)
        # dados_aux$conta <- 0
        #
        #
        # dados_aux <- dados_aux[!is.na(dados_aux$x),]
        #
        # dados_aux[dados_aux$cum.weights > inf &
        # dados_aux$cum.weights < 100 - sup |
        # dados_aux$cum.weights == as.character(100 - sup), "conta"] <- 1

        data[pos.inf, "weights"] <- data[pos.inf, "cum.weights"] - inf
        data[pos.sup-1, "weights"] <- (100-sup) - data[pos.sup-1, "cum.weights"]
        data <- na.omit(data[data$cum.weights > inf & data$cum.weights < 100 - sup,])

        chosen[i,rownames(data)] <- t(data$x)

        for(j in rownames(data)){
            new.weights[i,j] <- data[j,"weights"]
        }

    }

    new.weights <- new.weights/rowSums(new.weights,na.rm = T)
    core <- ts(rowSums(new.weights*subits.var, na.rm = T), start = start(subits.var), frequency = 12)

    return(list(core = core, chosen = chosen))
}


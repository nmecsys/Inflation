#' @title Computes the double weighting core inflation
#'
#' @param infl.var A \code{ts} object. The inflation index variation.
#' @param subit.var A \code{ts}. Subitems' variation.
#' @param weights A \code{ts}. Weights corresponding to each subitem.
#' @param wind An \code{integer}. The volatility's window size to be computed.
#'
#' @return A \code{ts} object.
#' @keywords core weight
#' @export

INFL.core_dw <- function(infl.var, subit.var, weights, wind = 48){


    # browser()
    pesos <- weights
    pi_rel <- subit.var - infl.var
    colnames(pi_rel) <- colnames(subit.var)

    data <- as.Date(paste0(start(pi_rel)[1], "-", start(pi_rel)[2],"-01"))
    data <- seq(data, by = "1 month", length.out = nrow(pi_rel))
    novos_pesos <- sub*0

    for(t in nrow(pi_rel):wind){

        ano1 <- as.numeric(substr(data[t-wind+1],1,4))
        mes1 <- as.numeric(substr(data[t-wind+1],6,7))

        ano2 <- as.numeric(substr(data[t],1,4))
        mes2 <- as.numeric(substr(data[t],6,7))

        sub_pi_rel <- window(pi_rel, start = c(ano1,mes1), end = c(ano2,mes2), frequency = 12)
        desvios <- apply(sub_pi_rel, MARGIN = 2, FUN = sd, na.rm = T)
        novos_pesos[t,] <- ((1/desvios)/sum(1/desvios, na.rm = T))*pesos[t,]
    }

    novos_pesos2 <- window(novos_pesos, start = c(as.numeric(substr(data[wind+1],1,4)), as.numeric(substr(data[wind+1],6,7))),
                           frequency = 12)
    novos_pesos3 <- novos_pesos2/rowSums(novos_pesos2,na.rm = T)
    sub2 <- window(sub, start = start(novos_pesos2), frequency = 12)
    core <- ts(rowSums(novos_pesos3*sub2, na.rm = T), start = start(sub2), frequency = 12)
    core
}



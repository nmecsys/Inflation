 #' Computes the core inflation using the subitem exclusion method
#' @param sub A \code{ts}. Subitems' variation.
#' @param weights A \code{ts}. Each subitem corresponding weights. If missing, all items get the
#' same weight.
#' @param codes A \code{data.frame}. Subitem containing the codes and their descriptions.
#' @param part An \code{integer}. Partitions' number inside the temporal window.
#' @param alpha An \code{integer}. Significance level in percentage.
#' @keywords core exclusion
#' @encoding utf8
#' @export
#' @examples
#' \dontrun{
#' load("~/INFLATION/data/codigos.rda")
#' load("~/INFLATION/data/pesos.ts.rda")
#' load("~/INFLATION/data/variacao.ts.rda")
#' codes <- codigos[2:nrow(codigos),]
#'
#' ipc.ex1 <- core.ex(sub = variacao.ts$subitens, weights = pesos.ts$subitens, codes = codigos,
#' part = 4, alpha = 2)
#'
#' # ex2: excluindo alimentação
#' novos_pesos <- pesos.ts$subitens
#' novos_pesos[,substr(colnames(novos_pesos), 5,5) == 1] <- 0
#' novos_pesos <- (novos_pesos/rowSums(novos_pesos, na.rm = TRUE))*100
#' ipc.ex3 <- ts(rowSums(variacao.ts$subitens*novos_pesos,
#' na.rm = TRUE)/100, start = start(variacao.ts$subitens), frequency = 12)
#'
#'
#' # gráficos
#' ts.plot(variacao.ts$ipc, ipc.ex1, col = c(1:2), lwd = 1:2)
#' ts.plot(variacao.ts$ipc, ipc.ex3, col = c(1:2), lwd = 1:2)
#'
#' ipca <- ipca_get(group = "subitem")
#' # Estes códigos ainda são do IPC - mudar para IPCA
#' load("~/INFLATION/data/codigos.rda")
#' codigos <- codigos[1:373,]
#' nuc <- core.ex(sub = ipca$ipca_ts, weights = ipca$weights_ts, codes = ipca$cod)
#'
#' }


core.ex <- function(sub, weights, codes, part = 4, alpha = 2){

    sd <- vola(x = sub, codes, part, alpha)

    # sd: saida de vola1999 ou vola
    saem <- rownames(sd[sd$SAI1 | sd$SAI2, ])
    weights[,colnames(weights) %in% saem] <- 0
    weights <- (weights/rowSums(weights, na.rm = TRUE))*100

    ts(rowSums(sub*weights, na.rm = TRUE)/100, start = start(sub), frequency = 12)
}


# core.ex(sub=a$ipca_ts, weights = a$weights_ts, codes = a$codigo)


#' Computes the volatility in the core by exclusion
#' @param x Subitems' variation.
#' @param codes Subitems' codes.
#' @param cortes Number of cuts to be made.
#' @param alpha Significance level.
#' @encoding utf8
#' @importFrom stats end sd start time ts window is.mts na.omit

vola <- function(x, codes, cortes,alpha){
    # x: mts

    if(! stats::is.mts(x)){stop("O argumento x deve ser uma 'mts'.")}

    pedaco = ceiling(dim(x)[1]/cortes)
    ped = pedaco

    corte = seq(cortes)
    desvio = mapply(paste0, "desvio_", corte, USE.NAMES = FALSE)
    corte = mapply(paste0, "x_", corte, USE.NAMES = FALSE)
    freq = stats::frequency(x)
    t1 = time(x)[1]
    t2 = time(x)[1+pedaco]


    for (i in seq_along(corte)){

        # Parte das janelas baseadas nos cortes
        x_star <- window(x, start = t1, end = t2, freq = freq)

        assign(corte[i],x_star)


        t1 = time(x)[1+pedaco+1]
        pedaco = pedaco + ped + 1
        t2 = time(x)[1+pedaco]
        if (is.na(t2)){t2 = end(x)}


        # Parte do cálculo dos desvios parciais
        desvios <- apply(x_star, MARGIN = 2, FUN = sd, na.rm = T)
        desvios <- (desvios - mean(desvios, na.rm = T))/sd(desvios, na.rm = T)
        assign(desvio[i],desvios)

        rm(x_star)

    }

    # Calculando o desvio total de x
    desvio_total <- apply(x, MARGIN = 2, FUN = sd, na.rm = T)
    desvios <- (desvio_total - mean(desvio_total, na.rm = T))/sd(desvio_total, na.rm = T)


    # browser()

    vola <- data.frame(matrix(NA, ncol = 2 + cortes, nrow = ncol(x)))
    rownames(vola) <- colnames(x)
    desvio = c("Descricao", "desvio_total", desvio)
    colnames(vola) <- desvio
    vola$Descricao <- codes


    vola[,2] = desvios

    for (i in 3:(length(corte)+2)){

        vola[,i] = eval(as.symbol(paste0("desvio", "_", (i-2))))

    }


    masc_total <- abs(vola[,2]) > alpha
    masc <- abs(vola[,3:length(desvio)]) > alpha


    vola$SAI1 <- masc_total
    vola$SAI2 <- rowSums(masc, na.rm = T) > 1


    vola <- vola[order(vola$desvio_total, decreasing = T),]
    vola
}

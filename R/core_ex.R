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

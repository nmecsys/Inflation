 #' Núcleo de inflação por exclusão
#' @param sub Subitens em variação.
#' @param pesos Pesos correspondentes a cada subitem.
#' @param codigos Codigos dos subitens
#' @param cortes Quantidade deseja de cortes a serem realizados na janela temporal.
#' @param alpha Nível de significância.
#' @keywords core
#' @encoding utf8
#' @export
#' @examples 
#' ipc.ex1 <- core.ex(variacao.ts$subitens, pesos.ts$subitens)
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


core.ex <- function(sub, pesos, codigos, cortes = 4, alpha = 2){
    
    sd <- vola(x = sub, codigos, cortes, alpha)
    
    # sd: saida de vola1999 ou vola
    saem <- rownames(sd[sd$SAI1 | sd$SAI2, ])
    pesos[,colnames(pesos) %in% saem] <- 0
    pesos <- (pesos/rowSums(pesos, na.rm = TRUE))*100
    
    ts(rowSums(sub*pesos, na.rm = TRUE)/100, start = start(sub), frequency = 12)
}
#' A 
#' @param infla Índice de inflação (variação).
#' @param sub Subitens em variação.
#' @param pesos Pesos correspondentes a cada subitem.
#' @param janela Janela para o cálculo da volatilidade
#' @keywords core
#' @export
#' @examples 
#' 

core.dp <- function(infla, sub, pesos, janela = 48){

    
     # browser()
    pi_rel <- sub - infla
    colnames(pi_rel) <- colnames(sub)
    
    data <- as.Date(paste0(start(pi_rel)[1], "-", start(pi_rel)[2],"-01"))
    data <- seq(data, by = "1 month", length.out = nrow(pi_rel))
    novos_pesos <- sub*0
    
    for(t in nrow(pi_rel):janela){
        
        ano1 <- as.numeric(substr(data[t-janela+1],1,4))
        mes1 <- as.numeric(substr(data[t-janela+1],6,7))
        
        ano2 <- as.numeric(substr(data[t],1,4))
        mes2 <- as.numeric(substr(data[t],6,7))
        
        sub_pi_rel <- window(pi_rel, start = c(ano1,mes1), end = c(ano2,mes2), frequency = 12)
        desvios <- apply(sub_pi_rel, MARGIN = 2, FUN = sd, na.rm = T)
        novos_pesos[t,] <- ((1/desvios)/sum(1/desvios, na.rm = T))*pesos[t,]
    }
    
    novos_pesos2 <- window(novos_pesos, start = c(as.numeric(substr(data[janela+1],1,4)), as.numeric(substr(data[janela+1],6,7))),
                           frequency = 12)
    novos_pesos3 <- novos_pesos2/rowSums(novos_pesos2,na.rm = T)
    sub2 <- window(sub, start = start(novos_pesos2), frequency = 12)
    core <- ts(rowSums(novos_pesos3*sub2, na.rm = T), start = start(sub2), frequency = 12)
    core
}
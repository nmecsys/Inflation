#' Calcula a volatilidade para núcleo por exclusão 
#' @param x Subitens em variação.
#' @param codigos Codigos dos subitens
#' @param cortes Quantidade deseja de cortes a serem realizados na janela temporal.
#' @param alpha Nível de significância.
#' @encoding utf8
#' @importFrom stats end sd start time ts window is.mts na.omit

vola <- function(x, codigos, cortes,alpha){
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
    vola$Descricao <- codigos

    
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
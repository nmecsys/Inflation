# Acumulado nos últimos 12 meses
#' @export
acum12 <- function(x){
    # x: ts
    if (! is.mts(x)){ n <- length(x); } else{ n <- length(x[,1]); m <- length(x[1,])}


    data <- data.frame(x)
    data2 <- data/100 + 1
    data <- data[13:n,]


    #data2$w <- rep(NA, n)
    #data2$z <- rep(NA, n)


    for(i in n:13){
        data[(i-12),] <-  data2[i,]*data2[i-1,]*data2[i-2,]*
            data2[i-3,]*data2[i-4,]*data2[i-5,]*data2[i-6,]*
            data2[i-7,]*data2[i-8,]*data2[i-9,]*data2[i-10,]*data2[i-11,]
    }




    data$z <- (data$w - 1)*100
    st <- ts(data$z, start = start(x), end = end(x), freq = 12)
    st
}



#' Acumulado no ano
#' @export
acumano <- function(x){
    inicio <- start(x)
    fim <- end(x)
    inicio_ok <- c(inicio[1],1)
    fim_ok <- c(fim[1],12)
    x_completo <- ts(NA, start = inicio_ok, end = fim_ok, freq = 12)
    dados <- data.frame(cbind(x,x_completo))
    matriz <- matrix(dados[,1], ncol = 12, byrow = T)
    matriz <- (matriz/100 + 1)
    prod <- (apply(matriz, MARGIN = 1, FUN = cumprod) - 1)*100
    acum <- ts(matrix(prod, ncol = 1), start = start(x_completo), freq = 12)
    acum
}


#' Anualizado
#' @export
anual <- function(x){
    # x: ts
    ((x/100+1)^12-1)*100
}


# Relativo
#' @export
relativo <- function(x){
    x/100+1
}

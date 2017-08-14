# Acumulado nos últimos 12 meses
#' @export
acum12 <- function(x){
    # x: ts
    if (! is.mts(x)){ n <- length(x); m <- 1
                      dim(x) <- c(length(x),1);
    } else{ n <- nrow(x); m <- ncol(x)}


    data <- x
    data2 <- data/100 + 1
    data <- ts(as.matrix(data[13:n,]),
               start = c(start(x)[1],start(x)[2]), frequency = frequency(x))

    #browser()

    data=INFLATION::acum(data = data, data2 = data2 , n = n, m=m)


    data <- tibble::as_data_frame(data)

    data <- (data - 1)*100
    st <- ts(data, start = start(x), end = (end(x) - c(1,0)), freq = 12)
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


#' Relativo
#' @export
relativo <- function(x){
    x/100+1
}


#' Trimestral
#' @export
geom3 <- function(x, anual = F){
    # x: ts
    n <- length(x)
    data <- data.frame(x, y = rep(NA, n), w = rep(NA, n), z = rep(NA, n), row.names = 1:n)
    colnames(data)[1] <- "x"
    data$y <- data$x/100 + 1

    for(i in n:3){
        data[i,"w"] <-  data$y[i]*data$y[i-1]*data$y[i-2]
    }
    data[,"w"] <- data[,"w"]^(1/3)
    if(anual){data[,"w"] <- data[,"w"]^12}
    data$z <- (data$w - 1)*100
    st <- ts(data$z, start = start(x), end = end(x), freq = 12)
    st
}

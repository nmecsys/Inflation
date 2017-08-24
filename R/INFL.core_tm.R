#' Computes the trimmed means core inflation
#'
#' @param sub A \code{ts}. Subitems' variation.
#' @param weights A \code{ts}. Each subitem corresponding weights. If missing, all items get thevsame weight.
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

core.tm <- function(subits.var, weights, smoo, inf = 20, sup = 20, wind = 12){

    if (missing(subits.var)){
        stop("Specify the subitems to be used in computations.")
    }

    if (missing(weights)){

        weights <- subits.var
        peso <- 100/length(subits.var[,1])
        weights <- sapply(weights, FUN = function(x){peso})
        weights <- matrix(peso, nrow = dim(subits.var)[1], ncol = dim(subits.var)[2])
        colnames(weights) <- colnames(subits.var)

    }

    weights_novos <- weights*0

    # browser()

    if(!missing(smoo)){

        # browser()

        # itens que serão suavizados
        codigos_smoo <- smoo
        codigos_smoo <- paste0("cod_",codigos_smoo)
        filtro_smoo <- subits.var[,colnames(subits.var) %in% codigos_smoo]
        filtro_smoo <- (filtro_smoo/100 + 1)^(1/wind)
        filtro_smoo2 <- filtro_smoo


        # processo de suavização
        for(i in wind:nrow(filtro_smoo)){
            v <- apply(filtro_smoo[(i-wind+1):i,], MARGIN = 2, FUN = prod)
            masc_v <- !is.na(v)
            filtro_smoo2[i,masc_v] <- v[masc_v]
        }


        filtro_smoo2 <- (filtro_smoo2-1)*100
        subits.var[,colnames(subits.var) %in% codigos_smoo] <- filtro_smoo2
    }


    entrou <- subits.var*NA
    entrou_vari <- NULL

    for(i in 1:nrow(subits.var)){



        dados <- data.frame(x = subits.var[i,], weights = weights[i,])
        dados <- dados[order(dados$x),]
        dados$weights_acum <- cumsum(dados$weights)
        pos_inf <- min(which(dados$weights_acum > inf))
        pos_sup <- min(which(dados$weights_acum > 100-sup))

        dados_aux1 <- dados[1:(pos_inf-1),]
        dados_aux3 <- dados[(pos_inf+1):(pos_sup-1),]
        dados_aux5 <- dados[(pos_sup+1):nrow(dados),]

        peso_sai_inf <- inf - dados[pos_inf-1, "weights_acum"]
        peso_entra_inf <- dados[pos_inf, "weights_acum"] - inf
        peso_entra_sup <- (100-sup) - dados[pos_sup-1, "weights_acum"]
        peso_sai_sup <- dados[pos_sup, "weights_acum"] - (100-sup)

        dados_aux2 <- rbind(dados[pos_inf,],dados[pos_inf,])
        dados_aux4 <- rbind(dados[pos_inf,],dados[pos_inf,])

        dados_aux2$weights <- c(peso_sai_inf, peso_entra_inf)
        dados_aux4$weights <- c(peso_entra_sup, peso_sai_sup)

        rownames(dados_aux2) <-  c(paste0(rownames(dados[pos_inf,]),"_sai"),
                                   rownames(dados[pos_inf,]))

        rownames(dados_aux4) <-  c(rownames(dados[pos_sup,]),
                                   paste0(rownames(dados[pos_sup,]),"_sai"))

        dados_aux <- rbind(dados_aux1, dados_aux2,
                           dados_aux3, dados_aux4, dados_aux5)
        dados_aux$weights_acum <- cumsum(dados_aux$weights)
        dados_aux$conta <- 0

        # browser()

        dados_aux <- dados_aux[!is.na(dados_aux$x),]

        dados_aux[dados_aux$weights_acum > inf &
                      dados_aux$weights_acum < 100 - sup |
                      dados_aux$weights_acum == as.character(100 - sup), "conta"] <- 1

        entrou_vari <- subset(dados_aux, subset = dados_aux$conta == 1)
        entrou[i,rownames(entrou_vari)] <- t(entrou_vari$x)

        for(j in rownames(subset(dados_aux, dados_aux$conta == 1))){
            weights_novos[i,j] <- dados_aux[j,"weights"]
        }

    }

    weights_novos2 <- weights_novos/rowSums(weights_novos,na.rm = T)*100

    core <- ts(rowSums(weights_novos2*subits.var, na.rm = T)/100, start = start(subits.var), frequency = 12)

    return(invisible(list(core = core, var_in = entrou)))
}


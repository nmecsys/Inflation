#' A
#' @param sub Subitens em variação.
#' @param pesos Pesos correspondentes a cada subitem.
#' @param inf Corte percentual da cauda inferior. Predefinido como 20.
#' @param sup Corte percentual da cauda superior. Predefinido como 20.
#' @param suave Código dos itens que serão suavizados.
#' @param janela Tamanho da janela temporal de corte.
#' @keywords core ma média móvel
#' @export
#' @examples
#' \dontrun{
#'
#' load("~/INFLATION/data/codigos.rda")
#' load("~/INFLATION/data/pesos.ts.rda")
#' load("~/INFLATION/data/variacao.ts.rda")
#' codigos <- codigos[2:nrow(codigos),]
#' core1 <- core.ma(sub=variacao.ts$subitens, pesos = pesos.ts$subitens, inf = 20, sup = 20,
#' suave = FALSE, janela = 12)
#'
#' # gráficos
#' ts.plot(variacao.ts$ipc, core1$core, col = c(1:2), lwd = 1:2)
#' }

core.ma <- function(sub, pesos, inf = 20, sup = 20, suave = FALSE, janela = 12){

    # browser()
    # sub = variacao.ts$subitens
    # pesos = pesos.ts$subitens
    # inf = 20
    # sup = 13
    # janela = 12
    # adm = T

    # sub: subitens (variação)
    # pesos: pesos dos subitens
    # sup e inf: corte percentual das caudas inferior e superior

    if (missing(sub)){
        stop("Especifique os subitens a serem utilizados.")
    }

    if (missing(pesos)){

        pesos <- sub
        peso <- 100/length(sub[,1])
        pesos <- sapply(pesos, FUN = function(x){peso})
        pesos <- matrix(peso, nrow = dim(sub)[1], ncol = dim(sub)[2])
        colnames(pesos) <- colnames(sub)

    }

    pesos_novos <- pesos*0

    # browser()

    if(!suave[1] == FALSE){

        # browser()

        # itens que serão suavizados
        codigos_suave <- suave
        codigos_suave <- paste0("cod_",codigos_suave)
        filtro_suave <- sub[,colnames(sub) %in% codigos_suave]
        filtro_suave <- (filtro_suave/100 + 1)^(1/janela)
        filtro_suave2 <- filtro_suave


        # processo de suavização
        for(i in janela:nrow(filtro_suave)){
            v <- apply(filtro_suave[(i-janela+1):i,], MARGIN = 2, FUN = prod)
            masc_v <- !is.na(v)
            filtro_suave2[i,masc_v] <- v[masc_v]
        }


        filtro_suave2 <- (filtro_suave2-1)*100
        sub[,colnames(sub) %in% codigos_suave] <- filtro_suave2
    }


    entrou <- sub*NA
    entrou_vari <- NULL

    for(i in 1:nrow(sub)){



        dados <- data.frame(x = sub[i,], pesos = pesos[i,])
        dados <- dados[order(dados$x),]
        dados$pesos_acum <- cumsum(dados$pesos)
        pos_inf <- min(which(dados$pesos_acum > inf))
        pos_sup <- min(which(dados$pesos_acum > 100-sup))

        dados_aux1 <- dados[1:(pos_inf-1),]
        dados_aux3 <- dados[(pos_inf+1):(pos_sup-1),]
        dados_aux5 <- dados[(pos_sup+1):nrow(dados),]

        peso_sai_inf <- inf - dados[pos_inf-1, "pesos_acum"]
        peso_entra_inf <- dados[pos_inf, "pesos_acum"] - inf
        peso_entra_sup <- (100-sup) - dados[pos_sup-1, "pesos_acum"]
        peso_sai_sup <- dados[pos_sup, "pesos_acum"] - (100-sup)

        dados_aux2 <- rbind(dados[pos_inf,],dados[pos_inf,])
        dados_aux4 <- rbind(dados[pos_inf,],dados[pos_inf,])

        dados_aux2$pesos <- c(peso_sai_inf, peso_entra_inf)
        dados_aux4$pesos <- c(peso_entra_sup, peso_sai_sup)

        rownames(dados_aux2) <-  c(paste0(rownames(dados[pos_inf,]),"_sai"),
                                   rownames(dados[pos_inf,]))

        rownames(dados_aux4) <-  c(rownames(dados[pos_sup,]),
                                   paste0(rownames(dados[pos_sup,]),"_sai"))

        dados_aux <- rbind(dados_aux1, dados_aux2,
                           dados_aux3, dados_aux4, dados_aux5)
        dados_aux$pesos_acum <- cumsum(dados_aux$pesos)
        dados_aux$conta <- 0

        # browser()

        dados_aux <- dados_aux[!is.na(dados_aux$x),]

        dados_aux[dados_aux$pesos_acum > inf &
                      dados_aux$pesos_acum < 100 - sup |
                      dados_aux$pesos_acum == as.character(100 - sup), "conta"] <- 1

        entrou_vari <- subset(dados_aux, subset = dados_aux$conta == 1)
        entrou[i,rownames(entrou_vari)] <- t(entrou_vari$x)

        for(j in rownames(subset(dados_aux, dados_aux$conta == 1))){
            pesos_novos[i,j] <- dados_aux[j,"pesos"]
        }

    }

    pesos_novos2 <- pesos_novos/rowSums(pesos_novos,na.rm = T)*100

    core <- ts(rowSums(pesos_novos2*sub, na.rm = T)/100, start = start(sub), frequency = 12)

    return(invisible(list(core = core, var_in = entrou)))
}


# core_ma <- core.ma(sub = a$ipca_ts, pesos = a$pesos_ts)

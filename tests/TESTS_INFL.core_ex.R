## -- EXAMPLE 1

# Once the package is built, file path is not needed
load(paste0(getwd(),"/data/codigos.rda"))
load(paste0(getwd(),"/data/pesos.ts.rda"))
load(paste0(getwd(),"/data/variacao.ts.rda"))

# Load open IPCA series codes and descriptions
codes <- codigos[2:nrow(codigos),]

# Call INFL.core_ex
ipc.ex1 <- INFL.core_ex(subitems = variacao.ts$subitens,
                        weights = pesos.ts$subitens,
                        info = codes,
                        n.blocks = 4,
                        alpha = 2)



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

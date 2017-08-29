## -- EXAMPLE 1

require(Inflation)

# Once the package is built, file path is not needed
ipca <- Inflation::ipca_sub

# Call INFL.core_ex
ipc.ex1 <- INFL.core_ex(subits.var = ipca$ipca_ts,
                        weights = ipca$weights_ts,
                        info = ipca$cod,
                        n.blocks = 4,
                        alpha = 2)

# ## -- EXAMPLE 2
#
# # Excluding food
# weights <- ipca$ipca_ts
# weights[,substr(colnames(ipca$weights), 5,5) == 1] <- 0
#
# # Rebalancing
# weights <- (weights/rowSums(novos_pesos, na.rm = TRUE))*100
# ipc.ex2 <- ts(rowSums(variacao.ts$subitens*novos_pesos, na.rm = TRUE)/100,
#           start = start(variacao.ts$subitens),
#           frequency = 12)
#
# # Plots
# ts.plot(variacao.ts$ipc, ipc.ex1, col = c(1:2), lwd = 1:2)
# ts.plot(variacao.ts$ipc, ipc.ex2, col = c(1:2), lwd = 1:2)

## -- EXAMPLE 3

ipca <- ipca_sub

#' codigos <- codigos[1:373,]
nuc <- INFL.core_ex(subits.var = ipca$ipca_ts,
                    weights = ipca$weights_ts,
                    info = ipca$cod)


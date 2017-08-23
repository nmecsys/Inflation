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

## -- EXAMPLE 2

# Excluding food
weights <- pesos.ts$subitens
weights[,substr(colnames(weights), 5,5) == 1] <- 0

# Rebalancing
weights <- (weights/rowSums(novos_pesos, na.rm = TRUE))*100
ipc.ex2 <- ts(rowSums(variacao.ts$subitens*novos_pesos, na.rm = TRUE)/100,
          start = start(variacao.ts$subitens),
          frequency = 12)

# Plots
ts.plot(variacao.ts$ipc, ipc.ex1, col = c(1:2), lwd = 1:2)
ts.plot(variacao.ts$ipc, ipc.ex2, col = c(1:2), lwd = 1:2)

## -- EXAMPLE 3

ipca <- ipca_get(group = "subitem")

# Estes códigos ainda são do IPC - mudar para IPCA
load(paste0(getwd(),"/data/codigos.rda"))

#' codigos <- codigos[1:373,]
nuc <- INFL.core_ex(subitems = ipca$ipca_ts,
                    weights = ipca$weights_ts,
                    info = ipca$cod)


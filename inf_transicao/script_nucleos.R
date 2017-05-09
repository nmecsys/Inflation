
# diretório de trabalho --------------------------------------------------
setwd("C:/Users/fernando.teixeira/Documents/INFLATION/R")

# funções e pacotes necessários ------------------------------------------
source("C:/Users/fernando.teixeira/Documents/INFLATION/inf_transicao/core_functions.R")

# leitura ---------------------------------------------------------

# leitura variação e pesos
# variacao <- read.csv2("../data/variacao.Rda")
# pesos <- read.csv2("../data/pesos.Rda")


variacao.ts$ipc <- window(variacao.ts$ipc, end = c(2016,3), freq = 12)
variacao.ts$subitens <- window(variacao.ts$subitens, end = c(2016,3), freq = 12)
pesos.ts <- ipcts(pesos)
pesos.ts$ipc <- window(pesos.ts$ipc, end = c(2016,3), freq = 12)
pesos.ts$subitens <- window(pesos.ts$subitens, end = c(2016,3), freq = 12)
codigos <- variacao.ts$codigos

# > Núcleo por Exclusão -------------

# ex1: excluindo os que mais variam
# cálculo das volatilidades

ipc.ex1 <- core.ex(variacao.ts$subitens, pesos.ts$subitens)

# ex2: excluindo alimentação
novos_pesos <- pesos.ts$subitens
novos_pesos[,substr(colnames(novos_pesos), 5,5) == 1] <- 0
novos_pesos <- (novos_pesos/rowSums(novos_pesos, na.rm = T))*100
ipc.ex3 <- ts(rowSums(variacao.ts$subitens*novos_pesos, na.rm = T)/100, start = start(variacao.ts$subitens), freq = 12)


# gráficos
ts.plot(variacao.ts$ipc, ipc.ex1, col = c(1:2), lwd = 1:2)
ts.plot(variacao.ts$ipc, ipc.ex3, col = c(1:2), lwd = 1:2)


# > Núcleo por Dupla Ponderação --------------------------------------

# janela de 12 meses
ipc.dp1 <- core.dp(variacao.ts$ipc, variacao.ts$subitens, pesos.ts$subitens, j = 12)
inf = variacao.ts$ipc
sub = variacao.ts$subitens
pesos = pesos.ts$subitens
janela = 12

# janela de 48 meses
ipc.dp2 <- core.dp(variacao.ts$ipc, variacao.ts$subitens, pesos.ts$subitens, j = 48)

# gráficos
ts.plot(variacao.ts$ipc, ipc.dp1, col = c(1:2), lwd = 1:2)
ts.plot(variacao.ts$ipc, ipc.dp2, col = c(1:2), lwd = 1:2)

# > Núcleo por médias aparadas -------------

ipc.ma2020 <- core.ma(variacao.ts$subitens, pesos.ts$subitens, inf = 20, sup = 20, suave = T)
ipc.ma2013 <- core.ma(variacao.ts$subitens, pesos.ts$subitens, inf = 20, sup = 13, suave = T)

# gráficos
ts.plot(variacao.ts$ipc, ipc.ma2020, col = c(1:2), lwd = 1:2)
ts.plot(variacao.ts$ipc, ipc.ma2013, col = c(1:2), lwd = 1:2)

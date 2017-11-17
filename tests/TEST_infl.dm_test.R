require(Inflation)

ipca <- Inflation::ipca_item

inx <- ipca$ipca_index
nuc <- Inflation::INFL.core_dw(ipca$ipca_index, ipca$ipca_ts, ipca$weights_ts, wind = 12)


act.inx <- window(inx, start = c(2016,9))
inx <- window(inx, end = c(2015,8))

act.nuc <- window(nuc, start = c(2016,9))
nuc <- window(nuc, end = c(2015,8))


infl.dm_test(inx, nuc, act.inx, act.nuc, h = 12, arma = NULL, alt = "two.sided")

require(Inflation)

ipca <- Inflation::ipca_item

inx <- ipca$ipca_index
nuc <- Inflation::INFL.core_dw(ipca$ipca_index, ipca$ipca_ts, ipca$weights_ts, wind = 12)

infl.desc_stat(inx,nuc)

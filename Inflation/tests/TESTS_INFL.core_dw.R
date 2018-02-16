## -- EXAMPLE 1

require(Inflation)

ipca <- Inflation::ipca_sub

nuc <- Inflation::INFL.core_dw(infl.var = ipca$ipca_index,
               subits.var = ipca$ipca_ts,
               weights = ipca$weights_ts,
               wind = 12)

## -- EXAMPLE 2

require(Inflation)

ipca <- Inflation::ipca_item
nuc <- Inflation::INFL.core_dw(ipca$ipca_index, ipca$ipca_ts, ipca$weights_ts, wind = 12)

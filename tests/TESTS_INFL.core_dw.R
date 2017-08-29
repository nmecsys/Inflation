## -- EXAMPLE 1

ipca <- ipca_sub

nuc <- INFL.core_dw(infl.var = ipca$ipca_index,
               subits.var = ipca$ipca_ts,
               weights = ipca$weights_ts,
               wind = 12)

## -- EXAMPLE 2

ipca <- ipca_item
nuc <- INFL.core_dw(ipca$ipca_index, ipca$ipca_ts, ipca$weights_ts, wind = 12)

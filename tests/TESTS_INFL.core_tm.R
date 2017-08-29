#ipca <- ipca_get(group = "subitem")
ipca_sub <- Inflation::ipca_sub
nuc <- INFL.core_tm(subits.var = ipca_sub$ipca_ts, weights = ipca_sub$weights_ts)

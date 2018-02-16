#ipca <- ipca_get(group = "subitem")
require(Inflation)

ipca_sub <- Inflation::ipca_sub
nuc <- Inflation::INFL.core_tm(subits.var = ipca_sub$ipca_ts, weights = ipca_sub$weights_ts)

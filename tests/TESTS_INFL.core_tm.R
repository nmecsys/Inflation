ipca <- ipca_get(group = "subitem")
nuc <- core.tm(subits.var = ipca$ipca_ts, weights = ipca$weights_ts)

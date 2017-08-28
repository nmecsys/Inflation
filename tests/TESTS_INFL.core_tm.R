#ipca <- ipca_get(group = "subitem")
load(paste0(getwd(), "/data/ipca_sub.rda"))
nuc <- core.tm(subits.var = ipca_sub$ipca_ts, weights = ipca_sub$weights_ts)

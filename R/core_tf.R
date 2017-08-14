#' Computes the triple filter core inflation
#' @param sub A \code{ts}. Subitems' variation.
#' @param weights A \code{ts}. Each subitem corresponding weights. If missing, all items get the
#' same weight.
#' @param inf An \code{integer}. Percentage lower tail cut. Predefined as 20.
#' @param sup An \code{integer}. Percentage upper tail cut. Predefined as 20.
#' @param smoo A \code{vector}. List of codes to be smoothed. If missing, no item will be smoothed.
#' @param wind An \code{integer}. The volatility's window size to be computed.
#' @param ... arguments passed on to \link[seasonal]{seas} to compute the seasonal adjustment.
#'
#' @return A \code{ts} object.
#' @keywords core triple filter
#' @export
#' @examples
#' \dontrun{
#' ipca <- ipca_get(group = "subitem")
#' core.tf(sub=ipca$ipca_ts, weights = weights_ts)
#'
#'
#' }


core.tf <- function(sub, weights, smoo, inf = 20, sup = 20, wind = 12, ...){

    ff <- core.tm(sub, weights, smoo, inf = 20, sup = 20, wind = 12)

    sf <- seasonal::seas(ff$core, ...)


    sf_2 <- sf$series$s11

    tf <- geom3(sf_2)


    return(tf)
}






# Médias aparadas
# Ajuste sazonal
# Variação acumulada em 3 meses


# # > Filtro 1: médias aparadas -------------
# # demora um pouquinho (no máximo 3 minutos)
# ipc.ma2013 <- core.ma(variacao.ts$subitens, pesos.ts$subitens, inf = 20, sup = 13, suave = T)
#
# # > Filtro 2: ajuste sazonal -------------------------
# nma2013 <- seas(ipc.ma2013, …)#, regression.aictest = NULL,
# # transform.function = "log",
# #arima.model = "(0 1 1)(1 0 0)",
# #series.modelspan = "2009.jan,2014.dec")
# # seasonal package
#
# # núcleo com ajuste sazonal
# ipc.ma2013_seas <- nma2013$series$s11
#
# # > Filtro 3: médias móveis  ---------------------------
# ipc.ma2013_seas_mm3 <- geom3(ipc.ma2013_seas) – variação acumulada

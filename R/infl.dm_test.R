#' @description
#'
#' @param
#' @return
#'
#' @keywords
#'
#' @importFrom forecast dm.test auto.arima Arima
#' @importFrom BETS BETS.predict
#' @export
#' @examples
#'
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


infl.dm_test <- function(index, core, act.index, act.core, h = 12, arma = NULL, alt = "two.sided"){

    if(is.null(arma)){

        model.index = auto.arima(index)
        arma = model.index$arma
        ord = c(arma[1],arma[5],arma[2])
        sea = c(arma[3],arma[6],arma[4])


    } else {

        ord = arma[1:3]
        sea = arma[4:6]
        model.index = Arima(index, order = ord, seasonal = sea)
    }

    model.core = Arima(core, order = ord, seasonal = sea)

    len = length(act.index)
    freq = frequency(index)
    preds.index = vector(length = len)
    preds.core = vector(length = len)


    for(i in 1:len){

        pi = forecast(model.index, h)$mean[h]
        pc = forecast(model.core, h)$mean[h]

        index = ts(c(index,pi), start = start(index), frequency = freq)
        core = ts(c(index,pc), start = start(core), frequency = freq)

        preds.index = c(preds.index, pi)
        preds.core = c(preds.core, pc)

        model.index = Arima(index, order = ord, seasonal = sea)
        model.core = Arima(core, order = ord, seasonal = sea)

    }

    e1 = preds.index - act.index
    e2 = preds.core - act.core

    dm = dm.test(e1, e2, h = h, alternative = alt)
    dm = data.frame(Statistic = dm$statistic, P.Value = dm$p.value)

    ret = list(test = dm, preds.index = preds.index, preds.core = preds.core)

    return(ret)
}

#' @description
#'
#' @param
#' @return
#'
#' @keywords
#'
#' @import dynlm
#' @import vars
#' @export
#' @examples
#'
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

infl.fit_dyn <- function(index, core, f = 1, dif = T, criterion = "SC"){

    dlm <- dynlm(index ~ 1 + core)
    u = lag(dlm$residuals,f)

    d.index = diff(index, f)
    d.core = diff(core, f)

    mat = na.omit(cbind(u,d.index,d.core))

    if(dif){
        mat = na.omit(cbind(index - core, mat))
        colnames(mat) <- c("dif","u","d.index","d.core")
        sel = VARselect(mat[,c(-1,-2)], type = "const", exogen = mat[,1:2])

    } else {
        sel = VARselect(mat[,-1], type = "const", exogen = mat[,1])
    }

    if(criterion == "SC"){
        lags = as.integer(sel$selection[3])
    } else if(criterion == "AIC"){
        lags = as.integer(sel$selection[1])
    } else if(criterion == "HQ"){
        lags = as.integer(sel$selection[2])
    } else {
        lags = as.integer(sel$selection[4])
    }

    if(dif){
        var <- VAR(mat[,c(-1,-2)], p = lags, type = "const", exogen = mat[,1:2])
    } else {
        var <- VAR(mat[,-1], p = lags, type = "const", exogen = mat[,1])
    }

    var$arch = arch.test(var)
    var$autocorr = serial.test(var)
    var$stab = roots(var)

    return(var)
}

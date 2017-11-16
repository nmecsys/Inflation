#' @description
#'
#' @param
#' @return
#'
#' @keywords
#'
#' @import dynlm
#' @importFrom vars VARselect VAR
#' @export
#' @examples
#'
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

infl.fit_dyn <- function(index, core, criterion = "SC"){

    dlm <- dynlm(index ~ 1 + core)
    u = lag(dlm$residuals,1)

    d.index = diff(index)
    d.core = diff(core)

    mat = na.omit(cbind(u,d.index,d.core))

    sel = VARselect(mat[,-1], type = "const", exogen = mat[,1])

    if(criterion = "SC"){
        lags = as.integer(sel$selection[3])
    } else if(criterion  = "AIC"){
        lags = as.integer(sel$selection[1])
    } else if(criterion = "HQ"){
        lags = as.integer(sel$selection[2])
    } else {
        lags = as.integer(sel$selection[4])
    }

    var <- VAR(mat[,-1], p = lags, type = "const", exogen = mat[,1])

    return(var)
}

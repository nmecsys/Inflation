#' Computes the double weighted core inflation
#'
#' @param infl.var A \code{ts} object. The inflation index variation.
#' @param subits.var A \code{ts}. Subitems' variation.
#' @param weights A \code{ts}. Weights corresponding to each subitem.
#' @param wind An \code{integer}. The volatility's window size.
#'
#' @return A \code{ts} object.
#' @keywords core weight
#' @export

INFL.core_dw <- function(infl.var, subits.var, weights, wind = 12){

    # Build a matrix of the differences between subitems variation and total variation
    dif <- subits.var - infl.var
    colnames(dif) <- colnames(subits.var)

    # Get starting date of inflation series
    data <- as.Date(paste0(start(dif)[1], "-", start(dif)[2],"-01"))
    # Create a sequence of all the dates
    data <- seq(data, by = "1 month", length.out = nrow(dif))

    # Create an empty matrix of dim = dim(subits.var)
    db.weights <- subits.var*0

    # Loop over moving windows
    for(t in nrow(dif):wind){

        # Get window's starting year and month
        start.year <- as.numeric(substr(data[t-wind+1],1,4))
        start.mth <- as.numeric(substr(data[t-wind+1],6,7))

        # Get window's ending year and month
        end.year <- as.numeric(substr(data[t],1,4))
        end.mth <- as.numeric(substr(data[t],6,7))

        # Get window
        dif.wind <- window(dif, start = c(start.year,start.mth), end = c(end.year,end.mth), frequency = 12)
        # Calculate standard deviations of the differences
        std.devs <- apply(dif.wind, MARGIN = 2, FUN = sd, na.rm = T)

        # Normalize the inverse of the standard deviations to obtain new weights
        # Intuition: the larger the sd, the smaller should be the weight
        # Multiply new weights by old weights (hence double weighting)
        db.weights[t,] <- ((1/std.devs)/sum(1/std.devs, na.rm = T))*weights[t,]
    }


    # Cut first 'wind' rows from db.weights (rows filled with 0s)
    # !!-- PROBABLY REDUNDANT
    dw.start = c(as.numeric(substr(data[wind+1],1,4)), as.numeric(substr(data[wind+1],6,7)))
    db.weights <- window(db.weights, start = dw.start, frequency = 12)

    # Rebalance weights
    db.weights <- db.weights/rowSums(db.weights,na.rm = T)

    # Cut first 'wind' rows from subits.var (there are no double weights for these rows)
    subits.var <- window(subits.var, start = dw.start, frequency = 12)

    # Compute weighted sum of all items to obtain the core
    core <- ts(rowSums(db.weights*subits.var, na.rm = T), start = start(subits.var), frequency = 12)
    core
}



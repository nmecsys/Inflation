#' @title Computes the volatility matrix
#'
#' @description !! DESCREVER O QUE É A MATRIZ
#'
#' @param x Subitems' variation.
#' @param info Subitems' metadata.
#' @param n.blocks Number of cuts to be made.
#' @param alpha Significance level.
#' @encoding utf8
#' @importFrom stats end sd start time ts window is.mts na.omit
#'
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}, Fernando Teixeira \email{fernando.teixeira@fgv.br}, Daiane Marcolino \email{daiane.marcolino@fgv.br}


vol.mat <- function(x, info, n.blocks, alpha){

    # Get x frequency for later usage
    freq = stats::frequency(x)

    # Temporary; a message system will be implemented
    if(! stats::is.mts(x)){stop("Argument x must be a 'mts' object.")}

    # Define parameters to divide x in 'n.blocks' blocks
    block.size = ceiling(nrow(x)/n.blocks)
    block.id = seq(n.blocks)
    std.devs = mapply(paste0, "std.devs_", block.id, USE.NAMES = FALSE)
    block.id = mapply(paste0, "x_", block.id, USE.NAMES = FALSE)

    # Get date limits of the first block
    t1 = time(x)[1]
    t2 = time(x)[1+block.size]

    # ?! provavelmente desnecessário
    ped = block.size

    for (i in seq_along(block.id)){

        # Subset x to get i-th block
        x.star <- window(x, start = t1, end = t2, freq = freq)

        # Assign i-th block to the corresponding variable
        assign(block.id[i],x.star)

        # Get date limits of the next block
        t1 = time(x)[1+block.size+1]
        #-- Isso ficaria melhor, se funcionar:
        #-- block.size = block.size + nrow(x.star)
        block.size = block.size + ped + 1
        t2 = time(x)[1+block.size]

        # Test if the end of x was reached
        if (is.na(t2)){t2 = end(x)}

        # Calculate the standard deviation of each series
        sds <- apply(x.star, MARGIN = 2, FUN = sd, na.rm = T)
        # Normalize standard deviations
        norm.sds <- (sds - mean(sds, na.rm = T))/sd(sds, na.rm = T)
        # Assign standard deviations vector to the corresponding variable
        assign(std.devs[i],norm.sds)

        rm(x.star)
    }

    # Calculate total standard deviation
    total.sd <- apply(x, MARGIN = 2, FUN = sd, na.rm = T)
    # Normalize total standard deviation
    norm.total.sd <- (total.sd - mean(total.sd, na.rm = T))/sd(total.sd, na.rm = T)

    # Initialize volatility matrix
    vol.mat <- data.frame(matrix(NA, ncol = 2 + n.blocks, nrow = ncol(x)))
    rownames(vol.mat) <- colnames(x)
    std.devs = c("info", "total_sd", std.devs)
    colnames(vol.mat) <- std.devs
    vol.mat$info <- info

    # Assign total sd to the 2nd column of the volatility matrix
    vol.mat[,2] = norm.total.sd

    # Assign blocks sds to the other columns of the volatility matrix
    for (i in 3:(length(block.id)+2)){

        vol.mat[,i] = eval(as.symbol(paste0("std.devs_", (i-2))))
    }

    # Check which items should be removed
    masc.total <- abs(vol.mat[,2]) > alpha
    masc.blocks <- abs(vol.mat[,3:length(std.devs)]) > alpha

    # Remove most volatile items
    vol.mat$SAI1 <- masc.total
    vol.mat$SAI2 <- rowSums(masc.blocks, na.rm = T) > 1

    # Order matrix according to total sd
    vol.mat <- vol.mat[order(vol.mat$total_sd, decreasing = T),]
    vol.mat
}

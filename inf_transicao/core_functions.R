# funções utilizadas no arquivo 02 Rotina.R

ipcts <- function(x, start = c(1999,1)){
    # x: data.frame
    
    x[,2] <- as.character(x[,2])
    codigos <- data.frame(cod = x$Codigo, descricao = x$Descricao)
    codigos$cod <- paste0("cod_",codigos$cod)
    
    # remover colunas codigo e descricao da tabela
    x2 <- x[,-c(1,2)]
    rownames(x2) <- codigos$cod
    x3 <- t(x2)
    
    # arrumar datas
    ipc.ts <- ts(x3, start = start, freq = 12)
    
    # Retornar resultados
    list(ipc = ipc.ts[,"cod_0"], subitens = ipc.ts[,-1], codigos = codigos)
}



# diagnóstico -----------------------------------
# gráfico spectral - diagnóstico ajuste sazonal
spectral <- function(x, seats = T){
  if(seats){
    
  

    spec.orig <-data.frame(series(x, "sp0"))
    spec.seas <-data.frame(series(x, "s1s"))
    spec.irr <- data.frame(series(x, "s2s"))
    spec.rsd <- data.frame(series(x, "spr"))
    
    orig <- ggplot2::ggplot(aes(x=0:60,y = X10.Log.Spectrum_AdjOri.), data = spec.orig, colour = "black") +
      ggplot2::geom_line() +
      ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
      ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
      ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
      ggplot2::ggtitle("Spectral plot of the first-differenced original series") +
      ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
    
    seass <- ggplot2::ggplot(aes(x=0:60,y = X10.Log.Spectrum_SA_SEATS.), data = spec.seas, colour = "black") +
      ggplot2::geom_line() +
      ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
      ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
      ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
      ggplot2::ggtitle("Spectrum of the differenced final SEATS seasonal adjustment") +
      ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
    
    
    irr <- ggplot2::ggplot(aes(x=0:60,y = X10.Log.Spectrum_Irr_SEATS.), data = spec.irr, colour = "black") +
      ggplot2::geom_line() +
      ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
      ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
      ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
      ggplot2::ggtitle("Spectrum of the final SEATS irregular") +
      ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
    
    rsd <- ggplot2::ggplot(aes(x=0:60,y = X10.Log.Spectrum_Rsd.), data = spec.rsd, colour = "black") +
      ggplot2::geom_line() +
      ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
      ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
      ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
      ggplot2::ggtitle("Spectral plot of the regARIMA model residuals") +
      ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
    
  }else{
    
    spec.orig <-data.frame(series(x, "sp0"))
    spec.seas <-data.frame(series(x, "sp1"))
    spec.irr <- data.frame(series(x, "sp2"))
    spec.rsd <- data.frame(series(x, "spr"))
    
    orig <- ggplot2::ggplot(aes(x=0:60,y = X10.Log.Spectrum_AdjOri.), data = spec.orig, colour = "black") +
      ggplot2::geom_line() +
      ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
      ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
      ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
      ggplot2::ggtitle("Spectral plot of the first-differenced original series") +
      ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
    
    seass <- ggplot2::ggplot(aes(x=0:60,y = X10.Log.Spectrum_SA.), data = spec.seas, colour = "black") +
      ggplot2::geom_line() +
      ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
      ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
      ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
      ggplot2::ggtitle("Spectrum of the differenced final SEATS seasonal adjustment") +
      ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
    
    
    irr <- ggplot2::ggplot(aes(x=0:60,y = X10.Log.Spectrum_Irr.), data = spec.irr, colour = "black") +
      ggplot2::geom_line() +
      ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
      ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
      ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
      ggplot2::ggtitle("Spectrum of the final SEATS irregular") +
      ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
    
    rsd <- ggplot2::ggplot(aes(x=0:60,y = X10.Log.Spectrum_Rsd.), data = spec.rsd, colour = "black") +
      ggplot2::geom_line() +
      ggplot2::geom_vline(colour = "red", xintercept = c(10, 20, 30, 40, 50), linetype = 5) +
      ggplot2::geom_vline(colour = "blue", xintercept = c(42, 52),  linetype = 3) +
      ggplot2::ylab(" ") + ggplot2::xlab(" ") + ggplot2::theme_bw() +
      ggplot2::ggtitle("Spectral plot of the regARIMA model residuals") +
      ggplot2::theme(plot.title = element_text(lineheight=2, face="bold",size = 16))
  }
  
  print(gridExtra::grid.arrange(orig, seass, irr, rsd, ncol = 2))
}

# diagnóstico BATS
diag.bats <- function(bats){
  res <- resid(bats)
  plot(res)
  par(mfrow = c(2,1))
  acf(res, lag.max = 48, main = "FAC - Residuos BATS", drop.lag.0 = T)
  pacf(res, lag.max = 48, main = "FACP - Residuos BATS")
  par(mfrow = c(1,1))
  hist(res, main = "Histograma dos residuos de BATS", col = "goldenrod1", border = "goldenrod4")
  list(shapiro.test(res),
       jarque.bera.test(res))
}

# múltiplo ------------------------------

# Ajuste sazonal
MSA_IPC <- function(x){
  
  if(!("seasonal" %in% installed.packages())){
    stop(paste("package 'seasonal' not found."))
  }
  
  # x: object of class "mts"
  if(is.mts(x)){
    names <- colnames(x)  
    out_x13 <- c()
    
    # Para cada uma das series, aplicar a rotina abaixo.
    for(name in names){
      x.ts <- x[,name]
      x.start <- start(x.ts)
      x.end <- end(x.ts)
      
      out_x13[[name]] <- tryCatch(
        seasonal::seas(x.ts, x11 = "", regression.aictest = NULL, x11.trendma = 23, forecast.maxlead = 0), 
        error = function(e){NULL})
      
      if(is.null(out_x13[[name]])){
        out_x13[[name]] <- tryCatch(
          seasonal::seas(x.ts, x11 = "", regression.aictest = NULL, outlier = NULL, x11.trendma = 23, forecast.maxlead = 0), 
          error = function(e){NULL})
        
        if(is.null(out_x13[[name]])){
          out_x13[[name]]$x <- x.ts
          warning(paste(name, "cannot be seasonally adjusted!"))
        }
      }
    }
    out_x13 
  }else{
    stop(paste("x is not an object of class 'mts'"))
  }
}

# BATS - tendência
m.bats <- function(x){
  # x: mts
  modelos <- list()
  comp <- NULL
  trend <- x*NA
  sazon <- x*NA
  nomes <- colnames(x)[!(colSums(is.na(x)) == length(x[,1]))]
  for(nome in nomes){
    #     if(nome == "cod_410103"){
    #       trend[,nome] <- x[,nome]
    #       sazon[,nome] <- x[,nome]
    #     }else{  
    
    modelos[[nome]] <- bats(y = na.omit(x[,nome]), use.box.cox = T, use.trend = T, use.damped.trend = T, max.p = 2, max.q = 2)
    comp <- tbats.components(modelos[[nome]])
    trend[,nome] <- tryCatch(cbind(comp[,"level"] + comp[,"slope"], x[,nome])[,1],   error = function(e){x[,nome]})
    # sazon[,nome] <- tryCatch(cbind(seasadj(modelos[[nome]]), x[,nome])[,1],   error = function(e){x[,nome]})
    
  }
  list(trend = trend, sazon = sazon)
}


# formatar taxas -------------------------------------------------------
# Acumulado nos últimos 12 meses
acum12 <- function(x){ 
  # x: ts 
  n <- length(x)
  data <- data.frame(x, y = rep(NA, n), w = rep(NA, n), z = rep(NA, n), row.names = 1:n)
  data$y <- data$x/100 + 1
  for(i in n:13){
    data[i,"w"] <-  data$y[i]*data$y[i-1]*data$y[i-2]*
      data$y[i-3]*data$y[i-4]*data$y[i-5]*data$y[i-6]*
      data$y[i-7]*data$y[i-8]*data$y[i-9]*data$y[i-10]*data$y[i-11]
  }
  data$z <- (data$w - 1)*100
  st <- ts(data$z, start = start(x), end = end(x), freq = 12)
  st  
}

# Anualizado
anual <- function(x){
  # x: ts
  ((x/100+1)^12-1)*100
}

# Relativo
relativo <- function(x){
  x/100+1
}

# acumulada em três meses + anualizar
geom3 <- function(x, anual = F){ 
  # x: ts 
  n <- length(x)
  data <- data.frame(x, y = rep(NA, n), w = rep(NA, n), z = rep(NA, n), row.names = 1:n)
  colnames(data)[1] <- "x"
  data$y <- data$x/100 + 1
  
  for(i in n:3){
    data[i,"w"] <-  data$y[i]*data$y[i-1]*data$y[i-2]
  }
  data[,"w"] <- data[,"w"]^(1/3)
  if(anual){data[,"w"] <- data[,"w"]^12}
  data$z <- (data$w - 1)*100
  st <- ts(data$z, start = start(x), end = end(x), freq = 12)
  st
}

# núcleos tradicionais --------------

# verificar volatilidade



# avaliar medidas ------------

avaliar.vies <- function(y,x, conf = 0.95, cov = NULL){
  if(is.null(cov)){
    reg <- lm(y ~ x)}else{reg <- lm(y ~ x + cov)}
  reg2 <- summary(reg)
  coef <-  reg2$coefficients[,"Estimate"]
  sds <- reg2$coefficients[,"Std. Error"]
  t_alpha <- coef[1]/sds[1]
  t_beta <- (coef[2] - 1)/sds[2]
  t_test <- qt((1+conf)/2,length(y) - 2)
  
  # teste para alpha
  if(abs(t_alpha) > t_test){msg1 <- paste0("   Conclusão: rejeita-se H0 com ",conf*100,"% de confiança")
  }else{msg1 <- paste0("   Conclusão: não rejeita-se H0 com ",conf*100,"% de confiança")}
  
  # teste para beta
  if(abs(t_beta) > t_test){msg2 <- paste0("   Conclusão: rejeita-se H0 com ",conf*100,"% de confiança")
  }else{msg2 <- paste0("   Conclusão: não rejeita-se H0 com ",conf*100,"% de confiança")}
  
  # teste conjunto
  r <- c(0,1)
  R <- rbind(c(1,0),c(0,1))
  pvalor3 <- round(linearHypothesis(reg,c("(Intercept) = 0", "x = 1"))$`Pr(>F)`[2], digits = 6)
  msg3 <- "H0: alpha = 0 e beta = 1"
  cat("- ALPHA", "   H0: alpha = 0", paste("   coef:", round(coef[1],4)),
      paste("   sd:", round(sds[1],4)), msg1, " ",
      "- BETA", "   H0: beta = 1", paste("   coef:", round(coef[2],4)),
      paste("   sd:", round(sds[2],4)), msg2, " ",
      "- ALPHA E BETA", "   H0: alpha = 0 e beta = 1",
      paste("   p-valor:",pvalor3), " ", sep = "\n")
  names(coef)[2] <- deparse(substitute(x))
  b <- list(coef = coef, sd = sds)
}

REQM <- function(y,x){
  dados <- na.omit(cbind(y,x))
  sqrt(mean((dados[,1] - dados[,2])^2, na.rm = T))
}

MAPE <- function(y,x){
  dados <- cbind(y,x)
  mean(abs((dados[,1] - dados[,2])/dados[,1]), na.rm = T)
}

# estabilidade FK via MARSS --------------------------------------------

estab.UC <- function(y, start = c(2015,1), m, Z, Q, R, B, x0 = "zero", V0 = V0, U = "zero", A = "zero", 
                     control = list(maxit = 10000), method = "BFGS"){
  
  data <- data.frame(ano = as.numeric(substr(as.Date(y),1,4)),
                     mes = as.numeric(substr(as.Date(y),6,7))
  )
  
  posicao_inicial <- which(start[1] == data$ano & start[2] == data$mes)
  
  # primeira parte
  y0 <- window(y, end = c(data[posicao_inicial - 1,1], data[posicao_inicial - 1,2]), freq = frequency(y))
  dat <- matrix(c(y0), nrow = 1)
  n <- dim(dat)[1]
  TT <- dim(dat)[2]

  model <- MARSS(dat, 
                 model = list(Z = Z, A = A, R = R, B = B, U = U, Q = Q, x0 = x0, V0 = V0), 
                 control = control, method = method)
  
  sigma <- data.frame(c(model$par$R,model$par$Q))
  states <- print(model, what = "xtT", silent = T)
  trend0 <- ts(t(states)[,1], start = start(y0), freq = 12)
  trend <- matrix(NA, nrow = nrow(data) - posicao_inicial, ncol = 1)
  
  # adicionando um dado de cada vez
  for(i in posicao_inicial:nrow(data)){
    
    y0 <- window(y, end = c(data[i,1], data[i,2]), freq = frequency(y))
    # todos os dados
    dat <- matrix(c(y0), nrow = 1)

    model <- MARSS(dat, 
                   model = list(Z = Z, A = A, R = R, B = B, U = U, Q = Q, x0 = x0, V0 = V0), 
                   control = control, method = method)
    sigma <- cbind(sigma, data.frame(c(model$par$R,model$par$Q)))
    states <- print(model, what = "xtT", silent = T)
    trend[i - posicao_inicial + 1] <- states[1,i]
  }
  
  trendok <- ts(c(trend0,trend), end = end(y), freq = 12)
  trendall <- ts(c(states[1,]), end = end(y), freq = 12)

  return(list(trend_mesames = trendok, trend_total = trendall, sigma = sigma))
}


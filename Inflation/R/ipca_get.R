#' Núcleo de inflação por exclusão
#' @param grupamento Grupamento determina o que será retornado: subgrupos, itens ou subitens
#' @keywords core ipca
#' @encoding utf8
#' @export
#' @importFrom ecoseries series_sidra
#' @import dplyr tidyr tibble
#' @examples
#' \dontrun{ipc.ex1 <- Inflation::INFL.core_ex(variacao.ts$subitens, pesos.ts$subitens)
#'
#' # ex2: excluindo alimentação
#' novos_pesos <- pesos.ts$subitens
#' novos_pesos[,substr(colnames(novos_pesos), 5,5) == 1] <- 0
#' novos_pesos <- (novos_pesos/rowSums(novos_pesos, na.rm = TRUE))*100
#' ipc.ex3 <- ts(rowSums(variacao.ts$subitens*novos_pesos,
#' na.rm = TRUE)/100, start = start(variacao.ts$subitens), freq = 12)
#'
#' }
#' # gráficos
#' ts.plot(variacao.ts$ipc, ipc.ex1, col = c(1:2), lwd = 1:2)
#' ts.plot(variacao.ts$ipc, ipc.ex3, col = c(1:2), lwd = 1:2)


grupamento = "subgroup"
grupamento = "subitem"

ipca_get <- function(group = c("subgroup", "item", "subitem")){


    grupamento <- base::match.arg(group)


    # Todas as secões associadas a esta Tabela (1419)

    section = c(7169,7170,7171,7172,7173,7175,7176,7177,12222,7184,7185,7187,
                7188,7189,7190,7191,7192,7195,107608,7200,7202,7203,7204,7205,
                7210,7211,7212,7215,7216,12223,7219,7220,7221,12224,107609,7230,
                107611,7233,7241,7242,7244,7245,7246,7248,7249,7250,7253,7254,7255,
                7256,7257,7258,7259,7260,7262,7265,7266,7267,7268,7269,7270,7272,
                7275,7276,7279,7280,7281,7283,7285,7287,7288,7291,7292,7293,7294,
                7295,7296,12294,7298,7299,7300,7301,101448,7302,7303,7305,101699,
                7306,7307,107613,7310,7311,7312,7313,8873,7316,107615,107616,7317,
                7320,7323,12300,12431,12302,12432,7333,8874,31694,7335,7336,12304,
                7339,7341,12305,12379,7347,12380,7349,107617,107618,7355,7356,12393,
                7358,7359,107619,7360,12394,7367,7372,7373,7375,7376,7377,7378,7380,
                7384,7385,7386,12395,7389,7390,12396,7392,7393,107620,107621,7396,
                7397,7401,7402,107622,7406,7407,107624,107625,107626,107627,7411,
                7412,107628,107630,7415,7416,109463,7418,12397,7422,7423,7424,7425,
                7428,7432,7433,7434,7435,7436,107633,7438,7440,7443,7444,7445,7446,
                7447,7448,7449,7451,7453,7454,7455,7456,7457,12433,7459,12398,107638,
                107639,107640,107641,107642,12399,7461,7464,7465,7466,7467,7468,7470,
                7471,12400,7479,7480,7481,7482,7483,7484,7485,7486,7487,7488,7489,7490,
                7492,12401,7493,7495,7497,7498,12402,12403,107645,7508,107646,7517,7518,
                7520,7521,7522,7523,12434,7526,7529,7530,7531,7539,7540,7541,7542,7543,
                107647,12404,7547,7548,7549,12405,107648,7551,12406,7555,12407,7558,7559,
                7560,7561,7562,7563,7564,7565,107649,7572,7573,7574,7575,7576,7577,7579,
                7582,7587,12408,7589,7590,7591,7592,107650,12409,7604,7605,7606,7607,7608,
                7609,7610,7611,107652,7614,7615,7616,7617,7618,7619,7620,7621,7622,7623,
                7624,7625,7626,
                7627,7628,7629,7630,7631,7632,7634,7635,12410,7639,7640,7641,7642,7643,
                107653,7644,7645,12411,7647,7648,7649,7650,107654,7653,107656,7654,7656,
                7657,7658,7659,107657,7660,7661,7662,7663,7664,7665,7666,12412,7669,7670,
                7671,107658,7673,7674,107659,109464,7680,7681,12413,7683,7684,7685,7686,
                12414,7688,12435,12436,7690,7691,7692,12416,7695,7696,7697,7698,7699,
                12420,101642,101644,107661,7703,7704,7707,7708,7709,7710,7711,7712,7713,
                7714,7715,12421,7719,7720,7721,7724,7727,7728,12422,7730,7731,12423,7732,
                7733,7735,12424,7736,107666,7738,12425,12426,107668,7747,7753,7756,7758,
                7759,7760,7761,7763,7766,7767,12427,7769,107670,107671,107672,107673,
                107674,7777,7778,7779,107676,107677,7782,7783,7784,7785,107678,107679,
                107680,107681,107682,12428,7786,7787,7788,7789,7790,7791,7792,107688,
                7794,12429,12430)





    # Buscamos os valores até o último mês
    month <- Sys.Date()
    data_hj <- format(month, "%Y%m")
    from = 201201

    geral <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                     sections = 7169, variable = 63)



    if(grupamento == "subitem"){

# Procurando os subitens ---------------------------------------------------------------


    teste <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                     sections = section, variable = 63)



    # Pegamos todos os códigos e filtramos só os subitens (a lista contém 454 elementos contando o geral, que é retirado)
    t1 = teste$serie_1419$`Geral, grupo, subgrupo, item e subitem`[1:454]
    t1 = unlist(t1)
    t2 = strsplit(t1, split = "\\.")
    t2 = unlist(t2)
    t2 = t2[2:907]

    # Cria-se um dicionário com TODOS os grupos-itens-subitens e seus códigos

    dicionario <- matrix(t2, nrow=2)
    dicionario <- t(dicionario)
    dicionario <- as.data.frame(dicionario, stringsAsFactors = FALSE)
    dicionario[,1] <- as.numeric(dicionario[,1])


    # Criamos uma sequencia pegando apenas números ímpares (para puxar os códigos e não as descrições)
    sequencia <- seq(from=1, to=length(t2), by=2)


    # Loop que puxa apenas os valores associados aos subitens (>=5)
    t3 <- c()
    for( i in sequencia){
        if(nchar(t2[i]) >= 5){
            t3 <- c(t3, t2[i], t2[i+1])
        }
    }



    # Transforma em data frame e numérico
    tmatriz <- matrix(t3, nrow=2)
    tmatriz = t(tmatriz)
    tmatriz <- as.data.frame(tmatriz, stringsAsFactors = FALSE)
    tmatriz[,1] <- as.numeric(tmatriz[,1])

    # JUntando os valores dos códigos na tabela às descrições e aos números associados na API
    # require(dplyr)
    a <- cbind(dicionario, section[2:454])
    codigos_sub_ipca <- dplyr::left_join(tmatriz, a, by = c("V1", "V2"))
    colnames(codigos_sub_ipca)[3] <- "cod"

    # Puxando apenas os códigos desejados (subitens) do dataframe e aplicando à função tanto para o IPCA quanto para os pesos associados
    section2 <- codigos_sub_ipca$cod


    ipca <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                      sections = section2, variable = 63)
    ipca <- ipca$serie_1419

    pesos <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                      sections = section2, variable = 66)
    pesos <- pesos$serie_1419


    colnames(tmatriz) <- c("subitem", "cod_subitens")
    tmatriz3 <- tmatriz




    } else if(grupamento == "item"){

# Procurando os itens ---------------------------------------------------------------


    teste <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                     sections = section, variable = 63)



    # Pegamos todos os códigos e filtramos só os subgrupos (a lista contém 454 elementos contando o geral, que é retirado)
    t1 = teste$serie_1419$`Geral, grupo, subgrupo, item e subitem`[1:454]
    t1 = unlist(t1)
    t2 = strsplit(t1, split = "\\.")
    t2 = unlist(t2)
    t2 = t2[2:907]

    # Cria-se um dicionário com TODOS os grupos-itens-subitens e seus códigos

    dicionario <- matrix(t2, nrow=2)
    dicionario <- t(dicionario)
    dicionario <- as.data.frame(dicionario, stringsAsFactors = FALSE)
    dicionario[,1] <- as.numeric(dicionario[,1])


    # Criamos uma sequencia pegando apenas números ímpares (para puxar os códigos e não as descrições)
    sequencia <- seq(from=1, to=length(t2), by=2)


    # Loop que puxa apenas os valores associados aos items
    t3 <- c()
    for( i in sequencia){
        if(nchar(t2[i]) == 4){
            t3 <- c(t3, t2[i], t2[i+1])
        }
    }


    # Transforma em data frame e numérico
    tmatriz <- matrix(t3, nrow=2)
    tmatriz = t(tmatriz)
    tmatriz <- as.data.frame(tmatriz, stringsAsFactors = FALSE)
    tmatriz[,1] <- as.numeric(tmatriz[,1])


    # JUntando os valores dos códigos na tabela às descrições e aos números associados na API
    # require(dplyr)
    a <- cbind(dicionario, section[2:454])
    codigos_sub_ipca <- dplyr::left_join(tmatriz, a, by = c("V1", "V2"))
    colnames(codigos_sub_ipca)[3] <- "cod"

    # Puxando apenas os códigos desejados (subgrupos) do dataframe e aplicando à função tanto para o IPCA quanto para os pesos associados
    section2 <- codigos_sub_ipca$cod


    ipca <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                    sections = section2, variable = 63)
    ipca <- ipca$serie_1419

    pesos <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                     sections = section2, variable = 66)
    pesos <- pesos$serie_1419


    colnames(tmatriz) <- c("item", "cod_item")

    tmatriz2 <- tmatriz



    } else if(grupamento == "subgroup"){

# Procurando os subgrupos -----------------------------------------------------

    teste <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                     sections = section, variable = 63)



    # Pegamos todos os códigos e filtramos só os subgrupos (a lista contém 454 elementos contando o geral, que é retirado)
    t1 = teste$serie_1419$`Geral, grupo, subgrupo, item e subitem`[1:454]
    t1 = unlist(t1)
    t2 = strsplit(t1, split = "\\.")
    t2 = unlist(t2)
    t2 = t2[2:907]

    # Cria-se um dicionário com TODOS os grupos-itens-subitens e seus códigos

    dicionario <- matrix(t2, nrow=2)
    dicionario <- t(dicionario)
    dicionario <- as.data.frame(dicionario, stringsAsFactors = FALSE)
    dicionario[,1] <- as.numeric(dicionario[,1])


    # Criamos uma sequencia pegando apenas números ímpares (para puxar os códigos e não as descrições)
    sequencia <- seq(from=1, to=length(t2), by=2)


    # Loop que puxa apenas os valores associados aos subgrupos (>=5)
    t3 <- c()
    for( i in sequencia){
        if(nchar(t2[i]) == 2){
            t3 <- c(t3, t2[i], t2[i+1])
        }
    }


    # Transforma em data frame e numérico
    tmatriz <- matrix(t3, nrow=2)
    tmatriz = t(tmatriz)
    tmatriz <- as.data.frame(tmatriz, stringsAsFactors = FALSE)
    tmatriz[,1] <- as.numeric(tmatriz[,1])


    # Juntando os valores dos códigos na tabela às descrições e aos números associados na API
    # require(dplyr)
    a <- cbind(dicionario, section[2:454])
    codigos_sub_ipca <- dplyr::left_join(tmatriz, a, by = c("V1", "V2"))
    colnames(codigos_sub_ipca)[3] <- "cod"

    # Puxando apenas os códigos desejados (subgrupos) do dataframe e aplicando à função tanto para o IPCA quanto para os pesos associados
    section2 <- codigos_sub_ipca$cod


    ipca <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                    sections = section2, variable = 63)
    ipca <- ipca$serie_1419

    pesos <- ecoseries::series_sidra(1419, from = from, to = data_hj, cl=315,
                                     sections = section2, variable = 66)
    pesos <- pesos$serie_1419

    colnames(tmatriz) <- c("grupo", "cod_grupo")

    }

# Juntando subgrupos, itens e subitens -------------------------------------------



    s <- substr(ipca$`Geral, grupo, subgrupo, item e subitem`, 1, 2)
    ipca$grupo <- as.numeric(s)
    pesos$grupo <- as.numeric(s)

    if(grupamento == "item"){
        s2 <- substr(ipca$`Geral, grupo, subgrupo, item e subitem`, 1, 4)
        ipca$item <- as.numeric(s2)
        pesos$item <- as.numeric(s2)
    } else if (grupamento == "subitem"){
        s3 <- substr(ipca$`Geral, grupo, subgrupo, item e subitem`, 1, 7)
        ipca$subitem <- as.numeric(s3)
        pesos$subitem <- as.numeric(s3)
    }


    nome <- colnames(tmatriz)[1]

    ipca <- dplyr::left_join(ipca, tmatriz, by = nome)


    pesos <- dplyr::left_join(pesos, tmatriz, by = nome)




# Formatando para Daiane --------------------------------------------------



    ipca$mes <- sapply(ipca["Data"], FUN = function(x){substr(x,6,7)})
    ipca$ano <- sapply(ipca["Data"], FUN = function(x){substr(x,1, 4)})
    colnames(ipca)[4] = c("variavel")

    pesos$mes <- sapply(pesos["Data"], FUN = function(x){substr(x,6,7)})
    pesos$ano <- sapply(pesos["Data"], FUN = function(x){substr(x,1, 4)})
    colnames(pesos)[4] = c("variavel")



    ipca$mes_ano <- paste0(ipca$ano, "-",ipca$mes, "-01")
    ipca$mes_ano <- as.Date(ipca$mes_ano)
    pesos$mes_ano <- paste0(pesos$ano, "-",pesos$mes, "-01")
    pesos$mes_ano <- as.Date(pesos$mes_ano)




    ipca <- cbind(ipca[,length(ipca[1,])], ipca[,4:9])
    ipca_wide <- tidyr::spread(ipca, key = mes_ano, value = Valor)
    ipca_wide <- ipca_wide[,6:length(ipca_wide[1,])]




    ipca_wide_t <- tibble::as_data_frame(t(ipca_wide[,1:length(ipca_wide)]))
    colnames(ipca_wide_t) <- paste0("cod_",unlist(tmatriz[,1]))
    ipca_final <- ipca_wide_t

    pesos <- cbind(pesos[,length(pesos[1,])], pesos[,4:9])
    pesos_wide <- tidyr::spread(pesos, key = mes_ano, value = Valor)
    pesos_wide <- pesos_wide[,6:length(pesos_wide[1,])]
    pesos_wide_t <- tibble::as_data_frame(t(pesos_wide[,3:length(pesos_wide)]))
    colnames(pesos_wide_t) <- paste0("cod_",unlist(tmatriz[,1]))
    pesos_final <- pesos_wide_t

    ipca_ts = ts(ipca_final, start = c(2012,01),
                 end = c(as.numeric(format(Sys.Date(), "%Y")),
                         as.numeric(c(format(Sys.Date(), "%m")))), frequency = 12)
    pesos_ts = ts(pesos_final, start = c(2012,01),
                  end = c(as.numeric(format(Sys.Date(), "%Y")),
                          as.numeric(c(format(Sys.Date(), "%m")))), frequency = 12)



    geral <- ts(geral$serie_1419[7], start = c(2012,01),
                end = c(as.numeric(format(Sys.Date(), "%Y")),
                        as.numeric(c(format(Sys.Date(), "%m")))), frequency = 12)




    #save(ipca_final, file = "ipca_final.rda")
    ls = list(ipca = ipca_final, weights = pesos_final,
              ipca_ts = ipca_ts, weights_ts = pesos_ts,
              cod = ipca_wide[,2], ipca_index = geral)
    return(invisible(ls))
#

}





# a <- ipca_get(group = "item")

# as.Date(time(a$ipca_agrupado))



#














# section = c(7169,7170,7171,7172,7173,7175,7176,7177,12222,7184,7185,7187,7188,7189,7190,7191,7192,7195,107608,7200,7202,7203,7204,7205,7210,7211,7212,7215,7216,12223,7219,7220,7221,12224,107609,7230,107611,7233,7241,7242,7244,7245,7246,7248,7249,7250,7253,7254,7255,7256,7257,7258,7259,7260,7262,7265,7266,7267,7268,7269,7270,7272,7275,7276,7279,7280,7281,7283,7285,7287,7288,7291,7292,7293,7294,7295,7296,12294,7298,7299,7300,7301,101448,7302,7303,7305,101699,7306,7307,107613,7310,7311,7312,7313,8873,7316,107615,107616,7317,7320,7323,12300,12431,12302,12432,7333,8874,31694,7335,7336,12304,7339,7341,12305,12379,7347,12380,7349,107617,107618,7355,7356,12393,7358,7359,107619,7360,12394,7367,7372,7373,7375,7376,7377,7378,7380,7384,7385,7386,12395,7389,7390,12396,7392,7393,107620,107621,7396,7397,7401,7402,107622,7406,7407,107624,107625,107626,107627,7411,7412,107628,107630,7415,7416,109463,7418,12397,7422,7423,7424,7425,7428,7432,7433,7434,7435,7436,107633,7438,7440,7443,7444,7445,7446,7447,7448,7449,7451,7453,7454,7455,7456,7457,12433,7459,12398,107638,107639,107640,107641,107642,12399,7461,7464,7465,7466,7467,7468,7470,7471,12400,7479,7480,7481,7482,7483,7484,7485,7486,7487,7488,7489,7490,7492,12401,7493,7495,7497,7498,12402,12403,107645,7508,107646,7517,7518,7520,7521,7522,7523,12434,7526,7529,7530,7531,7539,7540,7541,7542,7543,107647,12404,7547,7548,7549,12405,107648,7551,12406,7555,12407,7558,7559,7560,7561,7562,7563,7564,7565,107649,7572,7573,7574,7575,7576,7577,7579,7582,7587,12408,7589,7590,7591,7592,107650,12409,7604,7605,7606,7607,7608,7609,7610,7611,107652,7614,7615,7616,7617,7618,7619,7620,7621,7622,7623,7624,7625,7626,7627,7628,7629,7630,7631,7632,7634,7635,12410,7639,7640,7641,7642,7643,107653,7644,7645,12411,7647,7648,7649,7650,107654,7653,107656,7654,7656,7657,7658,7659,107657,7660,7661,7662,7663,7664,7665,7666,12412,7669,7670,7671,107658,7673,7674,107659,109464,7680,7681,12413,7683,7684,7685,7686,12414,7688,12435,12436,7690,7691,7692,12416,7695,7696,7697,7698,7699,12420,101642,101644,107661,7703,7704,7707,7708,7709,7710,7711,7712,7713,7714,7715,12421,7719,7720,7721,7724,7727,7728,12422,7730,7731,12423,7732,7733,7735,12424,7736,107666,7738,12425,12426,107668,7747,7753,7756,7758,7759,7760,7761,7763,7766,7767,12427,7769,107670,107671,107672,107673,107674,7777,7778,7779,107676,107677,7782,7783,7784,7785,107678,107679,107680,107681,107682,12428,7786,7787,7788,7789,7790,7791,7792,107688,7794,12429,12430)



# teste <- ecoseries::series_sidra(1419, from = 201201, to = 201703, cl=315,
#                                  sections = section, variable = 63)
#
# teste2 <- ecoseries::series_sidra(1419, from = 201201, to = 201703, cl=315,
#                                   sections = section, variable = 66)

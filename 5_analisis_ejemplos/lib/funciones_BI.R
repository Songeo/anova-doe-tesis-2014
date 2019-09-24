
# Graf BI
GG.Dif.bi <- function(aux, nom, nivs, ord.nivs = NULL){
  tab <- data.frame(aux[grep(nom, rownames(aux)), ])
  print(rownames(tab))
  tab$niveles <- factor(nivs, levels = nivs)  
  if( length(ord.nivs) != 0){
    tab$niveles <- factor(tab$niveles, levels = ord.nivs)
  }
  gg <- ggplot(tab, aes(x = niveles, y = X50.)) + 
    geom_pointrange( aes(ymin = X2.5., ymax = X97.5.), alpha = .5 ) +  
    geom_pointrange( aes(ymin = X25., ymax = X75.), size = 1) +  
    geom_point(size = 1) + 
    geom_hline(yintercept = 0, linetype = 2, alpha = .5) +
    coord_flip() 
}

BIB.test_mod <- function (block, trt, y, alpha = 0.05) 
{
  # NOMBRE DE VARIABLES
  block.unadj <- as.factor(block)
  trt.adj <- as.factor(trt)
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  
  # MODELOS
  formu <- formula(paste(name.y, "~ block.unadj + trt.adj"))
  model <- lm(formu)
  DFerror <- df.residual(model)
  MSerror <- deviance(model)/DFerror
  # formu <- formula(paste(name.y, "~ block.unadj + trt.adj + Error(block.unadj)") )
  # model <- aov(formula = formu) 
  # summary(model)
  #tab.errores <- summary(model)[["Error: Within"]][[1]]["Residuals", ]
  #DFerror <- tab.errores[['Df']]
  #MSerror <- tab.errores[['Mean Sq']]
  
  # PARAMETROS
  k <- unique( table(block.unadj) )
  r <- unique( table(trt.adj) )
  b <- nlevels(block.unadj)
  a <- nlevels(trt.adj)
  lambda <- r * (k - 1)/(a - 1)
  fact <- k/(lambda * a)
  eff <- lambda * a/(r * k)
  dat <- data.frame(block, tmt = trt, y)  
  parametros <- list(lambda = lambda, 
                     a = a, 
                     b = b,
                     k = k, 
                     r = r, 
                     efficiency.factor = eff)
  
  # CREAR Q's (TOTAL ADJUSTED)
  tab.0 <- by(dat[, 3], dat[, 1:2], function(x) mean(x, na.rm = TRUE))
  tab <- data.frame(tab.0[ , ])
  AA <- !is.na(tab) # indicadoras
  BB <- tapply(y, block.unadj, sum) # suma por bloque
  B <- BB %*% AA    # producto de bloques ajustados
  Y <- tapply(y, trt.adj, sum)    # promedio por tratamiento
  Q <- Y - as.numeric(B)/k    # Q's (ajustados)
  efectos <- (k*Q)/(lambda*a)
  estimaciones <- cbind( Q.i = Q,
                        tau.i = efectos)
  
  # AJUSTE MEDIAS PARA MODELO
  SStrt.adj <- (k* sum(Q^2) ) / (lambda * a)
  MStrt.adj <- SStrt.adj/(a - 1)
  sdtdif <- sqrt(2 * k * MSerror/(lambda * a)) ###### ni idea porque lo multiplica por 2
  
  # EVALUACIÓN MEDIAS AJUSTADAS
  Fvalue <- MStrt.adj/MSerror
  mean.adj <- mean(y) + Q * k/(lambda * a)
  StdError.adj <- sqrt( MSerror * (1 + k*r*(a-1)/(lambda*a)) / (r*a) )
  
  # TABLAS FINALES
  anova.tab <- anova(model)# summary(model) #
  mean <- mean(y, na.rm = TRUE) # Revisar por el promedio ajustado
  nameTrt <- row.names(Y)
  means.tab <- data.frame(trt = nameTrt, means = Y/r, mean.adj, StdError.adj)
  
  # VARIABLES PARA COMPARAR TRATAMIENTOS
  Omeans <- order(mean.adj, decreasing = TRUE)
  Ordindex <- order(Omeans)
  comb <- combn(a, 2)
  nn <- ncol(comb)  
  dif.orig <- rep(0, nn)
  odif <- dif.orig
  sig <- rep(" ", nn)
  
  # DIFERENCIA ENTRE TODOS LOS PARES
  for (k in 1:nn) {
    i <- comb[1, k]
    j <- comb[2, k]
    dif.orig[k] <- (mean.adj[i] - mean.adj[j])
    odif[k] <- abs(Ordindex[i] - Ordindex[j]) + 1
  }
  dif <- abs(dif.orig)
  
  # CALCULO DE P-VALUES
    # LSD : t-Student
    tprob.lsd <- qt(1 - alpha/2, DFerror)
    pvalue.lsd <- 2 * (1 - pt(dif/sdtdif, DFerror))
  
    # HSD : Studentized Range Distribution  
    tprob.tuk <- qtukey(1 - alpha, a, DFerror)
    sdtdif.tuk <- sdtdif/sqrt(2)
    fact.tuk <- fact/2
    pvalue.hsd <- 1 - ptukey(dif/sdtdif.tuk, a, DFerror)
        
    # SNK
    snk <- 1
    sdtdif.snk <- sdtdif/sqrt(2)
    Tprob.snk <- qtukey(1 - alpha, 2:a, DFerror)
    SNK <- Tprob.snk * sdtdif.snk
    pvalue.snk <- 1 - ptukey( dif/sdtdif.snk, odif, DFerror)

    # DUNCAN
    snk.dun <- 2
    sdtdif.dunc <- sdtdif/sqrt(2)
    Tprob.dunc <- qtukey((1 - alpha)^(1:(a - 1)), 2:a, DFerror)
    duncan <- Tprob.dunc * sdtdif.dunc
    names(duncan) <- 2:a
    # REVISAR FORMULA
    pvalue.duncan <- (1 - ptukey(dif/sdtdif.dunc, odif, DFerror)) ^ 1/(odif - 1)
  
    pvalues.list <- list(LSD = pvalue.lsd, 
                       HSD = pvalue.hsd, 
                       SNK = pvalue.snk, 
                       DUNCAN = pvalue.duncan)
  
  # EVALUACIÓN DE DIFERENCIAS SIGNIFICATIVAS 
  eval.list <- lapply(names(pvalues.list), function(nom){
    df.ps <- data.frame( Difference = dif.orig )
    df.ps$pvalue <- pvalues.list[[nom]]
    df.ps$sig <- ""
    df.ps$sig[df.ps$pvalue < .1] <- "."
    df.ps$sig[df.ps$pvalue < .05] <- "*"
    df.ps$sig[df.ps$pvalue < .01] <- "**"
    df.ps$sig[df.ps$pvalue < .001] <- "***"
    rownames(df.ps) <- paste(nameTrt[comb[1, ]], '-', nameTrt[comb[2, ]])
    df.ps
  })
  names(eval.list) <- c(
    'Least Significant Difference', 
    'Honestly Significant Difference', 
    "Duncan's Multiple Range Test", 
    'Student-Newman-Keuls'
  )
  res <- list(#Anova = anova.tab, 
       #Overall = overall.mean, 
       Means = means.tab, 
       Parametros = parametros, 
       Pruebas = eval.list,
       Estimación = estimaciones)
  res
}







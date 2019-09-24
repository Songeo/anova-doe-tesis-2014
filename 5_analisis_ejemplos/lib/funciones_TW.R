# Tablas para el descriptivo TW
xtableTipoTemp <- function(xtab, nom.cambio=T, nom.tmt, cap = NULL, lab = NULL){
  xtab 
  if(nom.cambio){
    names(xtab) <- c(nom.tmt, "promedio", "desviación")  
  }
  else{
    names(xtab) <- c(nom.tmt, names(xtab)[-1])  
  }
  xtable(xtab, align = rep("c", ncol(xtab)+1), digits = rep(0, ncol(xtab)+1),caption = cap, label = lab )
}


# Intervalo de media con distribución normal
norm.ci <- function(xvec, alpha = 0.05) {
  xbar = mean(xvec)
  s = sqrt( var(xvec)/(length(xvec)-1) )
  zalpha = qnorm((alpha)/2, lower.tail = FALSE)
  c(promedio = xbar, LCI = xbar - zalpha * s, UCI = xbar + zalpha * s)
}



# Graf TW
GG.Dif.TW <- function(aux, nom, nivs, ord.nivs = NULL){
  tab <- data.frame(aux[grep(nom, rownames(aux)), ])
  tab$niveles <- factor(nivs, levels = nivs)  
  if( length(ord.nivs) != 0){
    tab$niveles <- factor(tab$niveles, levels = ord.nivs)
  }
  tab  
  gg <- ggplot(tab, aes(x = niveles, y = X50.)) + 
    geom_linerange( aes(ymin = X2.5., ymax = X97.5.), alpha = .5 ) +  
    geom_linerange( aes(ymin = X25., ymax = X75.), size = 1) +  
    geom_point(size = 3) + 
    geom_hline(yintercept = 0, linetype = 2, alpha = .5) +
    coord_flip() 
}

# Comparación de medias
LSDvsDUN <- function(dif.lsd, dif.dun){
  dif.lsd$dif.nom <- rownames(dif.lsd)
  dif.dun$dif.nom <- rownames(dif.dun)
  #   rownames(dif.lsd) <- NULL
  #   rownames(dif.dun) <- NULL
  dif.lsd$tipo <- "LSD"
  dif.dun$tipo <- "Duncan's Range"  
  
  tab <- rbind(dif.lsd, dif.dun)
  tab$pvalue <- as.numeric(as.character(tab$pvalue))
  tab$eval <- factor(tab$pvalue <= 0.05, levels = c("TRUE", "FALSE"),
                     labels = c("diferencia significativa", "diferencia no significativa") )
  
  gg.1 <- ggplot(tab, aes(x = dif.nom, y = Difference, ymin = LCL, ymax =  UCL, colour = eval)) +
    geom_linerange() + 
    geom_point( size = 3 ) + 
    facet_wrap(~tipo) + 
    coord_flip() + 
    ylab("diferencia") + 
    xlab("tratamientos") + 
    theme(legend.position = "bottom") + 
    scale_color_manual(values=c("black", "gray")) + 
    labs(colour = "significancia")
  
  gg.2 <- ggplot(tab, aes(x = tipo, y = Difference, ymin = LCL, ymax =  UCL, colour = eval)) +
    geom_linerange() + 
    geom_point( size = 2 ) + 
    facet_wrap(~dif.nom, scale = "free_y") + 
    coord_flip() + 
    ylab("diferencia") + 
    xlab("tratamientos") + 
    theme(legend.position = "bottom") + 
    scale_color_manual(values=c("black", "gray")) + 
    labs(colour = "significancia")
  gg.1
}



# LSD test
LSD.test_mod <- function (y, trt, DFerror, MSerror, alpha = 0.05, p.adj = c("none", 
                                                                            "holm", "hochberg", "bonferroni", "BH", "BY", "fdr"), group = TRUE, 
                          main = NULL) 
{
  p.adj <- match.arg(p.adj)
  clase <- c("aov", "lm")
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  if ("aov" %in% class(y) | "lm" %in% class(y)) {
    A <- y$model
    DFerror <- df.residual(y)
    MSerror <- deviance(y)/DFerror
    y <- A[, 1]
    ipch <- pmatch(trt, names(A))
    if (is.na(ipch)) 
      return(cat("Name: ", trt, "\n", names(A)[-1], "\n"))
    name.t <- names(A)[ipch]
    trt <- A[, ipch]
    name.y <- names(A)[1]
  }
  junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
  means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
  sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
  nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
  std.err <- sds[, 2]/sqrt(nn[, 2])
  Tprob <- qt(1 - alpha/2, DFerror)
  LCL <- means[, 2] - Tprob * std.err
  UCL <- means[, 2] + Tprob * std.err
  means <- data.frame(means, std.err, replication = nn[, 2], 
                      LCL, UCL)
  names(means)[1:2] <- c(name.t, name.y)
  ntr <- nrow(means)
  nk <- choose(ntr, 2)
  if (p.adj != "none") {
    a <- 1e-06
    b <- 1
    for (i in 1:100) {
      x <- (b + a)/2
      xr <- rep(x, nk)
      d <- p.adjust(xr, p.adj)[1] - alpha
      ar <- rep(a, nk)
      fa <- p.adjust(ar, p.adj)[1] - alpha
      if (d * fa < 0) 
        b <- x
      if (d * fa > 0) 
        a <- x
    }
    Tprob <- qt(1 - x/2, DFerror)
  }
  nr <- unique(nn[, 2])
  cat("\nStudy:", main)
  cat("\n\nLSD t Test for", name.y, "\n")
  if (p.adj != "none") 
    cat("P value adjustment method:", p.adj, "\n")
  cat("\nMean Square Error: ", MSerror, "\n\n")
  cat(paste(name.t, ",", sep = ""), " means and individual (", 
      (1 - alpha) * 100, "%) CI\n\n")
  print(data.frame(row.names = means[, 1], means[, -1]))
  cat("\nalpha:", alpha, "; Df Error:", DFerror)
  cat("\nCritical Value of t:", Tprob, "\n")
  if (group) {
    if (length(nr) == 1) {
      LSD <- Tprob * sqrt(2 * MSerror/nr)
      cat("\nLeast Significant Difference", LSD)
    }
    else {
      nr1 <- 1/mean(1/nn[, 2])
      LSD1 <- Tprob * sqrt(2 * MSerror/nr1)
      cat("\nLeast Significant Difference", LSD1)
      cat("\nHarmonic Mean of Cell Sizes ", nr1)
    }
    cat("\nMeans with the same letter are not significantly different.")
    cat("\n\nGroups, Treatments and means\n")
    output <- order.group(means[, 1], means[, 2], means[, 
                                                        4], MSerror, Tprob, means[, 3])
    w <- order(means[, 2], decreasing = TRUE)
    output <- data.frame(output, LCI = means[w, 5], UCI = means[w, 
                                                                6])
  }
  if (!group) {
    comb <- combn(ntr, 2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    pvalue <- dif
    sdtdif <- dif
    sig <- rep(" ", nn)
    for (k in 1:nn) {
      i <- comb[1, k]
      j <- comb[2, k]
      dif[k] <- means[i, 2] - means[j, 2]
      sdtdif[k] <- sqrt(MSerror * (1/means[i, 4] + 1/means[j, 
                                                           4]))
      pvalue[k] <- 2 * (1 - pt(abs(dif[k])/sdtdif[k], DFerror))
    }
    if (p.adj != "none") 
      pvalue <- round(p.adjust(pvalue, p.adj), 6)
    LCL1 <- dif - Tprob * sdtdif
    UCL1 <- dif + Tprob * sdtdif
    for (k in 1:nn) {
      if (pvalue[k] <= 0.001) 
        sig[k] <- "***"
      else if (pvalue[k] <= 0.01) 
        sig[k] <- "**"
      else if (pvalue[k] <= 0.05) 
        sig[k] <- "*"
      else if (pvalue[k] <= 0.1) 
        sig[k] <- "."
    }
    tr.i <- means[comb[1, ], 1]
    tr.j <- means[comb[2, ], 1]
    output <- data.frame(Diferencia = dif, pvalue = round(pvalue, 6), #sig, 
                         LCL = LCL1, UCL = UCL1)
    rownames(output) <- paste(tr.i, tr.j, sep = " - ")
    cat("\nComparison between treatments means\n\n")
    print(output)
    output.2 <- data.frame(trt = means[, 1], means = means[, 
                                                           2], M = "", N = means[, 4], std.err, LCL, UCL)
  }
  list(output, output.2) 
}




# duncan test
duncan.test_mod <- function (y, trt, DFerror, MSerror, alpha = 0.05, group = TRUE, 
                             main = NULL) 
{
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  clase <- c("aov", "lm")
  if ("aov" %in% class(y) | "lm" %in% class(y)) {
    A <- y$model
    DFerror <- df.residual(y)
    MSerror <- deviance(y)/DFerror
    y <- A[, 1]
    ipch <- pmatch(trt, names(A))
    if (is.na(ipch)) 
      return(cat("Name: ", trt, "\n", names(A)[-1], "\n"))
    name.t <- names(A)[ipch]
    trt <- A[, ipch]
    name.y <- names(A)[1]
  }
  junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
  means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
  sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
  nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
  means <- data.frame(means, std.err = sds[, 2]/sqrt(nn[, 2]), 
                      replication = nn[, 2])
  names(means)[1:2] <- c(name.t, name.y)
  ntr <- nrow(means)
  Tprob <- qtukey((1 - alpha)^(1:(ntr - 1)), 2:ntr, DFerror)
  nr <- unique(nn[, 2])
  cat("\nStudy:", main)
  cat("\n\nDuncan's new multiple range test\nfor", name.y, 
      "\n")
  cat("\nMean Square Error: ", MSerror, "\n\n")
  cat(paste(name.t, ",", sep = ""), " means\n\n")
  print(data.frame(row.names = means[, 1], means[, -1]))
  if (length(nr) == 1) 
    sdtdif <- sqrt(MSerror/nr)
  else {
    nr1 <- 1/mean(1/nn[, 2])
    sdtdif <- sqrt(MSerror/nr1)
  }
  DUNCAN <- Tprob * sdtdif
  names(DUNCAN) <- 2:ntr
  cat("\nalpha:", alpha, "; Df Error:", DFerror, "\n")
  cat("\nCritical Range\n")
  print(DUNCAN)
  if (length(nr) > 1) {
    cat("\nHarmonic Mean of Cell Sizes ", nr1)
    cat("\n\nDifferent value for each comparison")
  }
  if (group) {
    cat("\nMeans with the same letter are not significantly different.")
    cat("\n\nGroups, Treatments and means\n")
    output <- order.group(means[, 1], means[, 2], means[, 
                                                        4], MSerror, 1, means[, 3], parameter = 0.5, snk = 2, 
                          DFerror, alpha, sdtdif)
  }
  if (!group) {
    Omeans <- order(means[, 2], decreasing = TRUE)
    Ordindex <- order(Omeans)
    comb <- combn(ntr, 2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    DIF <- dif
    LCL <- dif
    UCL <- dif
    pvalue <- dif
    odif <- dif
    sig <- NULL
    for (k in 1:nn) {
      i <- comb[1, k]
      j <- comb[2, k]
      dif[k] <- means[i, 2] - means[j, 2]
      DIF[k] <- abs(dif[k])
      nx <- abs(i - j) + 1
      odif[k] <- abs(Ordindex[i] - Ordindex[j]) + 1
      pvalue[k] <- round((1 - ptukey(DIF[k]/sdtdif, odif[k], 
                                     DFerror))^1/(odif[k] - 1), 6)
      LCL[k] <- dif[k] - DUNCAN[odif[k] - 1]
      UCL[k] <- dif[k] + DUNCAN[odif[k] - 1]
      sig[k] <- " "
      if (pvalue[k] <= 0.001) 
        sig[k] <- "***"
      else if (pvalue[k] <= 0.01) 
        sig[k] <- "**"
      else if (pvalue[k] <= 0.05) 
        sig[k] <- "*"
      else if (pvalue[k] <= 0.1) 
        sig[k] <- "."
    }
    tr.i <- means[comb[1, ], 1]
    tr.j <- means[comb[2, ], 1]
    output <- data.frame(Difference = dif, pvalue = round(pvalue, 6), #sig,
                         LCL, UCL)
    rownames(output) <- paste(tr.i, tr.j, sep = " - ")
    cat("\nComparison between treatments means\n\n")
    print(output)
    output.2 <- data.frame(trt = means[, 1], means = means[, 
                                                           2], M = "", N = means[, 4], std.err = means[, 3])
  }
  list(output, output.2)
}
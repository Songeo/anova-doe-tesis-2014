library('ProjectTemplate')
load.project()


dat <- na.omit(BD.montgomery.6)
dat$treatment <- as.numeric(gsub("Tmt", "", dat$treatment))
dat$block <- as.numeric(gsub("Bck", "", dat$block))
sapply(dat, class)

tmt <- 'catalizador'
block <- 'maquina'
names(dat) <- c('y', tmt, block)
head(dat)

dat$catalizador <- factor(dat$catalizador, levels = 1:4, labels = paste("catalizador", 1:4))
dat$maquina <- factor(dat$maquina, levels = 1:4, labels = paste("máquina", 1:4))

# MODELO AOV
aov.bi <-  aov(data = dat, y ~ catalizador + maquina + Error(maquina))
summary(aov.bi)



# Ajuste del Modelo
dat$residuales <- residuals.aovlist(aov.bi)
dat$prediccion <- fitted.aovlist(aov.bi)

ggplot(dat, aes(sample = residuales)) + 
  geom_point(stat = "qq") +
  geom_abline(yintercept=-.5, slope=.5, alpha = .5) + 
  ylab('muestral') + 
  xlab('teórico')

ggplot(dat, aes(x = prediccion, y = residuales, group = 1)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2, colour = "blue" ) 

ggplot(dat, aes(x = catalizador, y = residuales, group = 1)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2, colour = "blue" ) 

ggplot(dat, aes(x = maquina, y = residuales, group = 1)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2, colour = "blue" ) 

ggplot(dat, aes(x = prediccion, y = y)) + 
  geom_point() + 
  ylab('tiempo observado') + xlab('tiempo estimado')



# TABLA ANOVA
tab.sum <- as.matrix( summary(aov.bi[["maquina"]])[[1]] )
rownames(tab.sum) <- "maquina"
xtable(tab.sum, digits = c(0,0,0,0), align = "cccc", label = "tab_aovbi_between")
xtable(summary(aov.bi[["Within"]]), digits = c(0,0,0,0,1,2), align = "cccccc")

# ANOVA of Bloques Incompletos
pruebas <- BIB.test_mod(block = dat$maquina, trt = dat$catalizador, y = dat$y, alpha = 0.05)
names(pruebas)
names(pruebas$Pruebas)


xtable(pruebas$Pruebas$`Least Significant Difference`, align = "cccc", digits = c(0,2,2,0), caption = "Prueba Diferencia Mínima Significativa entre Tratamientos de Catalizador", label ="lsd_cat")

xtable(pruebas$Pruebas$`Duncan's Multiple Range Test`, align = "cccc", digits = c(0,2,2,0), caption = "Prueba del Rango Múltiple de Duncan entre Tratamientos de Catalizador", label ="dun_cat")



# EXTRA
# Evaluacion con AGRICOLAE
library(agricolae)

lsd.test <- BIB.test(bloques, tratamientos, y, method = "lsd", group = FALSE)
tukey.test <- BIB.test(bloques, tratamientos, y, method = "tukey", group = FALSE)
snk.test <- BIB.test(materia.prima, catalizadores, y, method = "snk", group = FALSE)
duncan.test <- BIB.test(materia.prima, catalizadores, y, method = "duncan", group = FALSE)



# PARAMETROS
# Diseño del modelo
dis <- table(tmt = na.omit(dat)[, tmt], bck = na.omit(dat)[, block])
a <- nrow(dis)
b <- ncol(dis)
k <- unique(apply(dis, 2, sum))
r <- unique(apply(dis, 1, sum))
lamb <- r*(k-1)/(a-1)
N <- nrow(na.omit(dat))
diseño; a; k; r; lamb

# AJUSTES
# Suma por tratamiento y bloque
dat.1 <- ddply(dat, tmt, transform, sum.cat = sum(y, na.rm = TRUE))
dat.2 <- ddply(dat.1, block, transform, sum.mat = sum(y, na.rm = TRUE))
dat.2$ind <- dat.2$y/dat.2$y
dat.2$ind[is.na(dat.2$ind)] <- 0
# Ajuste por bloque
dat.3 <- ddply(dat.2, tmt, transform, adj.bloque = (sum.mat*ind)/k)
# Ajuste por tratamiento
dat.4 <- ddply(dat.3, tmt, transform, qs = sum.cat - sum(adj.bloque))
head(dat.4)


# Comprobar teoría
df.qs <- ddply(dat.4, tmt, summarise, qs = mean(qs))

SS.tmt.ad <- k*sum(df.qs$qs^2)/(lamb*a)
MS.tmt.ad <- SS.tmt.ad/(a-1)
SS.tmt.ad
MS.tmt.ad

SS.tot <- sum(dat.4$y^2, na.rm = TRUE) - (sum(dat.4$y, na.rm=TRUE)^2)/12
MS.tot <- SS.tot / (N-1)
SS.tot

SS.block <- sum(ddply(dat.4, block, summarise, sum.b = sum( y, na.rm = T)^2/k)$sum.b) - (sum(dat.4$y, na.rm=TRUE)^2)/12 
MS.block <- SS.block/(b-1)
SS.block
MS.block

SS.error <- SS.tot - SS.tmt.ad - SS.block
MS.error <- SS.error/(5)
SS.error
MS.error

F.0 <- MS.tmt.ad/MS.error
F.0
library(ProjectTemplate)
load.project()

base <- BD.Engel.split.plot
base$main <- NULL
for(k in 2:11){
  base[, k] <- base[, k]-1
}
head(base)


# Dos análisis para resolver el ejemplo de split plot saturado
# A) Daniel 1959 discriminar efectos con grafica normal (semi normal)
# B) Analizar por separado


# A)
# ===========
# DANIEL 1959
# SELECCIÓN DE TRATAMIENTOS 
# separado en 2 

#(MAIN PLOT)
lm.main <- lm(data = base, formula = percentage  ~ 
  A+B+C+D+E+F+G+m+n+o)
summary(lm.main)
round(lm.main$coefficients, 2)
main.eff <- lm.main$coefficients

#(SUB PLOT)
lm.sub <- lm(data = base, formula = percentage  ~ 
  (A+B+C+D+E+F+G)*m + 
  (A+B+C+D+E+F+G)*n + 
  (A+B+C+D+E+F+G)*o + m:n + m:o + n:m )
summary(lm.sub)
round(lm.sub$coefficients/2, 2)
sub.eff <- (lm.sub$coefficients/2)[12:32]

# Tabla de efectos
efectos <- c(main.eff, sub.eff)[-1]
effs.tab <- c(main.eff[1:8], 
              efectos[grep("m", names(efectos))],
              efectos[grep("n", names(efectos))],
              efectos[grep("o", names(efectos))]
)
tab.eff <- matrix(round(effs.tab,2), nrow = 8)
rownames(tab.eff) <- names(main.eff)[1:8]
colnames(tab.eff) <- names(main.eff)[c(1,9:11)]


# COMPROBAR EFECTOS
# efectos principales
df.m <- arrange(melt(base, id.vars=c("num", "percentage")), num, variable, value)
tt <- ddply(df.m, c("variable", "value"), summarise, prom = mean(percentage) )
tt.d <- dcast(data = tt, formula = variable ~value, value.var="prom")
tt.d$efecto.dif <- tt.d[, 3] - tt.d[, 2]
tt.d$efecto.lm <- main.eff[-1]
# qplot(x=efecto.dif, y = efecto.lm, data = tt.d, geom=c("point", "abline") )
# lm.m <- lm(data = base, formula = percentage ~ A+B+C+D+E+F+G, x = T)
# md.mat <- lm.m$x
# DanielPlot(fit = lm.m)
# DanielPlot(fit = lm.m, half=T)
# LenthPlot(obj = lm.m)
# plot(BsProb(X = base[,2:8], y=base$percentage, blk = 0, mFac = 7, mInt =2))
# print(BsProb(X = base[,2:8], y=base$percentage, blk = 0, mFac = 7, mInt =2))


# PLOT DE EFECTOS
efectos <- c(main.eff, sub.eff)[-1]
df.eff.0 <- data.frame(effects = efectos, 
             noms.eff = names(efectos))
df.eff.0$tipo <- 'efectos de sublote'
df.eff.0$tipo[names(efectos) %in% LETTERS[1:7]] <- 'efectos de lote principal'

CalculoS0 <- function(efs){
  abs.efs <- abs(efs)
  s0 <- 0
  while( sum(2.5*s0 < abs.efs)  > 0 ){
    med <- median( abs.efs )
    s0 <- 1.5*med
    abs.efs <- abs.efs[!(2.5*s0 < abs.efs)] 
  }
  s0
}

l.effs.sp <- dlply(df.eff.0, "tipo", function(sub){
  # qqplot
  tt <- qqnorm(sub$effects, plot=F) 
  sub$theo <- tt$x
  sub$samp <- tt$y
  # qqplot seminormal
  factor.effects <- abs(sub$effects)
  sub$theo.semi <- qnorm(0.5 * (( rank(abs(factor.effects)) -  0.5) 
      /length(factor.effects) + 1 ) )
  sub$samp.semi <- factor.effects
  sub
})
df.eff.sp <- Reduce(rbind, l.effs.sp)
aux.lenth <- ddply(df.eff.sp, "tipo", function(sub){
  s0.est <- CalculoS0(sub$effects)
  ME <- qt(.975, nrow(sub)/3)*s0.est
  SME <- qt( (1+.95^(1/nrow(sub)))/2, nrow(sub)/3)*s0.est
  c(ME = ME, SME = SME)
})
df.eff.sp <- join(df.eff.sp,aux.lenth, type = "left", by = "tipo")

                    
# a) QQPLOT separado por niveles del diseño
gg.qq <- ggplot( df.eff.sp, aes( sample = samp) ) + 
  facet_wrap(~tipo, scale = "free_y") +
  geom_text( aes( x = theo, y = samp+.1, label = noms.eff), size = 2) +
  stat_qq(alpha = .5, distribution = qnorm) + 
  ylab("efectos") + xlab("escala normal") 
gg.qq


# b) QQPLOT SEMI NORMAL separado por niveles del diseño
gg.semi <- ggplot( df.eff.sp, aes(x = theo.semi, y = samp.semi) ) + 
  geom_point(alpha = .5) + 
  facet_wrap(~tipo, scale = "free_y") +
  geom_text( aes( y = samp.semi+.1, label = noms.eff), size = 2) + 
  ylab("efectos") + xlab("escala normal") 
gg.semi
 

# c) LENTH 
gg.lenth <- ggplot(df.eff.sp, aes( x= noms.eff, y = effects)) + 
  facet_wrap(~tipo, scale = "free") +
  geom_pointrange(  aes(ymin = 0, ymax = effects) ) + 
  geom_hline( aes( yintercept = c(ME, -ME) ), size = .8) + 
  geom_hline( aes( yintercept = c(SME, -SME) ), linetype = 2) + 
  geom_hline( yintercept = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1,size = 7)) +
  ylab("efectos") + xlab("factor") 
gg.lenth


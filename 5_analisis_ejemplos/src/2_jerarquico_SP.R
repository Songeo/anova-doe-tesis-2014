# library(ProjectTemplate)
# load.project()

# Dos sigmas (main, sub)

# 1) Modelo jerárquico 
source('src/2_auxiliar_base.R')

# cambiar los niveles
mat.main.0 <- data.frame( cbind( as.matrix(base[, LETTERS[selec.main.pos]]) ,
                                 mat.error.main), check.names=F)
mat.sub.0 <- data.frame( cbind(as.matrix(base[, c('m', 'n', 'o')]),
                               mat.sub.comb,
                               mat.error.sub), check.names=F)
for(k in names(mat.main.0)){
  mat.main.0[, k] <- car::recode(mat.main.0[, k], "0 = -1; 1 = 1")
}
for(k in names(mat.sub.0)){
  mat.sub.0[, k] <- car::recode(mat.sub.0[, k], "0 = -1; 1 = 1")
}

# Datos
y <- base$percentage
mat.main <- mat.main.0
mat.sub <- mat.sub.0

n <- nrow(base)
n.main <- ncol(mat.main)
n.sub <- ncol(mat.sub)

# Tabla de betas
mat.s <- jags.sp.2c$BUGSoutput$summary
tab.b <- data.frame( mat.s[grep("^b\\.",rownames(mat.s), value = T), ] )
tab.b$efectos <- c(colnames(mat.main), colnames(mat.sub))
tab.b$efectos <- factor(tab.b$efectos, levels = rev(tab.b$efectos))

tab.b.gg <- tab.b[ ! 1:nrow(tab.b) %in% grep(".error", tab.b$efectos), ]
tab.b.gg$tipo <- "sublote"
tab.b.gg$tipo[tab.b.gg$efectos %in% colnames(mat.main)] <- "lote principal"

gg.betas <- ggplot(tab.b.gg, aes(x = efectos, y = X50., ymin = X25., ymax = X75.)) + 
  geom_point(size = 2) + 
  geom_linerange(aes(ymin = X2.5., ymax = X97.5.), alpha = .7) +
  geom_linerange(size = .8) + 
  coord_flip() + geom_hline(yintercept = 0, linetype = 2) +
  xlab(NULL) +
  ylab("coeficientes sobre\nporcentaje de contracción") + 
  theme(axis.text.y = element_text(size = 10) )
gg.betas
# library(ProjectTemplate)
# load.project()

# cambio parametrización -cambio de modelo
# sigma por nive del diseño


# 1) Modelo jerárquico 
source("src/2_auxiliar_base.R")

# cambiar los niveles
mat.main.0 <- data.frame( cbind( as.matrix(base[, LETTERS[selec.main.pos]]) 
                              , mat.error.main
                                 ), check.names=F)
mat.sub.0 <- data.frame( cbind(as.matrix(base[, c('m', 'n', 'o')]),
                               mat.sub.comb
                               ,mat.error.sub
                               ), check.names=F)
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



# 1) Modelo jerárquico
sink(file = "doc/modelo_splitplot_jerarquico_2c.bug")
cat('
    model{
    # Modelo 
    for(i in 1:n){
      y[i] ~ dnorm(y.hat[i], tau.y)
      y.hat[i] <- inprod(mat.main[i, ], b.main) +
                  inprod(mat.sub[i, ], b.sub)  
      e.y[i] <- y[i] - y.hat[i]
    }
    
    # Priori Main Plot
    for(k in 1:n.main){
    b.main[k] ~ dnorm(0, tau.main)
    }
    tau.main <- pow(sigma.main, -2)
    sigma.main ~ dunif(0,10)  
    
    for(k in 1:n.sub){
    b.sub[k] ~ dnorm(0, tau.sub)
    }  
    tau.sub <- pow(sigma.sub, -2)
    sigma.sub ~ dunif(0,10)      
        
    # Hiperparámetros
    tau.y <- pow(sigma.error, -2)
    sigma.error ~ dunif(0,10)
    }'
)
sink()

# Modelo 
dat.sp <- list("y", 
               'n','n.main','n.sub',
               'mat.main', 'mat.sub'
)
inits.sp <- function (){
  list (sigma.error = runif(1), 
        sigma.main = runif(1),
        sigma.sub = runif(1)
  ) 
}
param.sp <- c('y.hat', 'sigma.error',
              'b.main', 'sigma.main',
              'b.sub', 'sigma.sub'
)
jags.sp.2c <- jags(data = dat.sp, 
                      inits = inits.sp,  
                      parameters.to.save = param.sp,
                      n.iter = 50000,
                      n.chains = 3,
                      n.thin = 20,
                      model.file = "doc/modelo_splitplot_jerarquico_2c.bug", 
                      progress.bar = "text")

jags.sp.2c$BUGSoutput$DIC
plot(jags.sp.2c)

# pdf("graphs/jags_2c.pdf", width = 7, height=9)
# plot(jags.sp.2c)
# dev.off()
qplot(x = y, y = jags.sp.2c$BUGSoutput$mean$y.hat, 
      label = 1:length(y), geom = "text")
# cache("jags.sp.2c")


# Tabla de betas
mat.s <- jags.sp.2c$BUGSoutput$summary
tab.b <- data.frame( mat.s[grep("^b\\.",rownames(mat.s), value = T), ] )
tab.b$efectos <- c(colnames(mat.main), colnames(mat.sub))
tab.b$efectos <- factor(tab.b$efectos, levels = tab.b$efectos)

tab.b.gg <- tab.b[ ! 1:nrow(tab.b) %in% grep(".error", tab.b$efectos), ]
gg.2c <- ggplot(tab.b.gg, aes(x = efectos, y = X50., ymin = X25., ymax = X75.)) + 
  geom_pointrange() +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point( size = 1)
gg.2c
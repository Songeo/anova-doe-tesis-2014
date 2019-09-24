# library(ProjectTemplate)
# load.project()

# cambio parametrización -cambio de modelo
# una sigma por grupo de factores (main, sub, interaccion,error main, error sub)

# 1) Modelo jerárquico 
source("src/2_auxiliar_base.R")

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
mat.main <- as.matrix( mat.main.0[, LETTERS[selec.main.pos]] )
mat.main.error <- as.matrix(mat.main.0[, "mat.error.main"])
mat.sub <- as.matrix(mat.sub.0[, c('m', 'n', 'o')]) 
mat.sub.int <- mat.sub.0[, colnames(mat.sub.comb)]
mat.sub.error <- as.matrix(mat.sub.0[, "mat.error.sub"])

n <- nrow(base)
n.main <- ncol(mat.main)
n.sub <- ncol(mat.sub)
n.sub.int <- ncol(mat.sub.int)


# 1) Modelo jerárquico
sink(file = "doc/modelo_splitplot_jerarquico_2d.bug")
cat('
model{
    # Modelo 
    for(i in 1:n){
      y[i] ~ dnorm(y.hat[i], tau.y)
      y.hat[i] <- inprod(mat.main[i, ], b.main) +
                  inprod(mat.main.error[i,], b.main.error) + 
                  inprod(mat.sub[i, ], b.sub)  + 
                  inprod(mat.sub.int[i, ], b.sub.int) +
                  inprod(mat.sub.error[i, ], b.sub.error) 
      e.y[i] <- y[i] - y.hat[i]
    }

    # Priori Main Plot
    for(k in 1:n.main){
      b.main[k] ~ dnorm(0, tau.main)
    }
    tau.main <- pow(sigma.main, -2)
    sigma.main ~ dunif(0,10)  

    b.main.error ~ dnorm(0, tau.main.error)
    tau.main.error <- pow(sigma.main.error, -2)
    sigma.main.error ~ dunif(0,10)  

    for(k in 1:n.sub){
      b.sub[k] ~ dnorm(0, tau.sub)
    }  
    tau.sub <- pow(sigma.sub, -2)
    sigma.sub ~ dunif(0,10)      

    for(k in 1:n.sub.int){
      b.sub.int[k] ~ dnorm(0, tau.sub.int)
    }  
    tau.sub.int <- pow(sigma.sub.int, -2)
    sigma.sub.int ~ dunif(0,10)    

    b.sub.error ~ dnorm(0, tau.sub.error)
    tau.sub.error <- pow(sigma.sub.error, -2)
    sigma.sub.error ~ dunif(0,10)  

    # Hiperparámetros
    tau.y <- pow(sigma.error, -2)
    sigma.error ~ dunif(0,10)
    }'
)
sink()

# Modelo 
dat.sp <- list("y", 
               'n','n.main','n.sub','n.sub.int',
               'mat.main', 'mat.sub', 'mat.sub.int',
               'mat.main.error', 'mat.sub.error'
)
inits.sp <- function (){
  list (sigma.error = runif(1), 
        sigma.main = runif(1),
        sigma.main.error = runif(1),
        sigma.sub = runif(1),
        sigma.sub.int = runif(1),
        sigma.sub.error = runif(1)
  ) 
}
param.sp <- c('y.hat', 'sigma.error',
              'b.main', 'sigma.main',
              'b.sub', 'sigma.sub',
              'b.sub.int', 'sigma.sub.int',
              'b.sub.error', 'sigma.sub.error', 
              'b.main.error', 'sigma.main.error'
)
jags.sp.2d <- jags(data = dat.sp, 
               inits = inits.sp,  
                parameters.to.save = param.sp,
                n.iter = 30000,
                n.chains = 3,
                n.thin = 20,
                model.file = "doc/modelo_splitplot_jerarquico_2d.bug", 
                progress.bar = "text")

jags.sp.2d$BUGSoutput$DIC
plot(jags.sp.2d)

# pdf("graphs/jags_2d.pdf", width = 7, height=9)
# plot(jags.sp.2d)
# dev.off()
qplot(x = y, y = jags.sp.2d$BUGSoutput$mean$y.hat, 
      label = 1:length(y), geom = "text")
# cache("jags.sp.2d")

# Tabla de betas
mat.s <- jags.sp.2d$BUGSoutput$summary
tab.b <- data.frame( mat.s[grep("^b\\.",rownames(mat.s), value = T), ] )
tab.b$efectos <- c(colnames(mat.main), "main.error", 
                   colnames(mat.sub), "sub.error", colnames(mat.sub.int))
tab.b$efectos <- factor(tab.b$efectos, levels = tab.b$efectos)

tab.b.gg <- tab.b[ ! 1:nrow(tab.b) %in% grep(".error", tab.b$efectos), ]
gg.2d <- ggplot(tab.b.gg, aes(x = efectos, y = X50., ymin = X25., ymax = X75.)) + 
  geom_pointrange() + coord_flip() + geom_hline(yintercept = 0, linetype = 2) + 
  geom_point( size = 1)
gg.2d

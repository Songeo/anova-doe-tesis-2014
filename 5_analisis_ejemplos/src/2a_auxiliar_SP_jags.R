
# parametro por nivel
# sigma por nivel por factor


# 1) Modelo jerárquico 
source("src/2_auxiliar_base.R")

# Datos
y <- base$percentage
mat.main <- as.matrix(base[, LETTERS[selec.main.pos]]) + 1
mat.main.int <- mat.main.comb + 1
mat.sub <- as.matrix(base[, c('m', 'n', 'o')[selec.sub.pos]]) + 1
mat.sub.int <- mat.sub.comb + 1
mat.main.error <- mat.error.main + 1
mat.sub.error <- mat.error.sub + 1

n <- nrow(base)
n.main <- ncol(mat.main)
n.sub <- ncol(mat.sub)
n.main.int <- ncol(mat.main.int)
n.sub.int <- ncol(mat.sub.int)


# 1) Modelo jerárquico
sink(file = "doc/modelo_splitplot_jerarquico_2a.bug")
cat('
model{
    # Modelo 
    for(i in 1:n){

      y[i] ~ dnorm(y.hat[i], tau.y)
      
      # definición de coeficientes 
      for(k in 1:n.main){
        b.main.aux[i,k] <- b.main[k,mat.main[i,k]]
      }
      for(k in 1:n.sub){
        b.sub.aux[i,k] <- b.sub[k,mat.sub[i,k]]
      }
      for(k in 1:n.sub.int){
        b.sub.int.aux[i,k] <- b.sub.int[k,mat.sub.int[i,k]]
      }

      y.hat[i] <- sum(b.main.aux[i,]) +
                  b.main.error[mat.main.error[i]] +  
                  sum(b.sub.aux[i,]) + 
                  sum(b.sub.int.aux[i,]) + 
                  b.sub.error[mat.sub.error[i]]

      e.y[i] <- y[i] - y.hat[i]
    }
    
    # Priori Main Plot
    for(k in 1:n.main){
      for(j in 1:2){
        b.main[k, j] ~ dnorm(0, tau.main[k])
      }
      tau.main[k] <- pow(sigma.main[k], -2)
      sigma.main[k] ~ dunif(0,10)
      sd.main[k] <- sd(b.main[k,])
    }
    for(j in 1:2){
      b.main.error[j] ~ dnorm(0, tau.main.error)
    }
    tau.main.error <- pow(sigma.main.error, -2)
    sigma.main.error ~ dunif(0,10)
    sd.main.error <- sd(b.main.error)

    # Priori Sub Plot
    for(k in 1:n.sub){
      for(j in 1:2){
        b.sub[k,j] ~ dnorm(0, tau.sub[k])
      }
      tau.sub[k] <- pow(sigma.sub[k], -2)
      sigma.sub[k] ~ dunif(0,10)
      sd.sub[k] <- sd(b.sub[k,])
    }
    for(k in 1:n.sub.int){
      for(j in 1:2){
        b.sub.int[k,j] ~ dnorm(0, tau.sub.int[k])
      }
      tau.sub.int[k] <- pow(sigma.sub.int[k], -2)
      sigma.sub.int[k] ~ dunif(0,10)
      sd.sub.int[k] <- sd(b.sub.int[k,])
    }
    for(j in 1:2){
      b.sub.error[j] ~ dnorm(0, tau.sub.error)
    }
    tau.sub.error <- pow(sigma.sub.error, -2)
    sigma.sub.error ~ dunif(0,10)
    sd.sub.error <- sd(b.sub.error)

    # Hiperparámetros
    tau.y <- pow(sigma.error, -2)
    sigma.error ~ dunif(0,10)
    sd.error <- sd(e.y[])
    }'
)
sink()


# Modelo 
dat.sp <- list("y", 'n','n.main','n.sub',
               'mat.main', 'mat.sub',
               'mat.sub.int', 'n.sub.int',
               'mat.main.error', 'mat.sub.error'
)
inits.sp <- function (){
  list (sigma.error = runif(1), 
        sigma.main = runif(ncol(mat.main)),
#         sigma.main.int = runif(ncol(mat.main.int)),
        sigma.main.error = runif(1),
        sigma.sub = runif(ncol(mat.sub)),
        sigma.sub.int = runif(ncol(mat.sub.int)),
        sigma.sub.error = runif(1)
  ) 
}
param.sp <- c('y.hat', 'sigma.error', 'sd.error',
              'b.main', 'sigma.main', 'sd.main',
              'b.main.error',  'sigma.main.error', 'sd.main.error',
              'b.sub', 'sigma.sub', 'sd.sub',
              'b.sub.int', 'sigma.sub.int', 'sd.sub.int',
              'b.sub.error',  'sigma.sub.error', 'sd.sub.error'
)
jags.sp.2a <- jags(data = dat.sp, 
                inits = inits.sp,  
                parameters.to.save = param.sp,
                n.iter = 90000,
                n.chains = 3,
                n.thin = 20,
                model.file = "doc/modelo_splitplot_jerarquico_2a.bug", 
                progress.bar = "text")

jags.sp.2a$BUGSoutput$DIC
plot(jags.sp.2a)


# pdf("graphs/jags_2a.pdf", width = 7, height=9)
# plot(jags.sp.2a)
# dev.off()
qplot(x = y, y = jags.sp.2a$BUGSoutput$mean$y.hat, 
      label = 1:length(y), geom = "text")
# cache("jags.sp.2a")

# SD
mat.s <- jags.sp.2a$BUGSoutput$summary
tab.sd <- data.frame( mat.s[grep("sd",rownames(mat.s), value = T), ] )
tab.sd$efectos <- c("error", colnames(mat.main), 
                    "error main", colnames(mat.sub), 
                    "error sub", colnames(mat.sub.int))
tab.sd$efectos <- factor(tab.sd$efectos, levels = tab.sd$efectos)
gg.2a <- ggplot(tab.sd, aes(x = efectos, y = mean, ymin = X25., ymax = X75.)) + 
  geom_pointrange() + coord_flip()
gg.2a
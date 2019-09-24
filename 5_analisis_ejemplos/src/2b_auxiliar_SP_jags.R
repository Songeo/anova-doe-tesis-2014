# Necesita la primera parte de 2_jerarquico_SP.R para los modelos

# parametro por nivel
# sigma por nivel por factor (no jerárquico)


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

# 2) NO jerárquico
sink(file = "doc/modelo_splitplot_nojerarquico_2b.bug")
cat('
    model{
    # Modelo 
    for(i in 1:n){
    y[i] ~ dnorm(y.hat[i], tau.y)
    
    for(k in 1:7){
    b.main.aux[i,k] <- b.main[k,mat.main[i,k]]
    }
    for(k in 1:3){
    b.sub.aux[i,k] <- b.sub[k,mat.sub[i,k]]
    }
    for(k in 1:n.main.int){
    b.main.int.aux[i,k] <- b.main.int[k,mat.main.int[i,k]]
    }
    for(k in 1:n.sub.int){
    b.sub.int.aux[i,k] <- b.sub.int[k,mat.sub.int[i,k]]
    }
    
    y.hat[i] <- sum(b.main.aux[i,]) +
    sum(b.sub.aux[i,]) + 
    sum(b.main.int.aux[i,]) +
    sum(b.sub.int.aux[i,])
    e.y[i] <- y[i] - y.hat[i]
    }
    
    # Priori
    for(k in 1:7){
    for(j in 1:2){
    b.main[k, j] ~ dnorm(0, tau.main[k])
    }
    tau.main[k] <- pow(sigma.main[k], -2)
    sigma.main[k] <- 10
    sd.main[k] <- sd(b.main[k,])
    }
    for(k in 1:3){
    for(j in 1:2){
    b.sub[k,j] ~ dnorm(0, tau.sub[k])
    }
    tau.sub[k] <- pow(sigma.sub[k], -2)
    sigma.sub[k] <- 10
    sd.sub[k] <- sd(b.sub[k,])
    }
    for(k in 1:n.main.int){
    for(j in 1:2){
    b.main.int[k,j] ~ dnorm(0, tau.main.int[k])
    }
    tau.main.int[k] <- pow(sigma.main.int[k], -2)
    sigma.main.int[k] <- 10
    sd.main.int[k] <- sd(b.main.int[k,])
    }
    for(k in 1:n.sub.int){
    for(j in 1:2){
    b.sub.int[k,j] ~ dnorm(0, tau.sub.int[k])
    }
    tau.sub.int[k] <- pow(sigma.sub.int[k], -2)
    sigma.sub.int[k] <- 10
    sd.sub.int[k] <- sd(b.sub.int[k,])
    }
    
    # Hiperparámetros
    tau.y <- pow(sigma.error, -2)
    sigma.error <- 10
    sd.error <- sd(e.y[])
    }'
)
sink()

dat.sp <- list("y", 'n',
               'mat.main', 'mat.sub',
               'mat.sub.int', 'n.sub.int',
               'mat.main.int', 'n.main.int'
)
param.sp <- c('y.hat', 'sigma.error', 'sd.error',
              'b.main', 'sigma.main', 'sd.main',
              'b.sub', 'sigma.sub', 'sd.sub',
              'b.sub.int', 'sigma.sub.int', 'sd.sub.int',
              'b.main.int',  'sigma.main.int', 'sd.main.int'
)
jags.sp.2b <- jags(data = dat.sp, 
                parameters.to.save = param.sp,
                n.iter = 20000,
                n.chains = 3,
                model.file = "doc/modelo_splitplot_nojerarquico_2b.bug", 
                progress.bar = "text")
jags.sp.2b$BUGSoutput$DIC
plot(jags.sp.2b)
plot(jags.sp.2b$BUGSoutput$mean$y.hat, y)


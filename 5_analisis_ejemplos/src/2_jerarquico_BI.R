### ANOVA BLOQUES INCOMPLETOS - MULTILEVEL
library(ProjectTemplate)
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


# Modelo
y <- dat$y
maquina <- as.numeric(dat$maquina)
catalizador <- as.numeric(dat$catalizador)
n <- length(y)
n.maq <- length(unique(maquina))
n.cat <- length(unique(catalizador))

sink(file = "doc/modelo_bi_jerarquico.bug")
cat('
model{
    # Modelo
    for(i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- alpha + b.maquina[maquina[i]] + b.catalizador[catalizador[i]] 
    e.y[i] <- y[i] - y.hat[i]     
    efecto[i] <- alpha + b.catalizador[catalizador[i]]
    }
    
    # Priori
    for(k in 1:n.maq){
    b.maquina[k] ~ dnorm(0, tau.maquina) 
    }
    for(l in 1:n.cat){
    b.catalizador[l] ~ dnorm(0, tau.catalizador)
    }
    
    # Hiperparámetros
    alpha ~ dnorm(0, 0.0001)    
    tau.y <- pow(sigma.y, -2)
    sigma.y ~ dunif(0,100)
    tau.maquina <- pow(sigma.maquina, -2)
    sigma.maquina ~ dunif(0,100)
    tau.catalizador <- pow(sigma.catalizador, -2)
    sigma.catalizador ~ dunif(0,100)
    
    # Cálculos extra  
    sd.maquina <- sd(b.maquina[])
    sd.catalizador <- sd(b.catalizador[])
    sd.error <- sd(e.y[])
}
    ')
sink()
dat.aov <- list("y", "maquina", "catalizador", "n", "n.cat", "n.maq")
inits.aov <- function (){
  list (sigma.maquina = runif(1), 
        sigma.catalizador = runif(1),
        sigma.y = runif(1))
}
param.aov <- c("alpha", "b.maquina", "b.catalizador",
               "sigma.maquina", "sigma.catalizador", "sigma.y",
               "sd.maquina", "sd.catalizador", "sd.error", "y.hat", "efecto")

jags.bi <- jags(data = dat.aov, 
                  inits = inits.aov,  
                  parameters.to.save = param.aov,
                  n.iter =80000,
                  n.chains = 3,
                  model.file = "doc/modelo_bi_jerarquico.bug", 
                  progress.bar = "text")
plot(jags.bi)
traceplot(jags.bi)
# cache('jags.bi')



aux <- jags.bi$BUGSoutput$summary
aux.smaq <- aux[!1:nrow(aux) %in% grep('maquina', rownames(aux)), ]
# SD
c.nivs.sd <- c("Catalizador (Df 3)", "Error (Df 5)")
gg.sd <- GG.Dif.bi(aux = aux.smaq, nom= "sd.", nivs = c.nivs.sd, ord.nivs= rev(c.nivs.sd))
gg.sd + xlab(NULL) + ylab(NULL)
data.frame(aux[grep("sd.", rownames(aux)), ])

# Sigma
c.nivs.sigma <- c("Catalizador (Df 3)", "Error (Df 5)")
gg.sigma <- GG.Dif.bi(aux = aux.smaq, nom= "sigma", nivs = c.nivs.sigma, ord.nivs = rev(c.nivs.sigma))
gg.sigma+ xlab(NULL) + ylab(NULL)
data.frame(aux[grep("sigma.", rownames(aux)), ])



# catalizador
gg.cat <- GG.Dif.bi(aux = aux, nom= "b.catalizador", nivs = levels(dat$catalizador))
gg.cat


# prob catalizador
tt <- jags.bi$BUGSoutput$sims.list$b.catalizador
tab <- data.frame( 100*prop.table( table(apply(tt, 1, which.max)) ) )
names(tab) <- c("Tratamiento", "Probabilidad (%)")
tab$Tratamiento <- factor(1:4, levels = 1:4, labels = paste('catalizador', 1:4))
tab.cat <- tab
print(xtable(tab.cat, align = 'ccc', digits = c(0,0,0), caption = "Probabilidad de Mayor Impacto por Catalizador", label = 'prob_cat'), include.rownames = F)


# Probabilidad entre Tratamientos
combs <- expand.grid(1:n.cat, 1:n.cat)
combs$probs <- sapply(1:nrow(combs), function(i){
  round(100*sum(tt[, combs[i, 1]] > tt[, combs[i, 2]])/nrow(tt))
})
tab.prob <- dcast(data= combs, Var1 ~Var2, value.var="probs")

tab.prob$Var1 <- paste('catalizador', tab.prob$Var1)
names(tab.prob) <- paste('catalizador', names(tab.prob))
names(tab.prob)[1] <- ""
print(xtable(tab.prob, align = rep('c', ncol(tab.prob)+1), digits = rep(0, ncol(tab.prob)+1), caption = "Probabilidad de Impacto Comparativo entre Catalizadores", label = 'prob_comp_cat'), include.rownames = F)

library(ProjectTemplate)
load.project()

datos <- BD.montgomery.pilas
head(datos)
datos$tipo.material <- factor(datos$tipo.material)
datos$temperatura <- factor(datos$temperatura)

# Modelo
y <- datos$vida
tipo <- as.numeric( datos$tipo.material )
temp <- as.numeric( datos$temperatura )
n <- nrow(datos)

sink(file = "doc/modelo_twoway_jerarquico.bug")
cat('
model{
  # Modelo 
  for(i in 1:n){
    y[i] ~ dnorm(y.hat[i], tau.y)
    y.hat[i] <- alpha + b.tipo[tipo[i]] + b.temp[temp[i]] + b.inter[tipo[i], temp[i]]
    e.y[i] <- y[i] - y.hat[i]
  }

  # Priori
  for(k in 1:3){
    b.tipo[k] ~ dnorm(0, tau.tipo)
  }
  for(j in 1:3){
    b.temp[j] ~ dnorm(0, tau.temp)
  }
  for(k in 1:3){
    for(j in 1:3){
      b.inter[k,j] ~ dnorm(0, tau.inter)
    }
  }

  # Hiperparametros
  alpha ~ dnorm(0, 0.0001)    
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif(0, 100)
  tau.tipo <- pow(sigma.tipo, -2)
  sigma.tipo ~ dunif(0, 100)
  tau.temp <- pow(sigma.temp, -2)
  sigma.temp ~ dunif(0, 100)
  tau.inter <- pow(sigma.inter, -2)
  sigma.inter ~ dunif(0, 100)

  # Extra
  sd.tipo <- sd(b.tipo[])
  sd.temp <- sd(b.temp[])
  sd.inter <- sd(b.inter[,])
  sd.error <- sd(e.y[])
} ')
sink()

dat.twt <- list("y", "tipo", "temp", "n")
inits.twt <- function (){
  list (alpha = runif(1),
        sigma.tipo = runif(1), 
        sigma.temp = runif(1),
        sigma.inter = runif(1),
        sigma.y = runif(1) )
}
param.twt <- c("alpha", "b.tipo", "b.temp", "b.inter",
               "sigma.tipo", "sigma.temp", "sigma.inter", "sigma.y",
               "sd.tipo", "sd.temp", "sd.inter",  "sd.error", "y.hat")
jags.twt <- jags(data = dat.twt, 
                  inits = inits.twt,  
                  parameters.to.save = param.twt,
                  n.iter = 50000,
                  n.chains = 3,
                  model.file = "doc/modelo_twoway_jerarquico.bug", 
                  progress.bar = "text")
plot(jags.twt)
traceplot(jags.twt)
plot(jags.twt$BUGSoutput$mean$y.hat, y)
# cache('jags.twt')



aux <- jags.twt$BUGSoutput$summary
# SD
c.nivs.sd <- c("Error (Df 27)", "Interacción (Df 4)", "Temperatura (Df 2)", "Tipo (Df 2)")
gg.sd <- GG.Dif.TW(aux = aux, nom= "sd.", nivs = c.nivs.sd)
gg.sd + xlab(NULL) + ylab(NULL)
data.frame(aux[grep("sd.", rownames(aux)), ])


# Sigma
c.nivs.sigma <- c("Interacción (Df 4)", "Temperatura (Df 2)", "Tipo (Df 2)", "Error (Df 27)")
ord.nivs.sig <- c.nivs.sd
gg.sigma <- GG.Dif.TW(aux = aux, nom= "sigma.", nivs = c.nivs.sigma, ord.nivs = ord.nivs.sig)
gg.sigma+ xlab(NULL) + ylab(NULL)
data.frame(aux[grep("sigma.", rownames(aux)), ])




# Tipo
gg.tipo <- GG.Dif.TW(aux = aux, nom= "b.tipo", nivs = levels(datos$tipo.material))
gg.tipo
# Temperatura
gg.temp <- GG.Dif.TW(aux = aux, nom= "b.temp", nivs = levels(datos$temperatura))
gg.temp
#Interaccion
aux.nivs <- expand.grid(levels(datos$tipo.material), levels(datos$temperatura))
c.nivs <- paste(aux.nivs[,1], aux.nivs[,2], sep = " - ")
gg.int <- GG.Dif.TW(aux = aux, nom= "b.int", nivs = c.nivs)
gg.int
data.frame(aux[grep("b.int", rownames(aux)), ])


# Probabilidad de Coeficientes por Tratamiento
coefs.extrac <- c("b.temp", "b.tipo", "b.inter")
probs.ind.l <- lapply(coefs.extrac, function(sel){
  tt <- jags.twt$BUGSoutput$sims.list[[sel]]
  tab <- data.frame( 100*prop.table( table(apply(tt, 1, which.max)) ) )
  names(tab) <- c("Tratamiento", "Probabilidad (%)")
  tab
})
names(probs.ind.l) <- coefs.extrac

pr.tipo <-probs.ind.l[["b.tipo"]]
pr.tipo$Tratamiento <- paste("Tipo", pr.tipo$Tratamiento)
print(xtable(pr.tipo, digits = c(0,0,0), align="ccc", caption = "Probabilidad de Mayor Impacto Tipo de Material", label = "prob_tipo"), include.rownames = F)


pr.temp <-probs.ind.l[["b.temp"]]
pr.temp$Tratamiento <- factor(pr.temp$Tratamiento, levels = 1:3, labels = c('15º', '70º', '125º'))
print(xtable(pr.temp, digits = c(0,0,0), align="ccc", caption = "Probabilidad de Mayor Impacto Temperatura", label = "prob_temp"), include.rownames = F)



pr.int <-probs.ind.l[["b.inter"]]
aux.nivs <- expand.grid(levels(datos$tipo.material), levels(datos$temperatura))
c.nivs <- paste(aux.nivs[,1], aux.nivs[,2], sep = " - ")
pr.int$Tratamiento <- c.nivs
print(xtable(pr.int, digits = c(0,0,0), align="ccc", caption = "Probabilidad de Mayor Impacto Interacción de Factores", label = "prob_inter"), include.rownames = F)


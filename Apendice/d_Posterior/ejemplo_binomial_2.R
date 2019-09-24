
set.seed(80)
p <- runif(1)
set.seed(102872)
n <- rbinom(1,20,p)

#Probabilidad de moneda sesgada a sol?
# P(theta > 0.5)


# Analítico
posterior.fabrica <- function(n){
	function(x) {
		dbeta(x, n+1, 20-n+1)
	}
}
mi.posterior <- posterior.fabrica(n)
curve(mi.posterior, from = 0, to = 1)
1 - pbeta(0.5,n+1,20-n+1)



##Método grid, conociendo posterior hasta constante
## de proporcionalidad.
grid.theta <- seq(0,1,0.001)
eval.posterior <- (grid.theta^n)*((1-grid.theta)^(20-n))
eval.post.norm <- eval.posterior/sum(eval.posterior)
plot(eval.post.norm , type='l', mail = 'Grid de posterior')
sum(eval.post.norm[grid.theta>0.5])



## Monte Carlo
sims.posterior <- rbeta(1e6, n+1,20-n+1)
hist(sims.posterior, breaks=80, xlim=c(0,1), freq=FALSE)
sum(sims.posterior > 0.5)/1e6



## Metropolis: salto a la derecha o la izquierda en step unidades
# esto devuelve la posterior hasta una cte. de prop.
f.1 <- function(theta, n){
	ifelse(theta > 0 & theta < 1, (theta^n)*(1-theta)^(20-n),0)
}
step <- 0.3
theta.actual <- runif(1)
n.sims <- 1e6
theta.sim <- rep(NA,n.sims)
tasa.acept.acum <- rep(NA, n.sims)
no.aceptados <- 0
for(i in 1:n.sims){
	direccion.prop <- runif(1,-1,1)
	theta.prop <- theta.actual + step*direccion.prop
	prob.aceptar <- min(f.1(theta.prop, n)/f.1(theta.actual, n), 1)
	resultado <- runif(1)
	if(resultado < prob.aceptar){
		theta.actual <- theta.prop
		no.aceptados <- no.aceptados + 1
	}
	theta.sim[i] <- theta.actual
	tasa.acept.acum[i] <- no.aceptados/i
}
plot(tasa.acept.acum[1:10000])
plot(theta.sim[1:1000], ylim=c(0,1), type="l")
plot(theta.sim, type="l")
sum(theta.sim>0.5)/n.sims
#medias
mean(theta.sim)
sd(theta.sim)


# Con JAGS  un parametro
library(R2jags)
sink(file = "ejemplo_jags.bugs")
cat('
model{
  y ~ dbin(p, 20)
  p ~ dunif(0, 1)
}
')
sink()
jags.inits <- function(){
  list("p" = runif(1))
}
jags.params <- c('p')
y <- n
jags_moneda <- jags(model.file = 'ejemplo_jags.bugs',
                data = list('y' = y),
                inits=jags.inits,
                parameters.to.save = jags.params,
                n.iter=1000, 
                n.burnin=0,
                n.thin=1,
                n.chains = 3)
plot(jags_moneda)
traceplot(jags_moneda)
simulaciones <- (jags_moneda$BUGSoutput$sims.list$p)
mean(simulaciones>0.5)


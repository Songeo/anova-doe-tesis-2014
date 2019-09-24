#' Muestreo de Gibbs para Modelo jerárquicos

#' Suponemos tres monedas M1,M2,M3 extraídas
#' de una distribución beta(alpha,alpha)
#' suponemos alpha ~ exp(1)

#' theta_i dada alfa y otras thetas es beta(alpha,alpha)
#' alpha dada thetas la simulamos con Metropolis
#' usando propuesta uniforme.

set.seed(9192988)
m <- 5
n.volados <- 30
alpha <- rexp(1)
theta <- rbeta(m, alpha, alpha)
y <- rbinom(m, size = n.volados, prob = theta)

#' Gibbs
n.sims <- 20000
theta.sims <- matrix(ncol=m, nrow=n.sims)
alpha.sims <- rep(NA, length = n.sims)
theta.sims[1,] <- rep(0.5, m)
alpha.sims[1] <- runif(1)

full.cond.alpha.log <- function(alpha, theta){
  ifelse(alpha> 0 & all(theta < 1) & all(theta > 0), 
   sum( -lbeta(alpha, alpha) + (alpha -1)*log(theta + 0.0001) + (alpha -1)*log(1-theta+ 0.0001) ) - alpha, 
         -Inf)
}

Metropos.Alpha <- function(alpha.actual, theta){
    alpha.prop <- rnorm(1, alpha.actual, 0.1)
    f.alpha.prop <- full.cond.alpha.log(alpha = alpha.prop, theta = theta) 
    f.alpha.act <- full.cond.alpha.log(alpha = alpha.actual, theta = theta) 
    ratio.alpha <- exp( f.alpha.prop-f.alpha.act )
    prob.aceptar <- min(ratio.alpha, 1)
    #print(f.alpha.prop)
    #print(f.alpha.act)
    #print(ratio.alpha)
    resultado <- runif(1)
    if( resultado < prob.aceptar ){
      alpha.actual <- alpha.prop
    }  
    alpha.actual
}

for(i in 2:n.sims){
  alpha.act <- alpha.sims[i-1]
  
  # paso de simulacion para theta
  theta.sims[i, ] <- rbeta(m, y + alpha.act, n.volados - y + alpha.act)
	theta.act <- theta.sims[i, ]
  
	# paso de Metropolis para alpha
	alpha.sims[i] <- Metropos.Alpha(alpha.actual = alpha.act, theta = theta.act)
}

plot(alpha.sims, type='l')
quantile(alpha.sims, probs=c(0.05,0.95))
alpha
hist(alpha.sims,100)
plot(apply(theta.sims,2,mean), theta)

# Grafica
library(ggplot2)
df.sims.monedas <- data.frame(parametros = c('alpha', paste('theta', 1:m)), 
                              real = c(alpha, theta))
df.sims.monedas$sims <- c( mean(alpha.sims), apply(theta.sims, 2, mean) )

df.sims.monedas$X2.5. <- c( quantile(alpha.sims, probs=c(0.025)), 
                            apply(theta.sims, 2, function(col){ quantile(col, probs=c(0.025))}) )
df.sims.monedas$X97.5. <- c( quantile(alpha.sims, probs=c(0.975)), 
                             apply(theta.sims, 2, function(col){ quantile(col, probs=c(0.975))}) )
df.sims.monedas$X5. <- c( quantile(alpha.sims, probs=c(0.05)), 
                          apply(theta.sims, 2, function(col){ quantile(col, probs=c(0.05))}) )
df.sims.monedas$X95. <- c( quantile(alpha.sims, probs=c(0.95)), 
                           apply(theta.sims, 2, function(col){ quantile(col, probs=c(0.95))}) )
ggplot(df.sims.monedas, aes( x= parametros, y = sims) ) + 
  geom_pointrange( aes(ymin = X5., ymax = X95.), size = .8) + 
  geom_point(aes(x = parametros, y = real), size = 2, colour = 'red') + 
  coord_flip()



# Con JAGS  m+1 parametros
library(R2jags)
sink(file = "ejemplo_gibbs_monedas.bugs")
cat('
model{
  for(i in 1:length(y)){
    y[i] ~ dbin(theta.p[i], r)
    theta.p[i] ~ dbeta(alpha.p, alpha.p)
  }
  alpha.p ~ dexp(1)
}
')
sink()
jags.inits.monedas <- function(){
  list("alpha.p" = runif(1))
}
jags.params.monedas <- c('alpha.p', 'theta.p')
jags_m_monedas <- jags(model.file = 'ejemplo_gibbs_monedas.bugs',
                    data = list('y' = y, 'r' = n.volados),
                    inits=jags.inits.monedas,
                    parameters.to.save = jags.params.monedas,
                    n.iter=10000, 
                    n.burnin=0,
                    n.thin=1,
                    n.chains = 3)
tab.sum <- jags_m_monedas$BUGSoutput$summary[-2, 1:7]
df.sum.monedas <- data.frame(parametros = rownames(tab.sum), tab.sum, real = c(alpha,theta))
df.sum.monedas

# Gráficas
plot(jags_m_monedas)

ggplot(df.sum.monedas, aes( x= parametros, y = mean) ) + 
  geom_pointrange( aes(ymin = X2.5., ymax = X97.5.)) + 
  geom_pointrange( aes(ymin = X25., ymax = X75.), size = .8) +
  coord_flip() + 
  geom_point(aes(x = parametros, y = real), size = 2, colour = 'red') 

traceplot(jags_m_monedas)





#log.verosim.1 <- function(y,n.volados){
#  function(theta){
#    salida.1 <- sapply(1:length(theta), function(x){
#      dbeta(theta[x], y[x]+1, n.volados-y[x]+1, log=TRUE)
#    })
#    sum(salida.1)
#  }
#}
#mi.log.verosim <- log.verosim.1(y, n.volados)
#'MAx verosimilitud
#optim(c(0.5,0.5,0.5), fn=mi.log.verosim, control=list(fnscale=-1))
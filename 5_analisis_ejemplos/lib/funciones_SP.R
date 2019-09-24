# Gráfica Length
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
GG_subtab_sp <- function(tab){
  gg <- ggplot(tab, aes( y = X50., x = efectos.2)) + 
    geom_pointrange( aes(ymin = X2.5., ymax = X97.5.), alpha = .3 ) +  
    geom_pointrange( aes(ymin = X25., ymax = X75.), size = .4) +  
    coord_flip() +
    geom_hline(yintercept = 0, linetype = 2, alpha = .5) +
    ylab(NULL) + xlab(NULL)
  gg
}
library('ProjectTemplate')
load.project()

dat <- na.omit(BD.montgomery.6)
dat$treatment <- as.numeric(gsub("Tmt", "", dat$treatment))
dat$block <- as.numeric(gsub("Bck", "", dat$block))
sapply(dat, class)

tmt <- 'catalizador'
block <- 'maquina'
names(dat) <- c('y', tmt, block)
head(dat)

dat$catalizador <- factor(dat$catalizador, levels = 1:4)
dat$maquina <- factor(dat$maquina, levels = 1:4)#, labels = paste("máquina", 1:4))



# CATALIZADOR
# Tabla resumen por tratamiento (xtable)
tab.res <- ddply(dat, 'catalizador', summarise, 
                 promedio = mean(y, na.rm = T), 
                 desviación = sd(y, na.rm = T), 
                 mínimo = min(y, na.rm = T), 
                 máximo = max(y, na.rm = T))
xtable(tab.res, align = rep('c', ncol(tab.res) +1), digits = rep(0, ncol(tab.res) +1) , 
       label='tab_sum_dat', caption = 'Tabla Descriptiva del Tiempo de Reacción por Catalizador')

# Dot plots
ggplot(dat, aes(x = y, y = catalizador, colour = maquina)) + 
  geom_point(size = 3) + 
  facet_grid(catalizador~., scales='free_y')  + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        strip.text.y = element_text(angle = 0), 
        legend.position = "bottom") + 
  ylab('tiempo de reacción') + 
  xlab(NULL) +
  scale_color_brewer(palette="Paired")


ggplot(dat, aes(y = y, x = catalizador)) + 
  geom_boxplot() + 
  xlab(NULL) +
  ylab("tiempo de reacción") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0))


# MAQUINA
tab.res.maq <- ddply(dat, 'maquina', summarise, 
                 promedio = mean(y, na.rm = T), 
                 desviación = sd(y, na.rm = T), 
                 mínimo = min(y, na.rm = T), 
                 máximo = max(y, na.rm = T))
xtable(tab.res.maq, align = rep('c', ncol(tab.res) +1), digits = rep(0, ncol(tab.res) +1) , 
       label='tab_sum_maq', caption = 'Tabla Descriptiva del Tiempo de Reacción por Máquina')

ggplot(dat, aes(y = y, x = maquina)) + 
  geom_boxplot() +
  xlab(NULL) +
  ylab("tiempo de reacción") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0))

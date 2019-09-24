library(ProjectTemplate)
load.project()

datos <- BD.montgomery.pilas
head(datos)
datos$tipo.material <- factor(datos$tipo.material)
datos$temperatura <- factor(datos$temperatura)

# promedio
tab.tipo <- ddply(datos, "tipo.material", summarise, promedio = mean(vida), sd = sd(vida), var = var(vida) )
tab.temp <- ddply(datos, "temperatura", summarise, promedio = mean(vida), sd = sd(vida), var = var(vida) )
tab.int <- ddply(datos, c("temperatura", "tipo.material"), summarise, promedio = mean(vida), sd = sd(vida), var = var(vida) )
tab.int.l <- lapply( c("promedio", "sd", "var"), function(nom){
  dcast(formula = "tipo.material~temperatura", data = tab.int, value.var = nom)
})

print(xtableTipoTemp(xtab=tab.tipo, nom.tmt = "tipo de material", cap = "Evaluación por Tipo de Material", lab = "sum_tipo"), include.rownames = F)
print(xtableTipoTemp(xtab=tab.temp, nom.tmt = "temperatura", cap = "Evaluación por Temperatura (ºF)", lab = "sum_temp"), include.rownames = F)
print(xtableTipoTemp(xtab=tab.int.l[[1]], nom.cambio=F, nom.tmt = "promedio", cap = "Promedio por Tipo de Material dado Temperarura"), include.rownames = F)
print(xtableTipoTemp(xtab=tab.int.l[[2]], nom.cambio=F, nom.tmt ="desviación estándar", cap = "Desviación Estándar por Tipo de Material dado Temperarura"), include.rownames = F)
print(xtableTipoTemp(xtab=tab.int.l[[3]], nom.cambio=F, nom.tmt = "varianza", cap = "Varianza por Tipo de Material dado Temperarura"), include.rownames = F)



tab.tipo$tipo <- 1
tab.temp$tipo <- 1


# EVALUACIÓN FACTOR
# temperatura
gg.box.temp <- ggplot(datos, aes(x = temperatura, y = vida, 
        colour = temperatura, group = temperatura) ) + 
  geom_boxplot( aes(fill = temperatura) ) +  
  scale_colour_grey(start = .6, end =0) + 
  scale_fill_grey(start=.9, end =.4) + 
  xlab("temperatura (ºF)") + 
  ylab("tiempo de vida (horas)") + 
  theme(legend.position = "none") + 
  geom_point(data = tab.temp, aes(x = temperatura, y = promedio), size = 5, colour = "blue") + 
  geom_line(data = tab.temp, aes(x = temperatura, y = promedio, group = tipo), colour = "blue", linetype = 2) 
gg.box.temp 

# tipo material
gg.box.mat <- ggplot(datos, aes(x = tipo.material, y = vida, 
        colour = tipo.material, group = tipo.material) ) + 
  geom_boxplot( aes(fill = tipo.material) ) +  
  scale_colour_grey(start = .6, end =0) + 
  scale_fill_grey(start=.9, end =.4) + 
  xlab("tipo  de material") + 
  ylab("tiempo de vida (horas)") + 
  theme(legend.position = "none") + 
  geom_point(data = tab.tipo, aes(x = tipo.material, y = promedio), size = 5, colour = "blue") + 
  geom_line(data = tab.tipo, aes(x = tipo.material, y = promedio, group = tipo), colour = "blue", linetype = 2) 
gg.box.mat



# INTERACCIONES
tab <- ddply(datos, c("tipo.material", "temperatura"), summarise, 
             promedio = mean(vida))
tab$tipo.material <- factor(tab$tipo.material)
tab$temperatura <- factor(tab$temperatura)
gg.prom.int <- ggplot(tab, aes(x = tipo.material, y = promedio, 
          colour = temperatura, group = temperatura, linetype = temperatura) ) + 
  geom_point( size = 5 ) + 
  geom_line( width = 1) +
  scale_colour_grey(start = .7, end =0)+ 
  xlab("tipo  de material") + 
  ylab("tiempo de vida (horas)") + 
  theme(legend.position = "bottom") +
  labs(colour = "temperatura (ºF)", linetype = "temperatura (ºF)")
gg.prom.int

gg.prom.int2 <- ggplot(tab, aes(x = temperatura, y = promedio, 
          colour = tipo.material, group = tipo.material, linetype = tipo.material) ) + 
  geom_point( size = 5 ) + 
  geom_line( width = 1) +
  scale_colour_grey(start = .7, end =0)+ 
  xlab("temperatura (ºF)") + 
  ylab("tiempo de vida (horas)") + 
  theme(legend.position = "bottom") +
  labs(colour = "tipo  de material", linetype = "tipo  de material")
gg.prom.int2
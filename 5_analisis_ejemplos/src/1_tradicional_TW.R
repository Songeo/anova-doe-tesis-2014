library(ProjectTemplate)
load.project()

datos <- BD.montgomery.pilas
head(datos)
datos$tipo.material <- factor(datos$tipo.material)
datos$temperatura <- factor(datos$temperatura)

# Modelo
aov.tw <- aov(formula = vida ~ tipo.material*temperatura , data = datos)
summary(aov.tw)
xtable( summary(aov.tw), digits = c(0,0,2,1,3,4),align = "rccccc")





# Ajuste del Modelo
datos$residuales <- aov.tw$residuals
datos$prediccion <- aov.tw$fitted.values
datos$res.stand <- stdres(aov.tw)
datos$res.studen <- studres(aov.tw)
#plot(aov.tw)



ggplot(datos, aes(sample = res.studen)) + 
  geom_point(stat = "qq") +
  geom_abline(yintercept=-3, slope=1.1, alpha = .5)

ggplot(datos, aes(x = prediccion, y = res.studen, group = 1)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2, colour = "blue" ) 

ggplot(datos, aes(x = temperatura, y = res.studen, group = 1)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2, colour = "blue" ) 

ggplot(datos, aes(x = tipo.material, y = res.studen, group = 1)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = 2, colour = "blue") 


sub.ext <- datos[-c(which.min(datos$residuales),which.max(datos$residuales)), ]
ggplot(sub.ext, aes(x = prediccion, y = residuales)) + 
  geom_point() + 
  geom_hline(yintercept = 0, colour = "gray20", linetype = 2) 
ggplot(sub.ext, aes(x = temperatura, y = res.studen, group = 1)) + 
  geom_point() + 
  geom_hline(yintercept = 0, colour = "gray20", linetype = 2) +
  geom_smooth(method = "lm") 
ggplot(sub.ext, aes(x = tipo.material, y = residuales, group = 1)) + 
  geom_point() + 
  geom_hline(yintercept = 0, colour = "gray20", linetype = 2) +
  geom_smooth(method = "lm") 




# Pruebas diferencias
# tipo de material
lsd.mat <- LSD.test_mod(aov.tw, trt= "tipo.material", group = F)
dun.mat <- duncan.test_mod(aov.tw, trt= "tipo.material", group = F)

print(xtable(lsd.mat[[1]]), include.rownames=TRUE)
print(xtable(dun.mat[[1]]), include.rownames=TRUE)

LSDvsDUN(dif.lsd = lsd.mat[[1]], dif.dun = dun.mat[[1]])




# temperatura
lsd.temp <- LSD.test_mod(aov.tw, trt= "temperatura", group = F)
dun.temp <- duncan.test_mod(aov.tw, trt= "temperatura", group = F)

print(xtable(lsd.temp[[1]]), include.rownames=TRUE)
print(xtable(dun.temp[[1]]), include.rownames=TRUE)

LSDvsDUN(dif.lsd = lsd.temp[[1]], dif.dun = dun.temp[[1]])



# interacción de factores
datos$interaccion <- paste(datos$tipo.material, 
      datos$temperatura, sep = " vs ")
datos$interaccion <- reorder( datos$interaccion, as.numeric(as.character(datos$temperatura)), decreasing = F) 
df.ci <- ddply(datos, "interaccion", function(sub){
  norm.ci(xvec = sub$vida)  
})
ggplot(df.ci, aes( x = interaccion, y = promedio, ymin = LCI, ymax = UCI)) +
  geom_hline(yintercept =100, linetype = 2, alpha = .5) + 
  geom_pointrange() + 
  geom_point(size = 2) +
  coord_flip() + 
  ylab('promedio de tiempo de vida') + 
  xlab(NULL) 

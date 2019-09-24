library(ProjectTemplate)
load.project()

base <- BD.Engel.split.plot
base$main <- NULL
for(k in 2:11){
  base[, k] <- base[, k]-1
}
head(base)
df.m <- melt(base, id.vars=c("num", "percentage"),
             variable.name="factor",
             value.name="nivel")
head(df.m)
df.m$nivel.nom <- factor(df.m$nivel, labels = c("bajo", "alto")) 

# MAIN PLOT
df.m <- ddply(df.m, c("factor", "nivel.nom"), transform,
                 prom = mean(percentage))
df.m$tipo <- "diseño"
df.m$tipo[df.m$factor %in% c('m','n','o')] <- "ruido"

gg.main <- ggplot( df.m, aes( x = nivel.nom, y = prom, 
                              group = factor,color = factor,
                              linetype=factor, shape = factor) ) +
  geom_path()  +
  geom_point(size = 1.5, color = "black") + 
  facet_wrap(~tipo, ncol = 2) + 
  xlab("nivel de tratamiento") +
  ylab("promedio de\ncontracción") +
  scale_color_grey() + 
  scale_shape_manual(values=c(0:15)) + 
  labs(color = "factores", linetype = "factores", shape = "factores") +
  guides(color= guide_legend(nrow = 7))  +
  scale_y_continuous(label=function(x)round(x,1))


# NIVELES ENTRE FACTORES (BOXPLOT)
gg.main.box <- ggplot(df.m, aes(x = nivel.nom, y = percentage)) + 
  geom_boxplot() + 
  geom_point( alpha = .7, colour = "black",size = 1, 
              position = position_jitter(width = .1, height=.1) ) +  
  facet_wrap(~factor, ncol = 7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) +
  xlab("nivel de tratamiento") +
  ylab("porcentaje de\ncontracción")





# INTERACCIONES - Sub plot con whole plot
combs <- expand.grid(LETTERS[1:7], c('m', 'n', 'o'))
combs.df  <- ldply(1:nrow(combs), function(i){
  reng <- apply(combs[i,], 2, as.character)
  tt <- base[, c("num", reng["Var1"], reng["Var2"], "percentage")]
  names(tt) <- c("num", "int1", "int2", "percentage")
  tt.sum <- ddply(tt, c("int1","int2"), summarise,
        prom = mean(percentage))
  tt.sum$main <- as.character(reng[1])
  tt.sum$sub <- as.character(reng[2])
  tt.sum
})
combs.df$int1 <- factor(combs.df$int1, labels = c("bajo", "alto")) 
combs.df$int2 <- factor(combs.df$int2, labels = c("bajo", "alto")) 

gg.combs <- ggplot(combs.df, aes( x= int1, y = prom, 
    colour = int2, group = int2)) +
  geom_point(size = 2) + 
  geom_line() +
  facet_grid(sub~main) +
  xlab("niveles de factor de diseño") +
  ylab("promedio\nde contracción")+
  labs(colour = "niveles de ruido") + 
  scale_color_manual(values=c("gray55", "gray15")) + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) 

# dcast(combs.df, main + int1 ~ sub + int2, value.var="prom")
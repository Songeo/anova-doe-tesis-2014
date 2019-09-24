base <- BD.Engel.split.plot
base$main <- NULL
for(k in 2:11){
  base[, k] <- base[, k] -1
}
head(base)

# combinaciones de factores main y sub
selec.main.pos <- c(1:7)
selec.sub.pos <- c(1:3)
combs <- lapply(2:(length(selec.main.pos)+length(selec.sub.pos)), function(i){ 
  combn( c(LETTERS[selec.main.pos], c('m','n', 'o')[selec.sub.pos]), i )
})
comb.elem <- combs[[1]]

# interacciones segundo orden
orden.ints <- paste(comb.elem[1,], comb.elem[2, ], sep = ":")
formu <- paste('percentage ~ ', paste(orden.ints, collapse = " + ", sep = ""), "-1")
comb.mat <- model.matrix(object=as.formula(formu), data = base)

# interacciones segundo orden Main Plot
selec.main <- orden.ints[ apply(comb.elem, 2, function(col){ 
  sum(col %in% LETTERS[selec.main.pos]) }) >= 2 ]
mat.main.comb <- as.matrix( comb.mat[, selec.main] )
colnames(mat.main.comb) <- selec.main

# interacciones segundo orden Sub Plot
selec.sub <- orden.ints[c(apply(comb.elem, 2, function(col){ 
  sum(col %in% c('m', 'n', 'o')[selec.sub.pos]) }) == 1)]
mat.sub.comb <- as.matrix( comb.mat[, selec.sub] )
colnames(mat.sub.comb) <- selec.sub

# interacciones de error MP y SP
selec.error <- c("A:B:C:D:E:F:G","A:B:C:D:E:F:G:m:n:o")
error.l <- lapply(selec.error, function(sel){
  formu <- paste('percentage ~ ', paste(sel, collapse = " + ", sep = ""), "-1")  
  comb.mat <- model.matrix(object=as.formula(formu), data = base)
})
names(error.l) <- selec.error
mat.error.main <- error.l[["A:B:C:D:E:F:G"]][, 1]
mat.error.sub <- error.l[["A:B:C:D:E:F:G:m:n:o"]][, 1]

library(ggplot2)
theme_set(theme_bw())

cbb.s <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
           #"#F0E442", 
           "#0072B2", "#D55E00", "#CC79A7",
           "#8DD3C7",#"#FFFFB3",
           "#BEBADA","#FB8072",
           "#80B1D3","#FDB462","#B3DE69","#FCCDE5",
           "#D9D9D9","#BC80BD","#CCEBC5","#A6CEE3",
           "#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
           "#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99",
           "#1B9E77","#D95F02","#7570B3","#E7298A",
           "#66A61E","#E6AB02","#A6761D","#666666","#7FC97F",
           "#BEAED4","#FDC086","#386CB0",
           "#F0027F","#BF5B17","#66C2A5","#FC8D62","#8DA0CB",
           "#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3")

colcbb <- scale_color_manual(values=cbb.s)


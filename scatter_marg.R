library(ggplot2)
library(gridExtra)

scatter_marg <- function(xvar,yvar,zvar,xname,yname,zname,xbinwidth=0.1,ybinwidth=0.1){
  df <- data.frame(xvar, yvar, zvar)
  
  empty <- ggplot()+geom_point(aes(1,1), colour="white") +
    theme(                              
      plot.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  
  #scatterplot of x and y variables
  scatter <- ggplot(data=df,aes(x=xvar, y=yvar, color=zvar)) + 
    geom_point() +
    geom_smooth(method="lm") +
    theme(legend.position=c(1,1),legend.justification=c(1,1)) +
    xlab("") + ylab("") +
    scale_colour_gradient2(high="yellow", mid="black", zname)
  
  #marginal density of x - plot on top
  plot_top <- ggplot(df,aes(xvar)) + 
    geom_histogram(binwidth=xbinwidth) + 
    theme(legend.position = "none") +
    xlab(xname)
  
  #marginal density of y - plot on the right
  plot_right <- ggplot(df, aes(yvar)) + 
    geom_histogram(binwidth=ybinwidth) + 
    coord_flip()  + 
    theme(legend.position = "none") +
    xlab(yname)
  
  #arrange the plots together, with appropriate height and width for each row and column
  grid.arrange(plot_top, empty, scatter, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
}
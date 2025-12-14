library(ggrepel)
library(ggplot2)

CBF_PCA <- function(data, groups, useLabels = FALSE, labels = "", pcs = c(1,2), type = 'scores', scale = TRUE, legendName = "Cog task", absolute_loadings = FALSE) {
  
  data[is.na(data)] <- 0
  # Vector to calculate colors based on the number of groups
  colores <- rainbow(length(unique(groups)))
  
  # INPUTS:
  # data - data.frame or matrix - data to analyse with variables in columns and samples in rows
  # groups - factor - a grouping factor that determines the colours of the points in scores plots
  # useLabels (optional) - boolean - default=FALSE - if TRUE will draw labels next to each point
  # labels (optional) - default="" - labels to be drawn next to each point. If useLabels=TRUE and labels is empty will use rownames(data) as labels.
  # pcs (optional) - a vector of 2 integers - default=c(1,2) - principal components to be plotted
  # type (optional) - string - default='scores' - which plot to produce at the end. Possible: 'scores', 'loadings', 'varAcc'.
  # scale (optional) - boolean - default=TRUE - determines if variables are to be scaled (recommended)
  # legendName (optional) - string - default='Groups' - sets the name for the legend of colours (set by groups)
  # absolute_loadings (optional) - boolean - default=FALSE - if TRUE, loadings will be absolute
  
  # OUTPUTS:
  # a ggplot object. If not assigned to a variable will show the plot.
  
  if(scale) {
    pc <- prcomp(data, scale = TRUE)
  } else {
    pc <- prcomp(data)
  }
  
  if(absolute_loadings) {
    pc$rotation <- abs(pc$rotation)
  }
  
  if(type == 'scores') {
    if(useLabels & length(labels) != nrow(data)) {
      print("Warning: The labels not given or given incorrectly. Using rownames.")
      labels <- rownames(data)
    }
    
    pcdf <- data.frame(pc1 = pc$x[, pcs[1]], pc2 = pc$x[, pcs[2]])
    
    if(useLabels) pcdf$labels <- labels
    
    perc_accounted <- summary(pc)$importance[2, pcs] * 100
    
    .e <- environment()
    p <- ggplot(data = pcdf, aes(x = pc1, y = pc2), environment = .e) + 
      geom_point(aes(fill = groups), colour = "black", size = 5.5, pch = 21) +
      scale_fill_manual(values = colores, name = legendName)
    
    if(useLabels)  p <- p + geom_text_repel(aes(label = labels))
    
    p <- p + 
      xlab(paste("PC", pcs[1], " (", round(perc_accounted[1], 2), "%)", sep = "")) +
      ylab(paste("PC", pcs[2], " (", round(perc_accounted[2], 2), "%)", sep = "")) +
      theme_bw(base_size = 20) +
      theme(legend.position = "bottom")
    
    p
    
  } else if(type == 'loadings') {
    
    if(useLabels & length(labels) != nrow(pc$rotation)) {
      print("Warning: loadings labels not given or given incorrectly. Using the column names.")
      labels <- colnames(data)
    }
    
    pcdf <- data.frame(load1 = pc$rotation[, pcs[1]], load2 = pc$rotation[, pcs[2]], var = labels)
    
    label_offset_x <- 0.035 * (range(pcdf$load1)[2] - range(pcdf$load1)[1])
    label_offset_y <- 0.035 * (range(pcdf$load2)[2] - range(pcdf$load2)[1])
    
    .e <- environment()
    
    p <- ggplot(data = pcdf, aes(x = load1, y = load2), environment = .e) + geom_point()
    
    if(useLabels) p <- p + geom_text_repel(aes(x = load1, y = load2), label = labels)
    
    p <- p +
      xlab(paste("Loadings for PC", pcs[1], sep = "")) +
      ylab(paste("Loadings for PC", pcs[2], sep = "")) +
      ggtitle("PCA loadings plot") +
      theme_bw(base_size = 20)
    p
    
  } else if(type == 'varAcc') {
    perc_accounted <- (pc$sdev / sum(pc$sdev) * 100)
    perc_with_cumsum <- data.frame(pc = as.factor(1:length(perc_accounted)),
                                   perc_acc = perc_accounted,
                                   perc_cumsum = cumsum(perc_accounted))
    p <- ggplot(data = perc_with_cumsum, aes(x = pc, y = perc_cumsum)) +
      geom_bar(stat = 'identity', col = 'black', fill = 'white') +
      geom_hline(yintercept = 95, col = 'red') +
      geom_hline(yintercept = 0, col = 'black') +
      xlab('PC') +
      ylab('% Variance') +
      ggtitle('% Variance accounted for by principle components') +
      theme_bw()
    print(p)
    
  } else {
    cat(sprintf("\nError: no type %s", type))
  }
  
  return(list(plot = p, pc = pc))
  
}

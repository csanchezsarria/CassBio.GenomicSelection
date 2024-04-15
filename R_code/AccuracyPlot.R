# Short name: Accuracy plot
# Description: 
# Output: 
#
# Authors: Camilo E. Sánchez-Sarria (c.e.sanchez@cgiar.org)
#
# Arguments:
# trait_df: 
# type:



##### To do #####
# 1: Write good description, outputs, arguments and examples



# 0: Function init -------------------------------------------------------------
AccuracyPlot <- function(trait_df, type){
  
  
  dir <- 'D:/OneDrive - CGIAR/00_CassavaBioinformaticsPlatform/07_GenomicSelection/03_DaRTSeq/GS_Results_AllTraits_DiffProp_20/GS_Results_AllTraits_DiffProp_20.csv'
  trait_df <- read.csv(dir)
  
  
  # 1: Packages and data--------------------------------------------------------
  library(tidyverse)
  
  # Extract data
  traininig_value <- unique(trait_df$training_prop)
  CV_value <- max(trait_df$cv)
  
  
  
  # 2: -------------------------------------------------------------------------
  # 
  if (type == 'single'){
    
    # Plot the accuracy plot
    
    
  } else {
    
    type == 'all_traits'
    
    # Plot the accuracy plot
    plot <- ggplot(trait_df, aes(x = accuracy, 
                                 y = reorder(trait, accuracy, FUN = median),
                                 fill = trait, alpha = 0.5)) +
      geom_violin(fill = "gray80", color = "white", width = 1, alpha = 0.5) +
      geom_boxplot(width = 0.3) + 
      theme_classic() +
      theme(legend.position = "none",
            axis.title = element_text(size = 14, color = 'black'),
            axis.text = element_text(size = 14, color = 'black'),
            title = element_text(size = 14, color = 'black')) + 
      labs(x = 'Prediction ability', y = 'Traits', 
           title = paste0('Training proportion: ', traininig_value, ', CV: ', CV_value)) +
      xlim(0, 1) +
      scale_y_discrete(labels = labels) +
      geom_vline(xintercept = 0.50, linetype = 'dotted', color = 'gray40', linewidth = 0.80) + 
      geom_vline(xintercept = 0.30, linetype = 'dotted', color = 'gray40', linewidth = 0.80)
    
  }
  
  type == 'props'
  
  plot <- ggplot(trait_df, aes(x = accuracy, 
                               y = reorder(trait, accuracy, FUN = median),
                               fill = trait, alpha = 0.5)) +
    geom_violin(fill = "gray80", color = "white", width = 1, alpha = 0.5) +
    geom_boxplot(width = 0.3) + 
    theme_classic() +
    theme(legend.position = "none",
          axis.title = element_text(size = 14, color = 'black'),
          axis.text = element_text(size = 14, color = 'black'),
          title = element_text(size = 14, color = 'black')) + 
    labs(x = 'Prediction ability', y = 'Traits', 
         title = paste0('Training proportion: ', traininig_value, ', CV: ', CV_value)) +
    xlim(0, 1) +
    #scale_y_discrete(labels = labels) +
    geom_vline(xintercept = 0.50, linetype = 'dotted', color = 'gray40', linewidth = 0.80) + 
    geom_vline(xintercept = 0.30, linetype = 'dotted', color = 'gray40', linewidth = 0.80) +
    facet_wrap(~ training_prop, ncol = 1)
  
  
  # 3: -------------------------------------------------------------------------
  # 
  pdf(paste0('GS_Plot_AccuracyAbility.pdf'), width = 15, height = 10)
  return(plot)
  dev.off()

}



# Example(s) -------------------------------------------------------------------
# Set arguments
# trait_df <-
# type <- 



# Run function -----------------------------------------------------------------
# AccuracyPlot(trait_df, type)

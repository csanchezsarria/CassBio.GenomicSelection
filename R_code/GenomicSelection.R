# Short name: Genomic Selection
# Description: 
# Output: 
#
# Authors: Camilo E. Sánchez-Sarria (c.e.sanchez@cgiar.org) and Vianey Barrera-Enriquez (vpbarrerae@gmail.com).
#
# Arguments:
# single_trait:
# pheno:
# geno: 
# prop:
# k: 
# method:



##### To do #####
# 1: Write good description, outputs, arguments and examples



# 0: Function init -------------------------------------------------------------

GS <- function(single_trait, pheno, geno, prop, k, method){
  
  # Starting run time
  start_time <- Sys.time()
  
  # Convert geno file into numeric values
  geno <- sapply(geno[,-1], as.numeric)
  
  
  
  # 1: Imputation --------------------------------------------------------------
  # Impute and extract imputed values in a data frame
  geno_imputed <- 
    data.frame(
      A.mat(geno, max.missing = 0.5, impute.method = method, return.imputed = T)$imputed
    )
  
  # Round to -1, 0 and 1 the imputed values
  geno_imputed <- round(geno_imputed, digits = 0)
  
  # Select the trait
  trait <- pheno %>% select(all_of(single_trait)) %>% drop_na()
  
  # Estimate the proportion of samples to use and subset the data based on this
  prop_num <- round(length(rownames(trait)) * prop)
  geno_sub <- subset(geno_imputed, (rownames(geno_imputed) %in% rownames(trait)))
  
  # Divide data in training and testing sets
  training_samples <- as.matrix(sample(rownames(trait), prop_num))
  testing_samples <- as.matrix(setdiff(rownames(trait), training_samples))
  
  # Select training data
  pheno_training <- as.matrix(trait[training_samples,])
  geno_training <- as.matrix(geno_sub[training_samples,])
  
  # Select testing data
  pheno_testing <- as.matrix(trait[testing_samples,])
  geno_testing <- as.matrix(geno_sub[testing_samples,])
  
  # Training model
  trained_model <- mixed.solve(y = as.matrix(pheno_training), Z = geno_training)
  
  # Marker effect
  marker_effects <- as.matrix(trained_model$u)
  
  # Baseline, intercept
  BLUE <- as.vector(trained_model$beta)
  
  # Predictions
  predict_training <- as.matrix(geno_training) %*% marker_effects
  predict_testing <- as.matrix(geno_testing) %*% marker_effects
  
  # Predictions + BLUE
  predicted_training_result <- as.vector(predict_training[,1] + BLUE)
  predicted_testing_result <- as.vector(predict_testing[,1] + BLUE)
  
  # Accuracy & correlation
  accuracy_testing <- cor(as.vector(pheno_testing), predicted_testing_result, 
                          use = "complete")
  corr_model <- cor(as.vector(pheno_training), predicted_training_result, 
                    use = "complete")
  
  # Ends running time
  end_time <- Sys.time()
  
  # Calculate the time taken
  running_time <- end_time - start_time
  
  # 1: Function ends -----------------------------------------------------------
  
  return(list(trait = single_trait,
              training_prop = prop,
              testing_prop = 1 - prop,
              cv = k,
              prediction_ability = accuracy_testing, 
              time = running_time))
  
}



# Example(s) -------------------------------------------------------------------
# Set arguments
# single_trait <- 'yield_ha'
# pheno <-
# geno <- 
# prop <- 0.75
# k: Define better (cv <- 100)
# method <- 'mean'






# Run function -----------------------------------------------------------------
# GS(single_trait, pheno, geno, prop, k, method)
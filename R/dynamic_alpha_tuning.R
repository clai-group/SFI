# dynamic_alpha_tuning.R
#' Dynamically Select Optimal Alpha Using Cross-Validation
#'
#' @param df_train Training data with SFI scores and predictions
#' @param sfi_reference Reference SFI mean
#' @param alpha_range Range of alpha values to test
#' @param n_folds Number of CV folds
#' @param metric Optimization metric ("brier", "ece", "auc")
#'
#' @export
dynamic_alpha_selection <- function(df_train, 
                                    sfi_reference, 
                                    alpha_range = seq(0.5, 3.0, by = 0.1),
                                    n_folds = 5,
                                    metric = "brier") {
  
  library(caret)
  
  # Create folds
  folds <- createFolds(df_train$label, k = n_folds)
  
  # Test each alpha
  alpha_performance <- sapply(alpha_range, function(alpha) {
    
    fold_scores <- sapply(folds, function(fold_idx) {
      
      # Split data
      train_fold <- df_train[-fold_idx, ]
      val_fold <- df_train[fold_idx, ]
      
      # Calibrate validation fold
      val_fold_calib <- calibrate_predictions(
        val_fold, 
        sfi_reference = sfi_reference, 
        alpha = alpha
      )
      
      # Calculate metric
      if (metric == "brier") {
        score <- mean((val_fold_calib$calibrated_prob - val_fold_calib$label)^2)
      } else if (metric == "ece") {
        score <- expected_calibration_error(val_fold_calib$label, 
                                            val_fold_calib$calibrated_prob)
      } else if (metric == "auc") {
        score <- -as.numeric(auc(val_fold_calib$label, val_fold_calib$calibrated_prob))
      }
      
      return(score)
    })
    
    return(mean(fold_scores))
  })
  
  # Find optimal alpha
  optimal_idx <- which.min(alpha_performance)
  optimal_alpha <- alpha_range[optimal_idx]
  
  return(list(
    optimal_alpha = optimal_alpha,
    alpha_range = alpha_range,
    performance = alpha_performance,
    metric = metric
  ))
}


#' Adaptive Alpha Selection Based on Dataset Characteristics
#'
#' Adjusts alpha based on SFI distribution properties
#'
#' @param df_test Test dataset with SFI scores
#' @param sfi_reference Reference SFI mean
#' @param base_alpha Base alpha value
#'
#' @export
adaptive_alpha_selection <- function(df_test, sfi_reference, base_alpha = 2.0) {
  
  # Calculate SFI statistics
  sfi_mean_test <- mean(df_test$SFI, na.rm = TRUE)
  sfi_sd_test <- sd(df_test$SFI, na.rm = TRUE)
  sfi_sd_ref <- 0.15  # Typical reference SD (could be passed as parameter)
  
  # Adjust alpha based on:
  # 1. How different test SFI is from reference
  # 2. Variability of SFI in test set
  
  sfi_diff <- abs(sfi_mean_test - sfi_reference) / sfi_reference
  variability_ratio <- sfi_sd_test / sfi_sd_ref
  
  # Increase alpha if test set has lower quality or higher variability
  adjustment_factor <- 1 + 0.5 * sfi_diff + 0.3 * (variability_ratio - 1)
  
  adapted_alpha <- base_alpha * adjustment_factor
  
  # Clip to reasonable range
  adapted_alpha <- pmax(pmin(adapted_alpha, 3.0), 0.5)
  
  return(list(
    adapted_alpha = adapted_alpha,
    base_alpha = base_alpha,
    adjustment_factor = adjustment_factor,
    sfi_diff = sfi_diff,
    variability_ratio = variability_ratio
  ))
}
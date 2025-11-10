#' Identify Optimal Alpha for SFI-Aware Calibration
#'
#' This function performs a comprehensive workflow to identify the optimal alpha parameter
#' for SFI-aware calibration on a given dataset. It splits data into training and testing sets,
#' trains a random forest model, computes SFI components, tests multiple alpha values,
#' and selects the optimal alpha based on the geometric mean of F1-score and Recall improvements.
#'
#' @param data A data.frame containing patient-level longitudinal EHR data with columns:
#'   \itemize{
#'     \item \code{patient_id}: Unique patient identifier
#'     \item \code{label}: Binary outcome (0/1)
#'     \item \code{age}: Patient age
#'     \item \code{race}: Patient race/ethnicity
#'     \item \code{diagnosis_code}: ICD-10 diagnosis codes
#'     \item \code{encounter_date}: Date of encounter
#'     \item \code{setting}: Care setting (inpatient/outpatient)
#'     \item \code{medication}: Medication prescriptions (semicolon-separated)
#'   }
#' @param train_ratio Proportion of data to use for training (default = 0.7)
#' @param high_fidelity_codes Character vector of high-fidelity diagnosis codes for the phenotype
#' @param specific_medications Character vector of phenotype-specific medications
#' @param alpha_range Numeric vector of alpha values to test (default = seq(0.5, 2.5, by = 0.25))
#' @param predictors Character vector of predictor variables for random forest (default = c("age", "race"))
#' @param ntree Number of trees in random forest (default = 500)
#' @param seed Random seed for reproducibility (default = 123)
#' @param verbose Logical; print progress messages (default = TRUE)
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{optimal_alpha}: The selected optimal alpha value
#'     \item \code{optimal_metrics}: Performance metrics at optimal alpha
#'     \item \code{alpha_results}: Data.frame with performance across all alpha values
#'     \item \code{model}: The trained random forest model
#'     \item \code{train_data}: Training dataset with SFI scores
#'     \item \code{test_data}: Test dataset with SFI scores and predictions
#'     \item \code{sfi_reference}: Reference SFI mean from training data
#'     \item \code{alpha_plot}: ggplot object showing alpha sensitivity
#'   }
#'
#' @details
#' The function implements the methodology described in the SFI-aware calibration paper:
#' \enumerate{
#'   \item Splits data into training and testing sets at patient level
#'   \item Aggregates patient-level data and trains random forest model
#'   \item Computes SFI components for all patients
#'   \item Tests multiple alpha values (0.5 to 2.5)
#'   \item Selects optimal alpha by maximizing geometric mean of F1 and Recall improvements
#'   \item Validates statistical significance (p < 0.05) of performance gains
#' }
#'
#' The optimal alpha represents the calibration strength parameter that balances
#' prediction adjustment based on patient-level diagnostic fidelity.
#'
#' @examples
#' \dontrun{
#' # Load synthetic data
#' data <- read.csv("synthetic_data.csv")
#' 
#' # Define phenotype-specific parameters
#' high_fidelity_codes <- c("F90.0", "F90.1", "F90.2", "F90.8", "F90.9")
#' specific_medications <- c("methylphenidate", "amphetamine", "atomoxetine")
#' 
#' # Identify optimal alpha
#' results <- identify_optimal_alpha(
#'   data = data,
#'   train_ratio = 0.7,
#'   high_fidelity_codes = high_fidelity_codes,
#'   specific_medications = specific_medications,
#'   alpha_range = seq(0.5, 2.5, by = 0.25),
#'   seed = 123
#' )
#' 
#' # View results
#' print(results$optimal_alpha)
#' print(results$optimal_metrics)
#' plot(results$alpha_plot)
#' }
#'
#' @export
#' @import dplyr
#' @import randomForest
#' @import ggplot2
#' @import pROC
identify_optimal_alpha <- function(data,
                                   train_ratio = 0.7,
                                   high_fidelity_codes,
                                   specific_medications,
                                   alpha_range = seq(0.5, 2.5, by = 0.25),
                                   predictors = c("age", "race"),
                                   ntree = 500,
                                   seed = 123,
                                   verbose = TRUE) {
  
  # Check required packages
  required_packages <- c("dplyr", "randomForest", "ggplot2", "pROC")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "),
               "\nPlease install using: install.packages(c('", 
               paste(missing_packages, collapse = "', '"), "'))", sep = ""))
  }
  
  # Load packages
  library(dplyr)
  library(randomForest)
  library(ggplot2)
  library(pROC)
  
  if (verbose) cat("Starting optimal alpha identification workflow...\n")
  
  # Validate inputs
  required_cols <- c("patient_id", "label", "age", "race", "diagnosis_code", 
                    "encounter_date", "setting", "medication")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Set seed
  set.seed(seed)
  
  # ============================================================================
  # STEP 1: Train-Test Split at Patient Level
  # ============================================================================
  if (verbose) cat("\n[1/7] Splitting data into train and test sets...\n")
  
  unique_patients <- unique(data$patient_id)
  n_patients <- length(unique_patients)
  n_train <- floor(n_patients * train_ratio)
  
  train_patients <- sample(unique_patients, n_train)
  
  train_long <- data %>% filter(patient_id %in% train_patients)
  test_long <- data %>% filter(!patient_id %in% train_patients)
  
  if (verbose) {
    cat(sprintf("  - Total patients: %d\n", n_patients))
    cat(sprintf("  - Training patients: %d (%.1f%%)\n", n_train, train_ratio * 100))
    cat(sprintf("  - Testing patients: %d (%.1f%%)\n", n_patients - n_train, (1 - train_ratio) * 100))
  }
  
  # ============================================================================
  # STEP 2: Aggregate to Patient Level
  # ============================================================================
  if (verbose) cat("\n[2/7] Aggregating data to patient level...\n")
  
  train_patient <- train_long %>%
    group_by(patient_id) %>%
    summarise(
      label = first(label),
      age = last(age),
      race = first(race),
      .groups = 'drop'
    )
  
  test_patient <- test_long %>%
    group_by(patient_id) %>%
    summarise(
      label = first(label),
      age = last(age),
      race = first(race),
      .groups = 'drop'
    )
  
  # ============================================================================
  # STEP 3: Train Random Forest Model
  # ============================================================================
  if (verbose) cat("\n[3/7] Training random forest model...\n")
  
  # Prepare formula
  formula_str <- paste("as.factor(label) ~", paste(predictors, collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # Train model
  rf_model <- randomForest(
    formula = model_formula,
    data = train_patient,
    ntree = ntree,
    importance = TRUE
  )
  
  if (verbose) {
    cat(sprintf("  - Model: Random Forest with %d trees\n", ntree))
    cat(sprintf("  - Predictors: %s\n", paste(predictors, collapse = ", ")))
    cat(sprintf("  - Training accuracy: %.3f\n", 
                mean(predict(rf_model) == train_patient$label)))
  }
  
  # ============================================================================
  # STEP 4: Compute SFI Components
  # ============================================================================
  if (verbose) cat("\n[4/7] Computing SFI components...\n")
  
  # Compute SFI for training data
  train_sfi <- simulate_sfi_components(
    df_long = train_long,
    high_fidelity_codes = high_fidelity_codes,
    specific_medications = specific_medications
  )
  train_sfi <- compute_composite_sfi(train_sfi)
  
  # Compute SFI for test data
  test_sfi <- simulate_sfi_components(
    df_long = test_long,
    high_fidelity_codes = high_fidelity_codes,
    specific_medications = specific_medications
  )
  test_sfi <- compute_composite_sfi(test_sfi)
  
  # Calculate reference SFI
  sfi_reference <- mean(train_sfi$SFI, na.rm = TRUE)
  
  if (verbose) {
    cat(sprintf("  - Reference SFI (training): %.3f (SD = %.3f)\n", 
                sfi_reference, sd(train_sfi$SFI, na.rm = TRUE)))
    cat(sprintf("  - Test SFI mean: %.3f (SD = %.3f)\n", 
                mean(test_sfi$SFI, na.rm = TRUE), sd(test_sfi$SFI, na.rm = TRUE)))
  }
  
  # ============================================================================
  # STEP 5: Generate Predictions
  # ============================================================================
  if (verbose) cat("\n[5/7] Generating predictions on test set...\n")
  
  # Merge patient-level data with SFI
  test_patient <- test_patient %>%
    left_join(test_sfi %>% select(patient_id, SFI), by = "patient_id")
  
  # Generate predictions
  test_patient$predicted_prob <- predict(rf_model, newdata = test_patient, type = "prob")[, 2]
  
  if (verbose) {
    cat(sprintf("  - Test set size: %d patients\n", nrow(test_patient)))
    cat(sprintf("  - Mean predicted probability: %.3f\n", mean(test_patient$predicted_prob)))
  }
  
  # ============================================================================
  # STEP 6: Test Multiple Alpha Values
  # ============================================================================
  if (verbose) cat("\n[6/7] Testing alpha values:", paste(alpha_range, collapse = ", "), "...\n")
  
  # Initialize results
  alpha_results <- data.frame()
  
  # Compute uncalibrated baseline metrics
  baseline_metrics <- compute_performance_metrics(
    labels = test_patient$label,
    predictions = test_patient$predicted_prob
  )
  
  if (verbose) {
    cat("\n  Uncalibrated baseline:\n")
    cat(sprintf("    - AUC: %.3f\n", baseline_metrics$auc))
    cat(sprintf("    - F1-Score: %.3f\n", baseline_metrics$f1))
    cat(sprintf("    - Recall: %.3f\n", baseline_metrics$recall))
    cat(sprintf("    - Precision: %.3f\n", baseline_metrics$precision))
  }
  
  # Test each alpha
  for (alpha in alpha_range) {
    # Calibrate predictions
    test_calibrated <- calibrate_predictions(
      df = test_patient,
      sfi_reference = sfi_reference,
      alpha = alpha,
      prob_col = "predicted_prob"
    )
    
    # Compute metrics
    metrics <- compute_performance_metrics(
      labels = test_calibrated$label,
      predictions = test_calibrated$calibrated_prob
    )
    
    # Compute improvements
    f1_improvement <- metrics$f1 - baseline_metrics$f1
    recall_improvement <- metrics$recall - baseline_metrics$recall
    
    # Compute geometric mean (objective function)
    if (f1_improvement > 0 && recall_improvement > 0) {
      geometric_mean <- sqrt(f1_improvement * recall_improvement)
    } else {
      geometric_mean <- 0
    }
    
    # Store results
    alpha_results <- rbind(alpha_results, data.frame(
      alpha = alpha,
      auc = metrics$auc,
      f1 = metrics$f1,
      recall = metrics$recall,
      precision = metrics$precision,
      brier = metrics$brier,
      ece = metrics$ece,
      f1_improvement = f1_improvement,
      recall_improvement = recall_improvement,
      geometric_mean = geometric_mean
    ))
  }
  
  # ============================================================================
  # STEP 7: Select Optimal Alpha
  # ============================================================================
  if (verbose) cat("\n[7/7] Selecting optimal alpha...\n")
  
  # Find alpha with maximum geometric mean
  optimal_idx <- which.max(alpha_results$geometric_mean)
  optimal_alpha <- alpha_results$alpha[optimal_idx]
  optimal_metrics <- alpha_results[optimal_idx, ]
  
  # Test statistical significance
  test_calibrated_optimal <- calibrate_predictions(
    df = test_patient,
    sfi_reference = sfi_reference,
    alpha = optimal_alpha,
    prob_col = "predicted_prob"
  )
  
  # Perform paired t-test (simplified version - using bootstrap for robustness)
  n_bootstrap <- 100
  bootstrap_f1_diff <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    boot_idx <- sample(1:nrow(test_patient), replace = TRUE)
    boot_baseline <- compute_performance_metrics(
      test_patient$label[boot_idx],
      test_patient$predicted_prob[boot_idx]
    )$f1
    boot_calibrated <- compute_performance_metrics(
      test_patient$label[boot_idx],
      test_calibrated_optimal$calibrated_prob[boot_idx]
    )$f1
    bootstrap_f1_diff[i] <- boot_calibrated - boot_baseline
  }
  
  p_value <- mean(bootstrap_f1_diff <= 0)
  
  if (verbose) {
    cat(sprintf("\n  Optimal alpha: %.2f\n", optimal_alpha))
    cat(sprintf("  Geometric mean: %.4f\n", optimal_metrics$geometric_mean))
    cat(sprintf("  F1 improvement: %.3f (%.1f%%)\n", 
                optimal_metrics$f1_improvement,
                (optimal_metrics$f1_improvement / baseline_metrics$f1) * 100))
    cat(sprintf("  Recall improvement: %.3f (%.1f%%)\n", 
                optimal_metrics$recall_improvement,
                (optimal_metrics$recall_improvement / baseline_metrics$recall) * 100))
    cat(sprintf("  Statistical significance: p = %.4f\n", p_value))
    
    if (p_value < 0.05) {
      cat("  ✓ Significant improvement over baseline (p < 0.05)\n")
    } else {
      cat("  ✗ Not significant (p >= 0.05)\n")
    }
  }
  
  # ============================================================================
  # Create Alpha Sensitivity Plot
  # ============================================================================
  alpha_plot <- ggplot(alpha_results, aes(x = alpha, y = geometric_mean)) +
    geom_line(color = "darkgreen", linewidth = 1.2) +
    geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
    geom_vline(xintercept = optimal_alpha, linetype = "dashed", 
               color = "red", linewidth = 1) +
    annotate("text", x = optimal_alpha, y = max(alpha_results$geometric_mean) * 0.9,
             label = sprintf("Optimal α = %.2f", optimal_alpha),
             hjust = -0.1, color = "red", fontface = "bold") +
    labs(
      title = "Alpha Sensitivity Analysis",
      subtitle = sprintf("Optimal α = %.2f maximizes geometric mean of F1 and Recall improvements", 
                        optimal_alpha),
      x = "Alpha (Calibration Strength)",
      y = "Geometric Mean (F1 × Recall Improvement)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(face = "bold")
    )
  
  # ============================================================================
  # Return Results
  # ============================================================================
  if (verbose) cat("\n✓ Optimal alpha identification complete!\n")
  
  results <- list(
    optimal_alpha = optimal_alpha,
    optimal_metrics = optimal_metrics,
    baseline_metrics = baseline_metrics,
    alpha_results = alpha_results,
    model = rf_model,
    train_data = train_sfi,
    test_data = test_patient,
    sfi_reference = sfi_reference,
    alpha_plot = alpha_plot,
    p_value = p_value,
    statistical_significance = p_value < 0.05
  )
  
  class(results) <- c("sfi_optimal_alpha", "list")
  
  return(results)
}


#' Print Method for sfi_optimal_alpha Objects
#'
#' @param x An object of class sfi_optimal_alpha
#' @param ... Additional arguments (not used)
#' @export
print.sfi_optimal_alpha <- function(x, ...) {
  cat("\n=== SFI-Aware Calibration: Optimal Alpha Results ===\n\n")
  
  cat("Optimal Alpha:", sprintf("%.2f", x$optimal_alpha), "\n")
  cat("Statistical Significance:", ifelse(x$statistical_significance, 
                                         "YES (p < 0.05)", 
                                         sprintf("NO (p = %.4f)", x$p_value)), "\n\n")
  
  cat("Performance at Optimal Alpha:\n")
  cat(sprintf("  AUC:       %.3f\n", x$optimal_metrics$auc))
  cat(sprintf("  F1-Score:  %.3f (+%.3f from baseline)\n", 
              x$optimal_metrics$f1, x$optimal_metrics$f1_improvement))
  cat(sprintf("  Recall:    %.3f (+%.3f from baseline)\n", 
              x$optimal_metrics$recall, x$optimal_metrics$recall_improvement))
  cat(sprintf("  Precision: %.3f\n", x$optimal_metrics$precision))
  cat(sprintf("  Brier:     %.3f\n", x$optimal_metrics$brier))
  cat(sprintf("  ECE:       %.3f\n", x$optimal_metrics$ece))
  
  cat("\nBaseline (Uncalibrated) Performance:\n")
  cat(sprintf("  AUC:       %.3f\n", x$baseline_metrics$auc))
  cat(sprintf("  F1-Score:  %.3f\n", x$baseline_metrics$f1))
  cat(sprintf("  Recall:    %.3f\n", x$baseline_metrics$recall))
  cat(sprintf("  Precision: %.3f\n", x$baseline_metrics$precision))
  
  cat("\nReference SFI:", sprintf("%.3f", x$sfi_reference), "\n")
  cat("Test Set Size:", nrow(x$test_data), "patients\n")
  
  cat("\nUse plot() to visualize alpha sensitivity curve\n")
  
  invisible(x)
}


#' Plot Method for sfi_optimal_alpha Objects
#'
#' @param x An object of class sfi_optimal_alpha
#' @param ... Additional arguments passed to print.ggplot
#' @export
plot.sfi_optimal_alpha <- function(x, ...) {
  print(x$alpha_plot)
  invisible(x)
}

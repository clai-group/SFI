#' Compute Performance Metrics for Binary Classification
#'
#' Calculates discrimination and calibration metrics for binary classification predictions.
#'
#' @param labels Binary outcome vector (0/1)
#' @param predictions Predicted probabilities (0-1)
#' @param threshold Classification threshold (default = 0.5)
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{auc}: Area Under ROC Curve
#'     \item \code{f1}: F1-Score
#'     \item \code{recall}: Recall (Sensitivity)
#'     \item \code{precision}: Precision (PPV)
#'     \item \code{specificity}: Specificity
#'     \item \code{accuracy}: Overall accuracy
#'     \item \code{brier}: Brier score
#'     \item \code{ece}: Expected Calibration Error
#'   }
#'
#' @export
#' @import pROC
compute_performance_metrics <- function(labels, predictions, threshold = 0.5) {
  
  # Remove NA values
  valid_idx <- !is.na(labels) & !is.na(predictions)
  labels <- labels[valid_idx]
  predictions <- predictions[valid_idx]
  
  # Convert to binary predictions
  pred_class <- as.numeric(predictions >= threshold)
  
  # Calculate confusion matrix elements
  tp <- sum(pred_class == 1 & labels == 1)
  fp <- sum(pred_class == 1 & labels == 0)
  tn <- sum(pred_class == 0 & labels == 0)
  fn <- sum(pred_class == 0 & labels == 1)
  
  # Calculate metrics
  accuracy <- (tp + tn) / (tp + fp + tn + fn)
  
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0
  
  f1 <- if ((precision + recall) > 0) {
    2 * (precision * recall) / (precision + recall)
  } else {
    0
  }
  
  # Calculate AUC
  if (length(unique(labels)) > 1) {
    auc <- as.numeric(pROC::auc(labels, predictions, quiet = TRUE))
  } else {
    auc <- NA
  }
  
  # Calculate Brier score
  brier <- mean((predictions - labels)^2)
  
  # Calculate ECE
  ece <- expected_calibration_error(labels, predictions, n_bins = 10)
  
  return(list(
    auc = auc,
    f1 = f1,
    recall = recall,
    precision = precision,
    specificity = specificity,
    accuracy = accuracy,
    brier = brier,
    ece = ece
  ))
}

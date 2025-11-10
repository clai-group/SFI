# brier_decomposition.R
#' Compute Brier Score Decomposition
#'
#' Decomposes the Brier score into reliability (calibration), 
#' resolution (discrimination), and uncertainty components.
#'
#' @param labels Binary outcome vector (0/1)
#' @param predictions Predicted probabilities (0-1)
#' @param n_bins Number of bins for grouping predictions (default = 10)
#'
#' @return List containing:
#'   - brier_score: Overall Brier score
#'   - reliability: Calibration component (lower is better)
#'   - resolution: Discrimination component (higher is better)
#'   - uncertainty: Inherent uncertainty in outcomes
#'
#' @references
#' Murphy AH. A new vector partition of the probability score. 
#' Journal of Applied Meteorology. 1973;12(4):595-600.
#'
#' @export
brier_decomposition <- function(labels, predictions, n_bins = 10) {
  
  # Remove NA values
  valid_idx <- !is.na(labels) & !is.na(predictions)
  labels <- labels[valid_idx]
  predictions <- predictions[valid_idx]
  
  n <- length(labels)
  
  # Overall Brier score
  brier_score <- mean((predictions - labels)^2)
  
  # Create bins based on predicted probabilities
  bins <- cut(predictions, breaks = seq(0, 1, length.out = n_bins + 1), 
              include.lowest = TRUE)
  
  # Calculate components for each bin
  bin_stats <- data.frame(
    bin = bins,
    pred = predictions,
    obs = labels
  ) %>%
    group_by(bin) %>%
    summarise(
      n_k = n(),
      mean_pred = mean(pred),
      mean_obs = mean(obs),
      .groups = 'drop'
    ) %>%
    filter(n_k > 0)  # Remove empty bins
  
  # Overall observed event rate
  overall_rate <- mean(labels)
  
  # Reliability (calibration-in-the-large)
  # Measures how close predicted probabilities are to observed frequencies
  reliability <- sum(bin_stats$n_k * (bin_stats$mean_pred - bin_stats$mean_obs)^2) / n
  
  # Resolution (discrimination)
  # Measures how much predicted probabilities differ from base rate
  resolution <- sum(bin_stats$n_k * (bin_stats$mean_obs - overall_rate)^2) / n
  
  # Uncertainty (inherent randomness)
  uncertainty <- overall_rate * (1 - overall_rate)
  
  # Verify decomposition: Brier = Reliability - Resolution + Uncertainty
  decomp_sum <- reliability - resolution + uncertainty
  
  return(list(
    brier_score = brier_score,
    reliability = reliability,
    resolution = resolution,
    uncertainty = uncertainty,
    decomposition_check = decomp_sum,
    n_observations = n,
    n_bins = nrow(bin_stats),
    bin_stats = bin_stats
  ))
}


#' Compute Expected Calibration Error (ECE)
#'
#' @param labels Binary outcome vector
#' @param predictions Predicted probabilities
#' @param n_bins Number of bins (default = 10)
#'
#' @export
expected_calibration_error <- function(labels, predictions, n_bins = 10) {
  
  valid_idx <- !is.na(labels) & !is.na(predictions)
  labels <- labels[valid_idx]
  predictions <- predictions[valid_idx]
  
  n <- length(labels)
  
  bins <- cut(predictions, breaks = seq(0, 1, length.out = n_bins + 1), 
              include.lowest = TRUE)
  
  bin_stats <- data.frame(
    bin = bins,
    pred = predictions,
    obs = labels
  ) %>%
    group_by(bin) %>%
    summarise(
      n_k = n(),
      mean_pred = mean(pred),
      mean_obs = mean(obs),
      .groups = 'drop'
    ) %>%
    filter(n_k > 0)
  
  # ECE: weighted average of absolute calibration errors
  ece <- sum(bin_stats$n_k * abs(bin_stats$mean_pred - bin_stats$mean_obs)) / n
  
  return(ece)
}


#' Plot Calibration Curve with Confidence Bands
#'
#' @param labels Binary outcomes
#' @param predictions Predicted probabilities
#' @param n_bins Number of bins
#' @param title Plot title
#'
#' @export
plot_calibration_curve <- function(labels, predictions, n_bins = 10, 
                                   title = "Calibration Curve") {
  
  library(ggplot2)
  
  bins <- cut(predictions, breaks = seq(0, 1, length.out = n_bins + 1), 
              include.lowest = TRUE)
  
  bin_stats <- data.frame(
    bin = bins,
    pred = predictions,
    obs = labels
  ) %>%
    group_by(bin) %>%
    summarise(
      n_k = n(),
      mean_pred = mean(pred),
      mean_obs = mean(obs),
      se_obs = sqrt(mean_obs * (1 - mean_obs) / n()),
      .groups = 'drop'
    ) %>%
    filter(n_k > 0)
  
  ggplot(bin_stats, aes(x = mean_pred, y = mean_obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = mean_obs - 1.96 * se_obs, 
                    ymax = mean_obs + 1.96 * se_obs),
                alpha = 0.2, fill = "blue") +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(aes(size = n_k), color = "blue", alpha = 0.7) +
    scale_size_continuous(name = "Sample size") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(
      title = title,
      x = "Mean Predicted Probability",
      y = "Observed Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}
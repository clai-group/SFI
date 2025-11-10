#' Apply Platt Scaling (Logistic Calibration) - Robust Version
#'
#' Fits a logistic calibration model on training probabilities and returns
#' calibrated probabilities for the test set with full edge-case handling.
#'
#' @param train_probs Numeric vector of training predicted probabilities.
#' @param train_labels Numeric (0/1) vector of training labels.
#' @param test_probs Numeric vector of test predicted probabilities.
#'
#' @return A numeric vector of calibrated probabilities.
#'
#' @export

platt_scaling <- function(train_probs, train_labels, test_probs) {
  
  # Check for edge cases
  if (length(unique(train_probs)) < 3) {
    warning("Insufficient variability in training probabilities for Platt scaling")
    return(test_probs)  # Return uncalibrated
  }
  
  if (length(unique(train_labels)) < 2) {
    warning("Training labels have only one class")
    return(test_probs)
  }
  
  # Clip probabilities to avoid numerical issues
  train_probs_clipped <- pmax(pmin(train_probs, 1 - 1e-10), 1e-10)
  test_probs_clipped <- pmax(pmin(test_probs, 1 - 1e-10), 1e-10)
  
  # Logit transformation
  train_df <- data.frame(
    logit_prob = log(train_probs_clipped / (1 - train_probs_clipped)),
    label = train_labels
  )
  
  # Check for infinite values
  if (any(!is.finite(train_df$logit_prob))) {
    warning("Infinite values in logit transformation")
    return(test_probs)
  }
  
  # Fit calibration model with error handling
  calib_model <- tryCatch({
    glm(label ~ logit_prob, data = train_df, family = binomial(link = "logit"))
  }, error = function(e) {
    warning(paste("GLM fitting failed:", e$message))
    return(NULL)
  })
  
  if (is.null(calib_model) || !calib_model$converged) {
    warning("Platt scaling did not converge")
    return(test_probs)
  }
  
  # Apply to test set
  test_df <- data.frame(
    logit_prob = log(test_probs_clipped / (1 - test_probs_clipped))
  )
  
  # Check for infinite values in test set
  if (any(!is.finite(test_df$logit_prob))) {
    warning("Infinite values in test set logit transformation")
    return(test_probs)
  }
  
  calibrated_probs <- tryCatch({
    predict(calib_model, newdata = test_df, type = "response")
  }, error = function(e) {
    warning(paste("Prediction failed:", e$message))
    return(test_probs)
  })
  
  # Final clipping
  calibrated_probs <- pmax(pmin(as.numeric(calibrated_probs), 1), 0)
  
  return(calibrated_probs)
}


#' Apply Isotonic Regression Calibration - ROBUST
#'
#' @export
isotonic_calibration <- function(train_probs, train_labels, test_probs) {
  
  # Check for edge cases
  if (length(unique(train_probs)) < 3) {
    warning("Insufficient variability in training probabilities for isotonic regression")
    return(test_probs)
  }
  
  if (length(unique(train_labels)) < 2) {
    warning("Training labels have only one class")
    return(test_probs)
  }
  
  # Fit isotonic regression with error handling
  iso_model <- tryCatch({
    isoreg(train_probs, train_labels)
  }, error = function(e) {
    warning(paste("Isotonic regression fitting failed:", e$message))
    return(NULL)
  })
  
  if (is.null(iso_model)) {
    return(test_probs)
  }
  
  # Check if we have enough unique points for interpolation
  if (length(unique(iso_model$x)) < 2) {
    warning("Insufficient unique points for isotonic regression")
    return(test_probs)
  }
  
  # Create interpolation function with bounds
  calibrate_fn <- tryCatch({
    approxfun(iso_model$x, iso_model$yf, 
              method = "linear", 
              yleft = min(iso_model$yf, na.rm = TRUE),
              yright = max(iso_model$yf, na.rm = TRUE),
              rule = 2)  # Use nearest value for extrapolation
  }, error = function(e) {
    warning(paste("Interpolation function creation failed:", e$message))
    return(NULL)
  })
  
  if (is.null(calibrate_fn)) {
    return(test_probs)
  }
  
  # Apply to test set
  calibrated_probs <- tryCatch({
    calibrate_fn(test_probs)
  }, error = function(e) {
    warning(paste("Calibration application failed:", e$message))
    return(test_probs)
  })
  
  # Clip to [0, 1]
  calibrated_probs <- pmax(pmin(calibrated_probs, 1), 0)
  
  return(calibrated_probs)
}


#' Apply Beta Calibration - ROBUST
#'
#' @export
beta_calibration <- function(train_probs, train_labels, test_probs) {
  
  # Check for edge cases
  if (length(unique(train_probs)) < 5) {
    warning("Insufficient variability for beta calibration")
    return(test_probs)
  }
  
  if (length(unique(train_labels)) < 2) {
    warning("Training labels have only one class")
    return(test_probs)
  }
  
  # Clip probabilities
  train_probs_clipped <- pmax(pmin(train_probs, 1 - 1e-10), 1e-10)
  test_probs_clipped <- pmax(pmin(test_probs, 1 - 1e-10), 1e-10)
  
  # Transform to log-odds
  train_logodds <- log(train_probs_clipped / (1 - train_probs_clipped))
  
  if (any(!is.finite(train_logodds))) {
    warning("Infinite values in beta calibration")
    return(test_probs)
  }
  
  # Fit three-parameter model
  train_df <- data.frame(
    logodds = train_logodds,
    logodds_sq = train_logodds^2,
    label = train_labels
  )
  
  calib_model <- tryCatch({
    glm(label ~ logodds + logodds_sq, 
        data = train_df, 
        family = binomial(link = "logit"))
  }, error = function(e) {
    warning(paste("Beta calibration GLM failed:", e$message))
    return(NULL)
  })
  
  if (is.null(calib_model) || !calib_model$converged) {
    warning("Beta calibration did not converge")
    return(test_probs)
  }
  
  # Apply to test set
  test_logodds <- log(test_probs_clipped / (1 - test_probs_clipped))
  
  if (any(!is.finite(test_logodds))) {
    warning("Infinite values in test set for beta calibration")
    return(test_probs)
  }
  
  test_df <- data.frame(
    logodds = test_logodds,
    logodds_sq = test_logodds^2
  )
  
  calibrated_probs <- tryCatch({
    predict(calib_model, newdata = test_df, type = "response")
  }, error = function(e) {
    warning(paste("Beta calibration prediction failed:", e$message))
    return(test_probs)
  })
  
  calibrated_probs <- pmax(pmin(as.numeric(calibrated_probs), 1), 0)
  
  return(calibrated_probs)
}


#' Apply Temperature Scaling - ROBUST
#'
#' @export
temperature_scaling <- function(train_probs, train_labels, test_probs) {
  
  # Check for edge cases
  if (length(unique(train_probs)) < 3) {
    warning("Insufficient variability for temperature scaling")
    return(test_probs)
  }
  
  if (length(unique(train_labels)) < 2) {
    warning("Training labels have only one class")
    return(test_probs)
  }
  
  # Clip probabilities
  train_probs_clipped <- pmax(pmin(train_probs, 1 - 1e-10), 1e-10)
  test_probs_clipped <- pmax(pmin(test_probs, 1 - 1e-10), 1e-10)
  
  # Convert to logits
  train_logits <- log(train_probs_clipped / (1 - train_probs_clipped))
  
  if (any(!is.finite(train_logits))) {
    warning("Infinite logits in temperature scaling")
    return(test_probs)
  }
  
  # Define negative log likelihood function
  nll <- function(T) {
    if (T <= 0) return(Inf)
    scaled_probs <- 1 / (1 + exp(-train_logits / T))
    # Add small epsilon to avoid log(0)
    scaled_probs <- pmax(pmin(scaled_probs, 1 - 1e-10), 1e-10)
    -mean(train_labels * log(scaled_probs) + 
            (1 - train_labels) * log(1 - scaled_probs))
  }
  
  # Optimize temperature with error handling
  opt_result <- tryCatch({
    optimize(nll, interval = c(0.01, 10), maximum = FALSE)
  }, error = function(e) {
    warning(paste("Temperature optimization failed:", e$message))
    return(list(minimum = 1.0))  # Default to T=1 (no scaling)
  })
  
  T_optimal <- opt_result$minimum
  
  # Check if optimization was successful
  if (!is.finite(T_optimal) || T_optimal <= 0) {
    warning("Invalid temperature found, using T=1")
    T_optimal <- 1.0
  }
  
  # Apply to test set
  test_logits <- log(test_probs_clipped / (1 - test_probs_clipped))
  
  if (any(!is.finite(test_logits))) {
    warning("Infinite logits in test set for temperature scaling")
    return(test_probs)
  }
  
  calibrated_probs <- 1 / (1 + exp(-test_logits / T_optimal))
  calibrated_probs <- pmax(pmin(as.numeric(calibrated_probs), 1), 0)
  
  return(calibrated_probs)
}


#' Apply Histogram Binning Calibration - ROBUST
#'
#' @export
histogram_binning <- function(train_probs, train_labels, test_probs, n_bins = 10) {
  
  # Check for edge cases
  if (length(unique(train_probs)) < n_bins) {
    warning("Insufficient unique probabilities for histogram binning, reducing bins")
    n_bins <- max(3, length(unique(train_probs)))
  }
  
  if (length(unique(train_labels)) < 2) {
    warning("Training labels have only one class")
    return(test_probs)
  }
  
  if (length(train_probs) < n_bins * 2) {
    warning("Too few samples for histogram binning")
    return(test_probs)
  }
  
  # Create bins based on quantiles to ensure each bin has data
  bin_edges <- tryCatch({
    unique(quantile(train_probs, probs = seq(0, 1, length.out = n_bins + 1)))
  }, error = function(e) {
    warning(paste("Bin edge creation failed:", e$message))
    return(seq(0, 1, length.out = n_bins + 1))
  })
  
  # Ensure proper bounds
  bin_edges[1] <- 0
  bin_edges[length(bin_edges)] <- 1
  
  # Handle case where we have fewer unique edges than requested bins
  if (length(bin_edges) < 3) {
    warning("Insufficient distinct bin edges, using simple mean")
    return(rep(mean(train_labels), length(test_probs)))
  }
  
  # Assign training data to bins
  train_bins <- cut(train_probs, 
                    breaks = bin_edges, 
                    include.lowest = TRUE, 
                    labels = FALSE)
  
  # Calculate calibrated value for each bin
  n_actual_bins <- length(unique(train_bins[!is.na(train_bins)]))
  bin_calibration <- numeric(n_actual_bins)
  
  for (b in 1:n_actual_bins) {
    idx <- which(train_bins == b)
    if (length(idx) > 0) {
      bin_calibration[b] <- mean(train_labels[idx])
    } else {
      # Use overall mean for empty bins
      bin_calibration[b] <- mean(train_labels)
    }
  }
  
  # Handle any remaining NA bins
  bin_calibration[is.na(bin_calibration)] <- mean(train_labels)
  
  # Apply to test set
  test_bins <- cut(test_probs, 
                   breaks = bin_edges, 
                   include.lowest = TRUE, 
                   labels = FALSE)
  
  # Map test bins to calibrated values
  calibrated_probs <- bin_calibration[test_bins]
  
  # Handle any test values outside training range
  calibrated_probs[is.na(calibrated_probs)] <- mean(train_labels)
  
  # Final safety check
  calibrated_probs <- pmax(pmin(calibrated_probs, 1), 0)
  
  return(calibrated_probs)
}


#' Wrapper function to safely apply any calibration method
#'
#' @export
safe_calibrate <- function(calibration_fn, train_probs, train_labels, 
                           test_probs, method_name = "calibration") {
  
  result <- tryCatch({
    calibration_fn(train_probs, train_labels, test_probs)
  }, error = function(e) {
    warning(sprintf("%s completely failed: %s. Returning uncalibrated.", 
                    method_name, e$message))
    return(test_probs)
  }, warning = function(w) {
    # Warnings are already issued by individual functions
    return(suppressWarnings(calibration_fn(train_probs, train_labels, test_probs)))
  })
  
  # Final validation
  if (any(is.na(result)) || any(!is.finite(result))) {
    warning(sprintf("%s produced invalid values. Returning uncalibrated.", method_name))
    return(test_probs)
  }
  
  # Ensure all values are in [0, 1]
  result <- pmax(pmin(result, 1), 0)
  
  return(result)
}
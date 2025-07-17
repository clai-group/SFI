#' End-to-end pipeline to compute SFI and calibrate predictions
#'
#' @param df A data.frame with raw SFI components and model predictions
#' @param sfi_reference Reference SFI (from training data)
#' @param alpha Recalibration multiplier
#' @param prob_col Name of predicted probability column
#' @param weights Optional named vector of SFI component weights
#' @return A data.frame with 'SFI' and 'calibrated_prob' columns added

sfi_calibration_pipeline <- function(df, sfi_reference, alpha = 1.0, prob_col = "predicted_prob", weights = NULL) {
  df <- compute_composite_sfi(df, component_weights = weights)
  df <- calibrate_predictions(df, sfi_reference = sfi_reference, alpha = alpha, prob_col = prob_col)
  return(df)
}

#' Recalibrate predicted probabilities using SFI
#'
#' @param df A data.frame with columns: predicted_prob and SFI
#' @param sfi_reference Mean SFI from training dataset
#' @param alpha Sensitivity multiplier (default = 1.0)
#' @param prob_col Name of the column with original model probabilities
#' @return A data.frame with an added column 'calibrated_prob'
#' @export
calibrate_predictions <- function(df, sfi_reference, alpha = 1.0, prob_col = "predicted_prob") {
  stopifnot(all(c(prob_col, "SFI") %in% names(df)))
  
  df$calibrated_prob <- df[[prob_col]] * (1 + alpha * (df$SFI - sfi_reference) / sfi_reference)
  df$calibrated_prob <- pmin(pmax(df$calibrated_prob, 0), 1)  # clip to [0,1]
  
  return(df)
}
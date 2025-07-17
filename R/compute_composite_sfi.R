#' Compute Composite SFI Score
#'
#' This function calculates a composite Score of Functional Integrity (SFI)
#' by combining six component scores using equal or custom weights.
#'
#' @param df A data.frame containing six SFI components:
#'   \itemize{
#'     \item \code{specificity}
#'     \item \code{temporal_consistency}
#'     \item \code{entropy}
#'     \item \code{contextual_concordance}
#'     \item \code{medication_alignment}
#'     \item \code{trajectory_stability}
#'   }
#' @param component_weights Optional named numeric vector of weights for each component.
#'   Must include names for all six components. Default assigns equal weights.
#'
#' @return A data.frame with an added column \code{SFI} containing the composite score.
#' @export
#' 
compute_composite_sfi <- function(df, component_weights = NULL) {
  sfi_cols <- c("specificity", "temporal_consistency", "entropy",
                "contextual_concordance", "medication_alignment", "trajectory_stability")
  
  # Check that all components exist
  if (!all(sfi_cols %in% names(df))) {
    stop("Missing one or more required SFI component columns.")
  }
  
  # Assign weights
  if (is.null(component_weights)) {
    weights <- rep(1/length(sfi_cols), length(sfi_cols))
    names(weights) <- sfi_cols
  } else {
    weights <- component_weights[sfi_cols]
    weights <- weights / sum(weights)  # normalize
  }
  
  # Compute weighted sum
  df$SFI <- as.numeric(as.matrix(df[sfi_cols]) %*% weights)
  
  return(df)
}



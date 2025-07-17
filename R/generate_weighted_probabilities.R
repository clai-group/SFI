#' Generate Weighted Probabilities
#'
#' Creates a probability vector by assigning different weights to two groups
#' (e.g., high-fidelity and low-fidelity diagnosis codes) and normalizing to sum to 1.
#'
#' @param n_high Number of items in the high-weight group (default = 14).
#' @param n_low Number of items in the low-weight group (default = 12).
#' @param high_weight Weight multiplier for the high group (default = 1.2).
#' @param low_weight Weight multiplier for the low group (default = 0.8).
#'
#' @return A numeric vector of probabilities that sums to 1.
#'
#' @examples
#' set.seed(123)  # For reproducibility
#' prob_vector <- generate_weighted_probabilities()
#' print(prob_vector)
#' sum(prob_vector)  # Should equal 1
#'
#' @export

generate_weighted_probabilities <- function(n_high = 14, n_low = 12, high_weight = 1.2, low_weight = 0.8) {
  # Generate raw random values
  high_part <- runif(n_high) * high_weight
  low_part  <- runif(n_low)  * low_weight
  
  # Combine and normalize to sum to 1
  probs <- c(high_part, low_part)
  probs <- probs / sum(probs)
  
  return(probs)
}



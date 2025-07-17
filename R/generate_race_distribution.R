#' Generate Race Distribution
#'
#' This function generates a random race distribution (White, Black, Hispanic, Asian, Other)
#' based on uniform sampling within specified ranges and ensures that "Other" remains
#' between 0 and 0.03.
#'
#' @return A named numeric vector of race proportions that sum to 1.
#' @examples
#' generate_race_distribution()
#'
#' @export
#' 
generate_race_distribution <- function() {
  repeat {
    # Draw initial values from uniform distributions within specified ranges
    white    <- runif(1, 0.40, 0.90)
    black    <- runif(1, 0.02, 0.30)
    hispanic <- runif(1, 0.05, 0.50)
    asian    <- runif(1, 0.03, 0.30)
    
    total_primary <- white + black + hispanic + asian
    other <- 1 - total_primary
    
    # Check constraint: other must be between 0 and 0.03
    if (other >= 0 && other <= 0.03) {
      break
    }
  }
  
  # Return as named vector
  probs <- c(White = white, Black = black, Hispanic = hispanic, Asian = asian, Other = other)
  return(round(probs / sum(probs), 4))  # Ensure exact normalization
}

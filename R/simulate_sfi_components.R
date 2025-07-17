#' Simulate SFI Components for Patients
#'
#' Computes six components of the SFI (Specificity, Temporal Consistency, Entropy,
#' Contextual Concordance, Medication Alignment, Trajectory Stability) from a
#' longitudinal patient dataset.
#'
#' @param df_long A long-format data frame with at least these columns:
#'   \itemize{
#'     \item \code{patient_id}
#'     \item \code{diagnosis_code}
#'     \item \code{encounter_date}
#'     \item \code{setting} (e.g., "inpatient", "outpatient")
#'     \item \code{medication}
#'     \item \code{label} (optional outcome)
#'     \item \code{race} (optional)
#'     \item \code{age}
#'   }
#'
#' @return A data frame in wide format
#'
#' @examples
#' # Example with simulated data
#' df_long <- simulate_patient_data(n = 1000, mean_age = 60, dementia_rate = 0.3)
#'
#' df_sfi <- simulate_sfi_components(df_long)
#' head(df_sfi)
#'
#' @export
simulate_sfi_components <- function(df_long) {
  library(dplyr)
  
  df_sfi <- df_long %>%
    group_by(patient_id) %>%
    summarise(
      label = first(label),
      race = first(race),
      age = last(age),
      
      # Specificity: fraction of highly specific codes
      specificity = mean(diagnosis_code %in% c("G30.0" , "G30.1" , "G30.8" , "G30.9" , "F01.50" ,"F01.51" ,"F02.80" ,"F02.81" ,"F03.90" ,"F03.91" ,"G31.1" , "G31.83")),###these are the specific code
     
      
      
      
      # Temporal Consistency: less frequent diagnosis switching
      temporal_consistency = {
        sorted_codes <- diagnosis_code[order(encounter_date)]
        if (length(sorted_codes) > 1) {
          1 - sum(head(sorted_codes, -1) != tail(sorted_codes, -1)) / (length(sorted_codes) - 1)
        } else {
          1
        }
      },
      
      # Entropy: normalized diversity of diagnosis codes
      entropy = {
        freqs <- table(diagnosis_code)
        probs <- freqs / sum(freqs)
        raw_entropy <- -sum(probs * log2(probs))
        max_entropy <- log2(length(unique(diagnosis_code)))
        ifelse(max_entropy > 0, 1 - raw_entropy / max_entropy, 0)
      },
      
      # Contextual Concordance: match between diagnosis and setting
      contextual_concordance = mean(ifelse(
        setting == "inpatient" & diagnosis_code %in% c("G30.0" , "G30.1" , "G30.8" , "G30.9" , "F01.50" ,"F01.51" ,"F02.80" ,"F02.81" ,"F03.90" ,"F03.91" ,"G31.1" , "G31.83"), 1,
        ifelse(setting == "outpatient" & diagnosis_code %in% c("G30.0" , "G30.1" , "G30.8" , "G30.9" , "F01.50" ,"F01.51" ,"F02.80" ,"F02.81" ,"F03.90" ,"F03.91" ,"G31.1" , "G31.83"), 0.5, 0)
      )),
      
      # Medication Alignment: dementia-specific drug use when diagnosed
      medication_alignment = mean(diagnosis_code %in% c("G30.0" , "G30.1" , "G30.8" , "G30.9" , "F01.50" ,"F01.51" ,"F02.80" ,"F02.81" ,"F03.90" ,"F03.91" ,"G31.1" , "G31.83") &
                                    medication %in% c("donepezil", "memantine", "rivastigmine", "galantamine")),
      
      # Trajectory Stability: consistent primary diagnosis across settings
      trajectory_stability = {
        dominant_codes <- df_long %>%
          filter(patient_id == first(patient_id)) %>%
          group_by(setting) %>%
          summarise(mode_code = names(which.max(table(diagnosis_code))), .groups = 'drop')
        
        if (nrow(dominant_codes) > 1) {
          as.numeric(length(unique(dominant_codes$mode_code)) == 1)
        } else {
          1
        }
      }
    ) %>%
    ungroup()
  
  return(df_sfi)
}


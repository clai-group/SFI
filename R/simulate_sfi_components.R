#' Simulate SFI Components for Patients
#'
#' Computes six components of the SFI (Specificity, Temporal Consistency, Entropy,
#' Contextual Concordance, Medication Alignment, Trajectory Stability) from a
#' longitudinal patient dataset.
#'
#' FIXED: Now uses dynamic high_fidelity_codes and dementia_specific_medications
#' passed from the parameter file instead of hardcoded values.
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
#' @param high_fidelity_codes Character vector of high-fidelity diagnosis codes
#' @param specific_medications Character vector of phenotype-specific medications for alignment calculation
#'
#' @return A data frame in wide format with SFI components
#'
#' @examples
#' # Load parameters
#' source("dementia_parameters.R")
#' 
#' # Example with simulated data
#' df_long <- simulate_patient_data(n = 1000, mean_age = 60, 
#'                                  phenotype_prevalence = 0.3,
#'                                  low_fidelity_codes = low_fidelity_codes,
#'                                  high_fidelity_codes = high_fidelity_codes,
#'                                  medications = medications,
#'                                  medication_probs_positive = medication_probs_positive,
#'                                  medication_probs_negative = medication_probs_negative,
#'                                  age_effect_threshold = age_threshold,
#'                                  age_effect_lowerrisk = age_lower_risk,
#'                                  race_multiplier = race_multiplier,
#'                                  inpatient_prob_positive = inpatient_prob_positive,
#'                                  inpatient_prob_negative = inpatient_prob_negative)
#'
#' df_sfi <- simulate_sfi_components(df_long, 
#'                                   high_fidelity_codes = high_fidelity_codes,
#'                                   specific_medications = dementia_specific_medications)
#' head(df_sfi)
#'
#' @export
simulate_sfi_components <- function(df_long, 
                                    high_fidelity_codes, 
                                    specific_medications) {
  library(dplyr)
  
  df_sfi <- df_long %>%
    group_by(patient_id) %>%
    summarise(
      label = first(label),
      race = first(race),
      age = last(age),
      
      # Specificity: fraction of highly specific codes
      # FIXED: Now uses dynamic high_fidelity_codes instead of hardcoded list
      specificity = mean(diagnosis_code %in% high_fidelity_codes),
      
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
      # FIXED: Now uses dynamic high_fidelity_codes
      contextual_concordance = mean(ifelse(
        setting == "inpatient" & diagnosis_code %in% high_fidelity_codes, 1,
        ifelse(setting == "outpatient" & diagnosis_code %in% high_fidelity_codes, 0.5, 0)
      )),
      
      # Medication Alignment: phenotype-specific drug use when diagnosed
      # FIXED: Now uses dynamic specific_medications and checks against high_fidelity_codes
      # Need to handle semicolon-separated medication strings
      medication_alignment = {
        # Get all medications for this patient
        all_meds <- unique(unlist(strsplit(medication, ";")))
        all_meds <- all_meds[all_meds != "none"]
        
        # Check if patient has any specific medications
        has_specific_med <- any(all_meds %in% specific_medications)
        
        # Check if patient has high-fidelity diagnosis
        has_high_fidelity_dx <- any(diagnosis_code %in% high_fidelity_codes)
        
        # Both conditions must be true for high alignment
        as.numeric(has_specific_med & has_high_fidelity_dx)
      },
      
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

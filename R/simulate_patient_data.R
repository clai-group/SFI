#' Simulate Longitudinal Patient Data for Dementia Research
#'
#' Generates synthetic patient-level longitudinal data with realistic distributions
#' of demographics, diagnoses, and medications for dementia-related research.
#'
#' @param n Number of patients to simulate.
#' @param mean_age Average age of the simulated cohort.
#' @param race_probs Named numeric vector of race probabilities
#' @param dementia_rate Base dementia prevalence rate (0–1).
#' @param start_date Start date for encounters.
#' @param end_date End date for encounters.
#'
#' @return A data frame in long format
#'
#' @examples
#' df_sim <- simulate_patient_data(
#   n = 1000,
#   mean_age = 68,
#   race_probs = c(White = 0.4, Black = 0.3, Hispanic = 0.2, Asian = 0.05, Other = 0.05),
#   dementia_rate = 0.3
# )
#' head(df_sim)
#'
#' @export

simulate_patient_data <- function(n,
                                  mean_age,
                                  race_probs = c(generate_race_distribution()),
                                  dementia_rate,
                                  start_date = as.Date("2020-01-01"),
                                  end_date = as.Date("2025-01-01")) {
  
  # Define diagnosis codes (separated by fidelity)
  low_fidelity_codes <- c("R41.81", "R41.89", "R41.9", "R40.0", "R40.4", 
                          "F05", "F05.9", "R41.0", "R41.3", "G31.84", 
                          "F06.7", "F09", "G93.40", "F07.89")
  
  high_fidelity_codes <- c("G30.0", "G30.1", "G30.8", "G30.9", "F01.50", 
                           "F01.51", "F02.80", "F02.81", "F03.90", "F03.91", 
                           "G31.1", "G31.83")
  
  diagnosis_codes <- c(low_fidelity_codes, high_fidelity_codes)
  
  # Other helper vectors
  medications <- c("donepezil", "memantine", "rivastigmine", "galantamine", "none")
  settings <- c("inpatient", "outpatient")
  
  # Create patient-level metadata
  patient_id <- paste0("patient_", 1:n)
  
  # Generate age and race first
  race <- sample(names(race_probs), n, replace = TRUE, prob = race_probs)
  age <- pmin(pmax(round(rnorm(n, mean = mean_age, sd = sample(5:20, 1))), 18), 90)
  
  # Create dementia labels based on age and race correlations from literature
  generate_dementia_probability <- function(age, race, base_rate) {
    # Age effect: exponential increase after 65
    # Risk roughly doubles every 5 years after 65
    age_effect <- ifelse(age < 65, 
                         0.5,  # Lower risk below 65
                         2^((age - 65) / 5))  # Exponential increase after 65
    
    # Race effect based on literature:
    # Black and Hispanic populations have higher dementia risk
    # Asian populations have mixed findings
    # White populations are reference
    race_multiplier <- case_when(
      race == "Black" ~ 1.5,      # ~50% higher risk
      race == "Hispanic" ~ 1.3,   # ~30% higher risk  
      race == "Asian" ~ 1.1,      # ~10% higher risk
      race == "Other" ~ 1.2,      # ~20% higher risk
      TRUE ~ 1.0                  # White reference
    )
    
    # Combined probability
    combined_prob <- base_rate * age_effect * race_multiplier
    
    # Cap at reasonable maximum (e.g., 80% for very elderly)
    return(pmin(combined_prob, 0.8))
  }
  
  # Generate individual probabilities and labels
  individual_dementia_probs <- mapply(generate_dementia_probability, 
                                      age = age, 
                                      race = race, 
                                      MoreArgs = list(base_rate = dementia_rate))
  
  label <- rbinom(n, 1, individual_dementia_probs)
  
  patients <- data.frame(patient_id, label, race, age)
  
  # Function to generate patient-specific diagnosis probabilities
  generate_patient_diagnosis_probs <- function(has_dementia) {
    base_probs <- generate_weighted_probabilities()
    n_low_fidelity <- length(low_fidelity_codes)
    n_high_fidelity <- length(high_fidelity_codes)
    
    if (has_dementia) {
      # For dementia patients: boost high-fidelity codes
      # Apply 2x weight to high-fidelity codes
      fidelity_weights <- c(rep(1, n_low_fidelity), rep(2, n_high_fidelity))
      adjusted_probs <- base_probs * fidelity_weights
      
      # Normalize to sum to 1
      return(adjusted_probs / sum(adjusted_probs))
    } else {
      # For non-dementia patients: 
      # - Low-fidelity codes: moderate reduction (10-30% of base rates)
      # - High-fidelity codes: very low rates (1-5% of base rates)
      
      low_fidelity_reduction <- runif(1, 0.1, 0.3)   # 10-30% for low-fidelity
      high_fidelity_reduction <- runif(1, 0.01, 0.05) # 1-5% for high-fidelity
      
      # Apply different reductions
      reduction_factors <- c(rep(low_fidelity_reduction, n_low_fidelity), 
                             rep(high_fidelity_reduction, n_high_fidelity))
      
      return(base_probs * reduction_factors)
    }
  }
  
  # Simulate encounters
  all_data <- do.call(rbind, lapply(1:n, function(i) {
    pid <- patient_id[i]
    lab <- label[i]
    rac <- race[i]
    ag <- age[i]
    n_visits <- sample(2:20, 1)
    encounter_dates <- sort(sample(seq(start_date, end_date, by = "day"), n_visits))
    
    # Generate diagnosis probabilities once per patient
    diagnosis_probs <- generate_patient_diagnosis_probs(lab == 1)
    
    data.frame(
      patient_id = pid,
      label = lab,
      race = rac,
      age = ag,
      encounter_date = encounter_dates,
      diagnosis_code = sample(diagnosis_codes, n_visits, replace = TRUE, prob = diagnosis_probs),
      setting = if (lab == 1) {
        # Dementia patients: higher rate of inpatient visits
        sample(settings, n_visits, replace = TRUE, prob = c(0.4, 0.6))
      } else {
        # Non-dementia patients: lower rate of inpatient visits
        sample(settings, n_visits, replace = TRUE, prob = c(0.25, 0.75))
      },
      medication = if (lab == 1) {
        # Dementia patients: higher rates of dementia medications
        sample(medications, n_visits, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1))
      } else {
        # Non-dementia patients: very low rates of dementia medications
        sample(medications, n_visits, replace = TRUE, prob = c(0.01, 0.01, 0.005, 0.005, 0.97))
      },
      stringsAsFactors = FALSE
    )
  }))
  
  return(all_data)
}


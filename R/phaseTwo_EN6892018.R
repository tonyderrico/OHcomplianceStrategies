#' Phase 2, BOHS/NVvA & EN689 2018 - U test
#'
#' This function applies the Mann-Whitney U test (70% Confidence) to evaluate SEG compliance 
#' based on the number of measurements performed.
#' 
#' The test compares the calculated U value (pg. 42, EN689 2018) with the U threshold from the Mann-Whitney U table:
#' - If **U > OEL**, there is **Compliance**.
#' - If **U < OEL**, there is **Non-Compliance**.
#' 
#' The function includes U-thresholds for a maximum of 15 measurements per SEG.
#'
#' @param measurements Numeric vector. Measurements of the SEG under assessment (between 6 and 15).
#' @param OEL Numeric. The Occupational Exposure Limit of the agent.
#' 
#' @return A character string:
#' - "Compliant" if `U < threshold`
#' - "Not Compliant" if `U > threshold`
#'
#' @export
phase2_Uvalue <- function(measurements, OEL) {
  if (length(measurements) < 6) {
    stop("Error: At least 6 measurements are required.")
  }
  
  # Calculate U-value
  U <- (log(OEL) - log(geomean(measurements))) / log(geosd(measurements))
  
  # Define threshold values based on the number of samples
  thresholds <- c(2.005, 2.035, 2.072, 2.120, 2.187)
  threshold <- thresholds[min(length(measurements), length(thresholds))]
  
  # Check compliance
  if (U < threshold) {
    result <- "Compliant"
  } else {
    result <- "Not Compliant"
  }
  
  return(result)
}

#' Phase 2, EN689 2018 - UTLv (Upper Tolerance Limit value)
#' 
#' This function evaluates compliance with the Occupational Exposure Limit (OEL) 
#' based on the Upper Tolerance Limit value (UTLv), calculated with a 95% Percentile 
#' Confidence Level and a 70% Confidence Level.
#' 
#' The test compares the UTLv with the OEL:
#' - If **UTL > OEL**, there is exceedance → **"Not Compliant"**.
#' - If **UTL < OEL**, the probability of exceedance is acceptable → **"Compliant"**.
#' 
#' @param measurements Numeric vector. At least 6 exposure measurements from the SEG under assessment.
#' @param OEL Numeric. The Occupational Exposure Limit of the agent.
#' @return A character string:
#' - `"Not Compliant"` if `UTL > OEL`
#' - `"Compliant"` if `UTL < OEL`
#' 
#' @export

phase2_UTL <- function(measurements, OEL) {
  if (length(measurements) < 6) {
    stop("Error: At least 6 measurements are required.")
  }
  
  # Calculate upper tolerance limit (UTL)
  TL <- normtol.int(log(measurements), alpha = 0.3, P = 0.95, side = 1)
  UTL <- exp(TL$`1-sided.upper`)
  
  # Check compliance
  if (UTL < OEL) {
    result <- "Compliant"
  } else {
    result <- "Not Compliant"
  }
  
  return(result)
}

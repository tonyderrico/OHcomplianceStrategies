#'Phase 2, BOHS/NVvA & EN689 2018  - U test
#'
#' Mann-Whitney U test (70% Confidence) is operated to evaluate SEG compliance based on the number of measurements performed.
#' If the U value calculated (pg. 42, EN689 2018) is greater than U threshold established in the Mann-Whitney U table,
#' there is Compliance, differently, if the U value is lower, there is Non-Compliance.
#' The U-thresholds integrated in the function are for a maximum of 15 measurements per SEG.
#'
#' @param samples measurements of the SEG under assessment from 6 to 15
#' @param OEL Exposure Limit of the agent
#' @return U value > ("Not Compliant") or < ("Compliant")  U thresholds
#' @export

phase2_Uvalue <- function(samples, OEL) {
  # Calculate U-value
  U <- (log(OEL) - log(geomean(samples))) / log(geosd(samples))
  
  # Define threshold values based on the number of samples
  thresholds <- c(2.005, 2.035, 2.072, 2.120, 2.187)
  threshold <- thresholds[min(length(samples), length(thresholds))]
  
  # Check compliance
  compliance <- 
  if (U < threshold) {
    result <- "Compliant"
  } else {
    result <- "Not Compliant"
  }
  
  return(result)
}

#'Phase 2, EN689 2018 - UTL (Upper Tolerance Limit), 95% C.I., 70% C.L.
#'
#' The test is based on the comparison of the UTL having 95% P.C. with 70% Confidence
#' Level with the OEL. If the UTL is greater than OEL, there is exceedance and so Non-Compliance.
#' Contrarly, if the UTL is lower than OEL, the probability of exceedance is acceptable, so there is Compliance.
#' @param samples at least 6 measurements of the SEG under assessment
#' @param OEL Occupational Exposure Limit of the agent
#' @return UTL > OEL ("Not Compliant") or UTL < OEL ("Compliant")
#' @export

phase2_UTL <- function(samples, OEL) {
  # Calculate upper tolerance limit (UTL)
  TL <- normtol.int(log(samples), alpha = 0.3, P = 0.95, side = 1)
  UTL <- exp(TL$`1-sided.upper`)
  
  # Check compliance
  compliance <- 
  if(UTL < OEL) {
      result = "Compliant"
      }
  else {
        result = "Not Compliant"
    }
  
  return(result)
}
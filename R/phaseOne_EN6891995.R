#'Phase 1, EN689 1995  - testing compliance for 1 measurement
#'
#'Compliance, uncertain compliance or non-compliance can be evaluated
#'for one measurement. Calculation of exposure index is performed (worker measurement/OEL).
#'Compliance is achieved if the index is below or equal to 0.1.
#'If the index is greater than 0.1 but lower/equal than 1, there is no decision ("UC")
#'and additional measurements are necessary (at least three). Non-Compliance is accomplished
#'if the measurement is greater than the OEL and/or if the exposure index is greater than 1.
#' @param sample One measurement under assessment
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN1995_k1 <- function(sample, OEL) {
  x1 <- sample / OEL
  
  if (sample > OEL || x1 > 1) {
    result <- "NC"  # Not compliant
  } else if (x1 <= 0.1) {
    result <- "C"   # Compliant
  } else {
    result <- "UC"  # Uncertain compliance
  }
  
  return(result)
}

#'Phase 1, EN689 1995  - testing compliance for at least 3 measurements
#'
#'Compliance, uncertain compliance or non-compliance can be evaluated
#'for at least three measurements. Calculation of exposure indeces is performed (worker measurement/OEL).
#'Compliance is achieved if all indeces are below or equal to 0.25.
#'If at least an index is greater than 0.25 but all indeces are lower/equal than 1 and the
#'geometric mean of the measurements is lower/equal than 0.5, Compliance is accomplished.
#'Contrarly, there is no decision ("UC").
#'There is Non-Compliance if at least a measurement is greater than the OEL
#'and/or if at least an exposure index is greater than 1.
#' @param samples At least 3 measurements
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance (C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN1995_k <- function(samples, OEL) {
  ratios <- samples / OEL
  
  if (any(samples > OEL) || any(ratios > 1)) {
    result <- "NC"  # Not compliant
  } else if (any(ratios > 0.25) || geomean(samples) > 0.5) {
    result <- "UC"  # Uncertain compliance
  } else {
    result <- "C"   # Compliant
  }
  
  return(result)
}



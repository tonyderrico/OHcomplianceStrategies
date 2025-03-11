#'Phase 2, EN689 1995  - testing compliance exceedance
#'
#' The normal distribution of the measurements is operated, and 99.9th percentile and 95th percentile values are observed to assess the possible exceedance of the OEL.
#' Compliance or "Green Area" is achieved if the value of the 99,9 the percentile is lower than the OEL, Uncertain Compliance or "Orange Area" is achieved if
#' the value of the 99.9th percentile is lower than the OEL but the value of 95th percentil is greater than the OEL. 
#' Non Compliance or "Red Area" is achieved if the OEL is lower than the value of the 95th percentile.
#' @param measurements measurements of the SEG under assessment
#' @param OEL Occupational Exposure Limit of the agent
#' @return Green Area, Orange Area or Red Area
#' @export

phase2EN689.1995 <- function(measurements, OEL) {
  if (length(measurements) < 6) {
    stop("Error: At least 6 measurements are required.")
  }
  
  # Calculate log-normal percentiles
  QN <- qnorm(0.999, mean(log(measurements)), sd(log(measurements)))  # 99.9th percentile
  QN1 <- qnorm(0.95, mean(log(measurements)), sd(log(measurements)))  # 95th percentile
  
  # Classification logic
  if (log(OEL) > QN) {
    return("Green Area")
  } else if (log(OEL) > QN1 && log(OEL) < QN) {
    return("Orange Area")
  } else if (log(OEL) < QN1) {
    return("Red Area")
  } else {
    return("Error in classification")
  }
}

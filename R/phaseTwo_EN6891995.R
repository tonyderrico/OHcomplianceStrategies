#'Phase 2, EN689 1995  - testing compliance exceedance
#'
#' The normal distribution of the measurements is operated, and 99.9th percentile and 95th percentile values are observed to assess the possible exceedance of the OEL.
#' Compliance or "Green Area" is achieved if the value of the 99,9 the percentile is lower than the OEL, Uncertain Compliance or "Orange Area" is achieved if
#' the value of the 95th percentile is lower than the OEL. Non Compliance or "Red Area" is achieved if the OEL is greater than the value of the 95th percentile.
#' @param samples measurements of the SEG under assessment
#' @return Green Area, Orange Area or Red Area
#' @export

phase2EN689.1995 <- function(samples, OEL) {
  QN <- qnorm(0.999, mean(samples), sd(samples))
  QN1 <- qnorm(0.95, mean(samples), sd(samples))
  if(QN < OEL)
  {print("Green Area")}
  else if(QN1 < OEL)
  {print("Orange Area")}
  else if(QN1 > OEL)
  {print("Red Area")}}

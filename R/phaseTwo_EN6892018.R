#'Phase 2, BOHS/NVvA & EN689 2018  - U test
#'
#' Mann-Whitney U test (70% Confidence) is operated to evaluate SEG compliance based on the number of measurements performed.
#' If the U value calculated (pg. 42, EN689 2018) is greater than U threshold established in the Mann-Whitney U table,
#' there is Compliance, differently, if the U value is lower, there is Non-Compliance.
#' The U-thresholds integrated in the function are for a maximum of 15 measurements per SEG.
#'
#' @param samples measurements of the SEG under assessment from 6 to 15
#' @param Occupational Exposure Limit of the agent
#' @return U value > ("TRUE") or < ("FALSE)  U thresholds
#' @export

phase2_Uvalue <- function(samples, OEL){
  U <- (log(OEL) - log(geomean(samples))) / log(geosd(samples))
  if(length(samples) == 6)
  {U > 2.187}
  else if(length(samples) == 7)
  {U > 2.120}
  else if(length(samples) == 8)
  {U > 2.072}
  else if(length(samples) == 9)
  {U > 2.035}
  else if(length(samples) == 10)
  {U > 2.005}
  }

#'Phase 2, EN689 2018 - UTL (Upper Tolerance Limit), 95% C.I., 70% C.L.
#'
#' The test is based on the comparison of the UTL having 95% Confidence Interval with 70% Confidence
#' Level with the OEL. If the UTL is greater than OEL, there is exceedance and so Non-Compliance.
#' Contrarly, if the UTL is lower than OEL, the probability of exceedance is acceptable, so there is Compliance.
#' @param samples at least 6 measurements of the SEG under assessment
#' @param OEL Occupational Exposure Limit of the agent
#' @return UTL > OEL ("TRUE") or UTL < OEL ("FALSE")
#' @export

phase2_UTL <- function(samples, OEL) {
  TL <- normtol.int(log(samples), alpha = 0.05, P = 0.7, side = 1)
  UTL <- TL$`1-sided.upper`
  ifelse(exp(UTL) > OEL, "TRUE", "FALSE")
}




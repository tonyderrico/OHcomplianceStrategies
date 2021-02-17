#'Phase 1, EN689 1995  - testing compliance for 1 measurement
#'
#'Compliance, uncertain compliance or non-compliance can be evaluated
#'for one measurement. Calculation of exposure index is performed (worker measurement/OEL).
#'Compliance is achieved if the index is below or equal to 0.1.
#'If the index is greater than 0.1 but lower/equal than 1, there is no decision ("UC")
#'and additional measurements are necessary (at least three). Non-Compliance is accomplished
#'if the measurement is greater than the OEL and/or if the exposure index is greater than 1.
#' @param x One measurement under assessment
#' @param OEL Establish the OEL (numeric)
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

  phase1EN1995_k1 <- function(x, OEL) {
    x1 <- x/OEL
    if(x[1] > OEL || x1[1] > 1)
    {print("NC")}
    else if(x1[1] <= 0.1)
    {print("C")}
    else if(x1[1] <= 1 && x1[1] > 0.1)
    {print("UC")}}

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
#' @param x at least 3 measurements (numeric)
#' @param OEL Establish the OEL (numeric)
#' @return Compliance (C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

  phase1EN1995_k <- function(x, OEL) {
    x1 <- x/OEL
    if(any(x > OEL) || any(x1 > 1))
    {print("NC")}
    else if(any(x1 > 0.25) || geomean(x) > 0.5)
    {print("UC")}
    else if(all(x1 <= 0.25))
    {print("C")}
    else if(geomean(x) <= 0.5)
    {print("C")}}


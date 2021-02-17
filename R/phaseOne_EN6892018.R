#'Phase 1, EN689 2018  - testing compliance for 3 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be
#'evaluated for three measurements. Compliance is achieved if all of
#'the measurements are below or equal to 0.1xOEL. There is Uncertain Compliance if
#'at least a measurement is greater than 0.1xOEL but below the OEL.
#'There is Non-Compliance if at least a measurement is greater than OEL.
#' @param x Three measurements (numeric)
#' @param OEL Establish the OEL (numeric)
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

  phase1EN2018_k3 <- function(x, OEL) {
    ifelse(any(x[1:3] > OEL), "NC", ifelse(any(x[1:3] >= 0.1*OEL), "UC","C"))
  }

#'Phase 1, EN689 2018  - testing compliance for 4 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be
#'evaluated for four measurements. Compliance is achieved if all of the measurements
#'are below or equal to 0.15xOEL. There is Uncertain Compliance if at least a
#'measurement is greater than 0.15xOEL but below the OEL. There is Non-Compliance
#'if at least a measurement is greater than OEL.
#' @param x Four measurements (numeric)
#' @param OEL Establish the OEL (numeric)
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN2018_k4 <- function(x, OEL) {
  ifelse(any(x[1:4] > OEL), "NC", ifelse(any(x[1:4] >= 0.15*OEL), "UC","C"))}

#'Phase 1, EN689 2018  - testing compliance for 5 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be evaluated for five measurements. Compliance is achieved if all of the measurements are below or equal to 0.2xOEL. There is Uncertain Compliance if at least a measurement is greater than 0.2xOEL but below the OEL. There is Non-Compliance if at least a measurement is greater than OEL.
#' @param x Five measurements (numeric)
#' @param OEL Establish the OEL (numeric)
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN2018_k5 <- function(x, OEL) {
  ifelse(any(x[1:5] > OEL), "NC", ifelse(any(x[1:5] >= 0.2*OEL), "UC","C"))}

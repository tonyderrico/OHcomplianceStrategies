#'Phase 1, EN689 2018  - testing compliance for 3 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be
#'evaluated for three measurements. Compliance is achieved if all of
#'the measurements are below or equal to 0.1xOEL. There is Uncertain Compliance if
#'at least a measurement is greater than 0.1xOEL but below the OEL.
#'There is Non-Compliance if at least a measurement is greater than OEL.
#' @param samples Three measurements
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN2018_k3 <- function(samples, OEL) {
  if (length(samples) > 3) {
    stop("Dataset exceeds three measurements. Function will not run.")
  }
  compliance <- ifelse(any(samples > OEL), "Not Compliant", 
                       ifelse(any(samples >= 0.1 * OEL), "Uncertain Compliance", "Compliant"))
  return(compliance)
}

#'Phase 1, EN689 2018  - testing compliance for 4 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be
#'evaluated for four measurements. Compliance is achieved if all of the measurements
#'are below or equal to 0.15xOEL. There is Uncertain Compliance if at least a
#'measurement is greater than 0.15xOEL but below the OEL. There is Non-Compliance
#'if at least a measurement is greater than OEL.
#' @param x Four measurements 
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN2018_k4 <- function(samples, OEL) {
  if (length(samples) > 4) {
    stop("Dataset exceeds three measurements. Function will not run.")
  }
  compliance <- switch(
    TRUE,
    any(samples > OEL), "Not Compliant",
    any(samples >= 0.15 * OEL), "Uncertain Compliant",
    "Compliant"
  )
  return(compliance)
}


#'Phase 1, EN689 2018  - testing compliance for 5 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be evaluated for five measurements. Compliance is achieved if all of the measurements are below or equal to 0.2xOEL. There is Uncertain Compliance if at least a measurement is greater than 0.2xOEL but below the OEL. There is Non-Compliance if at least a measurement is greater than OEL.
#' @param samples Five measurements 
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN2018_k5 <- function(samples, OEL) {
  if (length(samples) > 5) {
    stop("Dataset exceeds three measurements. Function will not run.")
  }
  compliance <- if (any(samples > OEL)) {
    "Not Compliant"  # Not compliant
  } else if (any(samples >= 0.2 * OEL)) {
    "Uncertain Complaince"  # Uncertain compliance
  } else {
    "Compliant"   # Compliant
  }
  return(compliance)
}
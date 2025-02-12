#'OverExposure
#'
#' OverExposure Calculation
#' @param measurements measurements (with repeated measurements) of the SEG under assessment
#' @param workers workers codes/ID
#' @param samples exposure concentrations of workers
#' @param OEL Occupational Exposure Limit of the agent
#' @return probability of OverExposure
#' @export

OverExp <- function(measurements, workers, samples, OEL) {
  # Calculate mean concentration samples by workers
  mean_samples <- measurements %>%
    group_by(workers) %>%
    summarise(mean_samples = mean(measurements)) %>%
    pull(mean_samples)
  
  # Calculate overall mean of samples
  overall_mean <- mean(mean_samples)
  
  # Fit linear mixed-effects model
  model <- lmer(measurements ~ 1 + (1 | workers), data = seg)
  
  # Extract variance components using broom
  VC_random <- tidy(VarCorr(model))
  
  # Calculate standard deviations
  wwsd <- sqrt(VC_random$stddev[2])
  bwsd <- sqrt(VC_random$stddev[1])
  
  # Calculate overexposure
  overexpos <- (log(OEL) - (overall_mean + 0.5 * wwsd)) / bwsd
  
  # Calculate probability of overexposure
  prob_overexposure <- 1 - pnorm(overexpos)
  
  return(prob_overexposure)

  }

#'Exceedance
#'
#' Probability of the OEL exceedance of the SEG considered
#' @param measurements measurements of the SEG under assessment
#' @param OEL Occupational Exposure Limit of the agent 
#' @return probability of Exceedance
#' @export

Exceedance <- function(measurements, OEL) {
  # Calculate geometric mean
  GM <- exp(mean(log(measurements)))
  
  # Calculate geometric standard deviation
  GSD <- exp(sd(log(measurements)))
  
  # Calculate exceedance
  Exc <- (log(OEL) - log(GM)) / log(GSD)
  
  # Calculate probability of exceedance
  prob_exceedance <- 1 - pnorm(Exc)
  
  return(prob_exceedance)
}

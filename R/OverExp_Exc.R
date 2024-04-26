#'OverExposure
#'
#' OverExposure Calculation
#' @param seg measurements (with repeated measurements) of the SEG under assessment
#' @param workers workers codes/ID
#' @param samples exposure concentrations of workers
#' @param OEL Occupational Exposure Limit of the agent
#' @return % of OverExposure
#' @export

OverExp <- function(seg, workers, samples, OEL) {
  # Calculate mean concentration samples by workers
  mean_samples <- seg %>%
    group_by(workers) %>%
    summarise(mean_samples = mean(samples)) %>%
    pull(mean_samples)
  
  # Calculate overall mean of samples
  overall_mean <- mean(mean_samples)
  
  # Fit linear mixed-effects model
  model <- lmer(samples ~ 1 + (1 | workers), data = seg)
  
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
#' @param samples measurements of the SEG under assessment
#' @param OEL Occupational Exposure Limit of the agent 
#' @return % of Exceedance
#' @export

Exceedance <- function(samples, OEL) {
  # Calculate geometric mean
  GM <- exp(mean(log(samples)))
  
  # Calculate geometric standard deviation
  GSD <- exp(sd(log(samples)))
  
  # Calculate exceedance
  Exc <- (log(OEL) - log(GM)) / log(GSD)
  
  # Calculate probability of exceedance
  prob_exceedance <- 1 - pnorm(Exc)
  
  return(prob_exceedance)
}

#'Phase 3, BOHS/NvVA 2011  - Between Worker Variance
#'
#' REML (Restricted Maximum Likelihood) is performed to calculate between worker variance
#' and total variance. The Compliance is achieved if the between worker variance is lower
#' than 0.2 of the total variance. Contrarly, there is Non-Compliance.
#' @param seg measurements with repeats of the SEG under assessment
#' @param samples samples concentrations of the agent
#' @param workers workers codes/names/ID
#' @return BW < 0.2totalVariance ("TRUE"), BW > 0.2totalVariance ("FALSE")
#' @export

phase3_BoHS.NvVA <- function(seg, workers, samples) {
  # Fit linear mixed-effects model
  model <- lmer(samples ~ 1 + (1 | workers), data = seg)
  
  # Extract variance components
  VC_random <- VarCorr(model)
  vc_df <- as.data.frame(VC_random)
  bw <- vc_df$vcov[1]
  ww <- vc_df$vcov[2]
  
  # Calculate total variance
  total_variance <- bw + ww
  
  # Check compliance
  if (bw < 0.2 * total_variance) {
    result <- "Compliance"
  } else {
    result <- "Non-Compliance"
  }
  
  return(result)
}

#'Phase 3, BOHS/NVVA 2011 - Individual Compliance
#'
#' Individual Compliance is achieved when there is less than 20% probability that
#' workers in a SEG have more than 5% of exposure greater than the OEL.
#' @param seg measurements with repeats of the SEG under assessment
#' @param samples samples concentrations of the agent
#' @param workers workers code/name
#' @return BW < 0.2totalVariance, True or False
#' @export


Individual_Compliance <- function(seg, workers, samples, OEL) {
  # Calculate mean samples by workers
  M <- seg %>% 
    group_by(workers) %>% 
    summarise(mean = mean(samples))
  
  # Calculate overall mean of samples
  M1 <- mean(M$mean)
  
  # Fit linear mixed-effects model
  t <- lmer(samples ~ 1 + (1 | workers), data = seg)
  
  # Extract variance components
  VCrandom <- VarCorr(t)
  vv <- as.data.frame(VCrandom)
  
  # Calculate standard deviations
  wwsd <- sqrt(vv$vcov[2])
  bwsd <- sqrt(vv$vcov[1])
  
  # Calculate compliance index
  H <- (log(OEL) - (M1 + 1.645 * wwsd)) / bwsd
  IE <- 1 - pnorm(H)
  
  # Determine compliance status
  if (IE < 0.2) {
    result <- "Compliance"
  } else {
    result <- "Non-Compliance"
  }
  
  return(result)
}
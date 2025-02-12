#'Phase 3, BOHS/NvVA 2011  - Between Worker Variance
#'
#' Linear Mixed models using REML (Restricted Maximum Likelihood) is performed to calculate between worker variance
#' and total variance. The Compliance is achieved if the between worker variance is lower
#' than 0.2 of the total variance. Contrarily, there is Non-Compliance.
#' @param seg measurements with repeats of the SEG under assessment
#' @param samples samples concentrations of the agent
#' @param workers workers codes/names/ID
#' @return BW < 0.2totalVariance ("Compliant"), BW > 0.2totalVariance ("Not Compliant")
#' @export

phase3_BoHS.NvVA <- function(seg, workers, samples) {
  if (length(samples) < 6) {
    stop("Error: At least 6 measurements are required.")
  }
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
    result <- "Compliant"
  } else {
    result <- "Not compliant"
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
#' @param OEL Occupational Exposure Limit
#' @return BW < 0.2totalVariance, Compliant or Not Compliant
#' @export


Individual_Compliance <- function(seg, workers, samples, OEL) {
  # Calculate mean samples by workers
  M <- seg %>% 
    group_by(workers) %>% 
    na.omit() %>%  
    summarise(mean = mean(log(samples)))
  
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
    result <- "Compliant"
  } else {
    result <- "Not Compliant"
  }
  
  return(result)
}

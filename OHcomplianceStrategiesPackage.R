#OHcomplianceStrategies Package

#'Phase 1, EN689 1995  - testing compliance for 1 measurement
#'
#'Compliance, uncertain compliance or non-compliance can be evaluated
#'for one measurement. Calculation of exposure index is performed (worker measurement/OEL).
#'Compliance is achieved if the index is below or equal to 0.1.
#'If the index is greater than 0.1 but lower/equal than 1, there is no decision ("UC")
#'and additional measurements are necessary (at least three). Non-Compliance is accomplished
#'if the measurement is greater than the OEL and/or if the exposure index is greater than 1.
#' @param measurements One measurement under assessment
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN1995_k1 <- function(measurements, OEL) {
  x1 <- measurements / OEL
  
  if (measurements > OEL || x1 > 1) {
    result <- "Non Compliant"  # Not compliant
  } else if (x1 <= 0.1) {
    result <- "Compliant"   # Compliant
  } else {
    result <- "Uncertain Compliance"  # Uncertain compliance
  }
  
  return(result)
}

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
#' @param measurements At least 3 measurements
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance (C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN1995_k <- function(measurements, OEL) {
  ratios <- measurements / OEL
  
  if (any(measurements > OEL) || any(ratios > 1)) {
    result <- "Not Compliant"  # Not compliant
  } else if (any(ratios > 0.25) || geomean(measurements) > 0.5) {
    result <- "Uncertain Compliance"  # Uncertain compliance
  } else {
    result <- "Compliant"   # Compliant
  }
  
  return(result)
}


#'Phase 1, EN689 2018  - testing compliance for 3 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be
#'evaluated for three measurements. Compliance is achieved if all of
#'the measurements are below or equal to 0.1xOEL. There is Uncertain Compliance if
#'at least a measurement is greater than 0.1xOEL but below the OEL.
#'There is Non-Compliance if at least a measurement is greater than OEL.
#' @param measurements Three measurements
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN2018_k3 <- function(measurements, OEL) {
  if (length(measurements) > 3) {
    stop("Dataset exceeds three measurements. Function will not run.")
  }
  compliance <- ifelse(any(measurements > OEL), "Not Compliant", 
                       ifelse(any(measurements >= 0.1 * OEL), "Uncertain Compliance", "Compliant"))
  return(compliance)
}

#'Phase 1, EN689 2018  - testing compliance for 4 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be
#'evaluated for four measurements. Compliance is achieved if all of the measurements
#'are below or equal to 0.15xOEL. There is Uncertain Compliance if at least a
#'measurement is greater than 0.15xOEL but below the OEL. There is Non-Compliance
#'if at least a measurement is greater than OEL.
#' @param measurements Four measurements 
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN2018_k4 <- function(measurements, OEL) {
  if (length(measurements) > 4) {
    stop("Dataset exceeds three measurements. Function will not run.")
  }
  compliance <- switch(
    TRUE,
    any(measurements > OEL), "Not Compliant",
    any(measurements >= 0.15 * OEL), "Uncertain Compliant",
    "Compliant"
  )
  return(compliance)
}


#'Phase 1, EN689 2018  - testing compliance for 5 measurements
#'
#'Compliance, uncertain compliance or non-compliance of a SEG can be evaluated for five measurements. Compliance is achieved if all of the measurements are below or equal to 0.2xOEL. There is Uncertain Compliance if at least a measurement is greater than 0.2xOEL but below the OEL. There is Non-Compliance if at least a measurement is greater than OEL.
#' @param measurements Five measurements 
#' @param OEL Occupational Exposure Limit of the agent
#' @return Compliance ("C"), Non-Compliance ("NC") or Uncertainty of Compliance ("UC")
#' @export

phase1EN2018_k5 <- function(measurements, OEL) {
  if (length(measurements) > 5) {
    stop("Dataset exceeds three measurements. Function will not run.")
  }
  compliance <- if (any(measurements > OEL)) {
    "Not Compliant"  # Not compliant
  } else if (any(measurements >= 0.2 * OEL)) {
    "Uncertain Complaince"  # Uncertain compliance
  } else {
    "Compliant"   # Compliant
  }
  return(compliance)
}

#'Phase 2, EN689 1995  - testing compliance exceedance
#'
#' The normal distribution of the measurements is operated, and 99.9th percentile and 95th percentile values are observed to assess the possible exceedance of the OEL.
#' Compliance or "Green Area" is achieved if the value of the 99,9 the percentile is lower than the OEL, Uncertain Compliance or "Orange Area" is achieved if
#' the value of the 95th percentile is lower than the OEL. Non Compliance or "Red Area" is achieved if the OEL is greater than the value of the 95th percentile.
#' @param measurements measurements of the SEG under assessment
#' @param OEL Occupational Exposure Limit of the agent
#' @return Green Area, Orange Area or Red Area
#' @export

phase2EN689.1995 <- function(measurements, OEL) {
  if (length(measurements) < 6) {
    stop("Error: At least 6 measurements are required.")
  }
  QN <- qnorm(0.999, mean(log(measurements)), sd(log(measurements)))
  QN1 <- qnorm(0.95, mean(log(measurements)), sd(log(measurements)))
  if(QN < log(OEL))
  {print("Green Area")}
  else if(QN1 < log(OEL))
  {print("Orange Area")}
  else if(QN1 > log(OEL))
  {print("Red Area")}}

#' Phase 2, BOHS/NVvA & EN689 2018 - U test
#'
#' This function applies the Mann-Whitney U test (70% Confidence) to evaluate SEG compliance 
#' based on the number of measurements performed.
#' 
#' The test compares the calculated U value (pg. 42, EN689 2018) with the U threshold from the Mann-Whitney U table:
#' - If **U > OEL**, there is **Compliance**.
#' - If **U < OEL**, there is **Non-Compliance**.
#' 
#' The function includes U-thresholds for a maximum of 15 measurements per SEG.
#'
#' @param measurements Numeric vector. Measurements of the SEG under assessment (between 6 and 15).
#' @param OEL Numeric. The Occupational Exposure Limit of the agent.
#' 
#' @return A character string:
#' - "Compliant" if `U < threshold`
#' - "Not Compliant" if `U > threshold`
#'
#' @export
phase2_Uvalue <- function(measurements, OEL) {
  if (length(measurements) < 6) {
    stop("Error: At least 6 measurements are required.")
  }
  
  # Calculate U-value
  U <- (log(OEL) - log(geomean(measurements))) / log(geosd(measurements))
  
  # Define threshold values based on the number of samples
  thresholds <- c(2.005, 2.035, 2.072, 2.120, 2.187)
  threshold <- thresholds[min(length(measurements), length(thresholds))]
  
  # Check compliance
  if (U < threshold) {
    result <- "Compliant"
  } else {
    result <- "Not Compliant"
  }
  
  return(result)
}

#' Phase 2, EN689 2018 - UTLv (Upper Tolerance Limit value)
#' 
#' This function evaluates compliance with the Occupational Exposure Limit (OEL) 
#' based on the Upper Tolerance Limit value (UTLv), calculated with a 95% Percentile 
#' Confidence Level and a 70% Confidence Level.
#' 
#' The test compares the UTLv with the OEL:
#' - If **UTL > OEL**, there is exceedance → **"Not Compliant"**.
#' - If **UTL < OEL**, the probability of exceedance is acceptable → **"Compliant"**.
#' 
#' @param measurements Numeric vector. At least 6 exposure measurements from the SEG under assessment.
#' @param OEL Numeric. The Occupational Exposure Limit of the agent.
#' @return A character string:
#' - `"Not Compliant"` if `UTL > OEL`
#' - `"Compliant"` if `UTL < OEL`
#' 
#' @export

phase2_UTL <- function(measurements, OEL) {
  if (length(measurements) < 6) {
    stop("Error: At least 6 measurements are required.")
  }
  
  # Calculate upper tolerance limit (UTL)
  TL <- normtol.int(log(measurements), alpha = 0.3, P = 0.95, side = 1)
  UTL <- exp(TL$`1-sided.upper`)
  
  # Check compliance
  if (UTL < OEL) {
    result <- "Compliant"
  } else {
    result <- "Not Compliant"
  }
  
  return(result)
}

#'Phase 3, BOHS/NvVA 2011  - Between Worker Variance
#'
#' Linear Mixed models using REML (Restricted Maximum Likelihood) is performed to calculate between worker variance
#' and total variance. The Compliance is achieved if the between worker variance is lower
#' than 0.2 of the total variance. Contrarily, there is Non-Compliance.
#' @param seg data of SEG under assessment
#' @param measurements samples concentrations of the agent
#' @param workers workers codes/names/ID
#' @return BW < 0.2totalVariance ("Compliant"), BW > 0.2totalVariance ("Not Compliant")
#' @export

phase3_BoHS.NvVA <- function(seg, workers, measurements) {
  if (length(measurements) < 6) {
    stop("Error: At least 6 measurements are required.")
  }
  # Fit linear mixed-effects model
  model <- lmer(measurements ~ 1 + (1 | workers), data = seg)
  
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
#' @param seg data of the SEG under assessment
#' @param measurements samples concentrations of the agent
#' @param workers workers code/name
#' @param OEL Occupational Exposure Limit
#' @return BW < 0.2totalVariance, Compliant or Not Compliant
#' @export


Individual_Compliance <- function(seg, workers, measurements, OEL) {
  # Calculate mean samples by workers
  M <- seg %>% 
    group_by(workers) %>% 
    na.omit() %>%  
    summarise(mean = mean(log(measurements)))
  
  # Calculate overall mean of samples
  M1 <- mean(M$mean)
  
  # Fit linear mixed-effects model
  t <- lmer(measurements ~ 1 + (1 | workers), data = seg)
  
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

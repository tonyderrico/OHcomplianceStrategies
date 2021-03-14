#'OverExposure
#'
#' OverExposure Calculation
#' @param seg measurements and repeats of the SEG under assessment
#' @param workers workers codes/names/ID
#' @param samples samples concentrations of the exposed workers
#' @param OEL Occupational Exposure Limit of the agent
#' @return % of OverExposure
#' @export

OverExp <- function(seg, workers, samples, OEL) {
  M <- xA %>% group_by(workers) %>% summarise(mean = mean(samples))
  M1 <- mean(M$mean)
  xA <- lmer(samples~1 + ( 1| workers), data = seg)
  VCrandom <- VarCorr(xA)
  vv <- as.data.frame(VCrandom)
  wwsd <- sqrt(vv$vcov[2])
  bwsd <- sqrt(vv$vcov[1])
  overexpos <- (log(OEL) - (M1 + 0.5*wwsd)) / bwsd
  1 - pnorm(overexpos)
}

#'Exceedance
#'
#' Probability of the OEL exceedance of the SEG considered
#' @param samples measurements of the SEG under assessment
#' @param OEL Occupational Exposure Limit of the agent 
#' @return % of Exceedance
#' @export

  Exceedance <- function(samples,OEL) {
    GM <- geomean(samples)
    GSD <- geosd(samples)
    Exc <- (log(OEL) - log(GM)) / log(GSD)
    1 - pnorm(Exc)
  }

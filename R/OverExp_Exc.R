#'OverExposure
#'
#' OverExposure Calculation
#' @param x measurements and repeats of the SEG under assessment
#' @return % of OverExposure
#' @export

OverExp <- function(samples, workers, agent, OEL) {
  M <- xA %>% group_by(workers) %>% summarise(mean = mean(agent))
  M1 <- mean(M$mean)
  xA <- lmer(agent~1 + ( 1| workers), data = samples)
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
#' @param x measurements under assessment
#' @param OEL concentration 
#' @return % of Exceedance
#' @export

  Exceedance <- function(samples,OEL) {
    GM <- geomean(x)
    GSD <- geosd(x)
    Exc <- (log(OEL) - log(GM)) / log(GSD)
    1 - pnorm(Exc)
  }

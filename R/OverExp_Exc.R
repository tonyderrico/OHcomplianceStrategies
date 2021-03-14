#'OverExposure
#'
#' OverExposure Calculation
#' @param x measurements and repeats of the SEG under assessment
#' @return % of OverExposure
#' @export

OverExp <- function(x, Worker, Agent, OEL) {
  M <- xA %>% group_by(Worker) %>% summarise(mean = mean(Agent))
  M1 <- mean(M$mean)
  xA <- lmer(Agent~1 + ( 1| Worker), data = x )
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

  Exceedance <- function(x, OEL) {
    GM <- geomean(x)
    GSD <- geosd(x)
    Exc <- (log(OEL) - log(GM)) / log(GSD)
    1 - pnorm(Exc)
  }

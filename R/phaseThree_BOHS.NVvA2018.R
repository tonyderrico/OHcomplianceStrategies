#'Phase 3, BOHS/NvVA 2018  - Between Worker Variance
#'
#' REML (Restricted Maximum Likelihood) is performed to calculate the between worker variance
#' and the total variance. The Compliance is achieved if the between worker variance is lower
#' than 0.2 of the total variance. Contrarly, there is Non-Compliance.
#' @param x measurements with repeats of the SEG under assessment
#' @param Agent agent concentrations of exposed workers
#' @param Worker workers codes/names
#' @return BW < 0.2totalVariance ("TRUE"), BW > 0.2totalVariance ("FALSE")
#' @export



  phase3BoHS.NvVA <- function(x, Worker, Agent) {
    xA <- lmer(Agent~1 + ( 1| Worker), data = x )
    VCrandom <- VarCorr(xA)
    vv <- as.data.frame(VCrandom)
    ww <- vv$vcov[2]
    bw <- vv$vcov[1]
    total_variance <- bw + ww
    ifelse(bw < 0.2*total_variance, "TRUE", "FALSE")
  }

#'Phase 3, BOHS/NVVA 2018 - Individual Compliance
#'
#' Individual Compliance is achieved when there is less than 20% probability that
#' workers in a SEG have more than 5% of exposure greater than the OEL.
#' @param x measurements with repeats of the SEG under assessment
#' @param Agent agent concentrations
#' @param Worker workers code/name
#' @return BW < 0.2totalVariance, True or False
#' @export


 Ind.Compl <- function(x, Worker, Agent, OEL) {
    M <- x %>% group_by(Worker) %>% summarise(mean = mean(Agent))
    M1 <- mean(M$mean)
    t <- lmer(Agent~1 + ( 1| Worker), data = x )
    VCrandom <- VarCorr(t)
    vv <- as.data.frame(VCrandom)
    wwsd <- sqrt(vv$vcov[2])
    bwsd <- sqrt(vv$vcov[1])
    H <- (log(OEL) - (M1 + 1.645*wwsd)) / bwsd
    IE <- 1 - pnorm(H)
    ifelse(IE < 0.2, "TRUE", "FALSE")
 }

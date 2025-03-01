% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phaseTwo_EN6892018.R
\name{phase2_UTL}
\alias{phase2_UTL}
\title{Phase 2, EN689 2018 - UTLv (Upper Tolerance Limit value)}
\usage{
phase2_UTL(measurements, OEL)
}
\arguments{
\item{measurements}{Numeric vector. At least 6 exposure measurements from the SEG under assessment.}

\item{OEL}{Numeric. The Occupational Exposure Limit of the agent.}
}
\value{
A character string:
- `"Not Compliant"` if `UTL > OEL`
- `"Compliant"` if `UTL < OEL`
}
\description{
This function evaluates compliance with the Occupational Exposure Limit (OEL) 
based on the Upper Tolerance Limit value (UTLv), calculated with a 95% Percentile 
Confidence Level and a 70% Confidence Level.
}
\details{
The test compares the UTLv with the OEL:
- If **UTL > OEL**, there is exceedance → **"Not Compliant"**.
- If **UTL < OEL**, the probability of exceedance is acceptable → **"Compliant"**.
}

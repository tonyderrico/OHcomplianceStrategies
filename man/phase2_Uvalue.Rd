% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phaseTwo_EN6892018.R
\name{phase2_Uvalue}
\alias{phase2_Uvalue}
\title{Phase 2, BOHS/NVvA & EN689 2018 - U test}
\usage{
phase2_Uvalue(measurements, OEL)
}
\arguments{
\item{measurements}{Numeric vector. Measurements of the SEG under assessment (between 6 and 15).}

\item{OEL}{Numeric. The Occupational Exposure Limit of the agent.}
}
\value{
A character string:
- "Compliant" if `U < threshold`
- "Not Compliant" if `U > threshold`
}
\description{
This function applies the Mann-Whitney U test (70% Confidence) to evaluate SEG compliance 
based on the number of measurements performed.
}
\details{
The test compares the calculated U value (pg. 42, EN689 2018) with the U threshold from the Mann-Whitney U table:
- If **U > OEL**, there is **Compliance**.
- If **U < OEL**, there is **Non-Compliance**.

The function includes U-thresholds for a maximum of 15 measurements per SEG.
}

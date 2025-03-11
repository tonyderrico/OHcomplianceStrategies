# OHcomplianceStrategies

The EN689:1995 and EN689:2018 are the European standards used to assess the compliance of similarly exposed groups (SEGs) of workers with airborne substance regulations and to evaluate worker exposures against occupational exposure limit values (OELs).

The "Preliminary Test" and "Statistical Test" are two assessment methods based on the number of measurements collected in the workplace. These tests are included in both EN689 standards and are implemented in the package as "Phase One" and "Phase Two," respectively.

The BOHS-NVvA (2011) outlines the "Individual Compliance Test" and the "Between-Worker Variance Test" to evaluate exposure compliance while accounting for temporal variability in (average) exposure within and between workers in a SEG. These are implemented in the package as the functions "Phase3" and "Individual Compliance."

Additionally, calculations for overexposure and exceedance have been integrated into the package.

Dependences: #tolerance, #lme4, #dplyr, #FSA

# lans2r 1.1.0

## Major changes

No major functionality changes.

## Bug fixes

The following changes were made to comply with changes in package dependencies and base R:

 - this package now adheres to the strict use of `TRUE` and `FALSE` instead of `T` and `F`
 - this package now no longer uses the deprecated `dplyr` helper functions `mutate_()`, `filter_()`, `select_()`, etc.
 - this package now switched to using `rlang` instead of `lazyeval`

# lans2r 1.0.5

## Major Features

#### Data import

 - import and combine LANS matlab ion map files from multiple analyses
 - import and combine LANS ROI summary and z-stack files from multiple analyses
 - import tuning information from HMR text files
 
#### Processing
 
 - calculate any derived quantities and errors for ROIs and ion maps across multiple analyses and based on any combination of raw and prior derived quantities
 - automatically calculate ratios, fractional abundances and ion sums 
 
#### Visualization

 - plot paneled ion maps with ROI outlines for any number of ions and analyses
 

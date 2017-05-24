# Functions for errors from counting statistic 
# These are currently not exported, just used for internal calculations.
# 
# @param NM is the major isotope
# @param Nm is the minor isotope
# 
# - error in measured ion counts:
#   - absolute: $\sqrt{N}$
#   - relative: $\frac{\sqrt{N}}{N} = \frac{1}{\sqrt{N}}$
#   - relative error in measured isotope ratio R (directly from counts or from N_M and ratio):
#   - $\frac{\sigma_R}{R} = \sqrt{\frac{1}{N_M} + \frac{1}{N_m}}$ 
#   - $\frac{\sigma_R}{R} = \sqrt{\frac{1}{N_M} \frac{1+R}{R}}$
#   - $N_M$ is the total counts of the major ion, $N_m$ the total counts of the minor ion
# - error in $\delta$ (comparing isotope ratios):
#   - $\sigma_\delta = \sqrt{2\left(\frac{1}{N_M} + \frac{1}{N_m}\right)} \approx  \sqrt{\frac{2}{N_m}}$ since usually $N_m < 10^{-2} N_M$ 
#   - *Note*: for delta notation in permil, need to add factor $10^3$: $\sigma_\delta = 1000 \sqrt{\frac{2}{N_M}}$
#   
# all err... functions return sample standard deviations

iso.relErrN <- function(N) 1/sqrt(N)
iso.errN <- function(N) sqrt(N)

iso.R <- function(NM, Nm) Nm/NM
iso.relErrR <- function(NM, Nm) sqrt(1/NM + 1/Nm)
iso.errR <- function(NM, Nm) Nm/NM * iso.relErrR(NM, Nm)

iso.F <- function(NM, Nm) Nm/(NM + Nm)
iso.relErrF <- function(NM, Nm) (1 - iso.F(NM, Nm)) * sqrt(1/NM + 1/Nm)
iso.errF <- function(NM, Nm) iso.F(NM, Nm) * iso.relErrF(NM, Nm)

iso.absErrD<-function(NM, Nm) sqrt(2*(1/NM + 1/Nm))
iso.absErrDx<-function(NM, Nm) 1000*iso.absErrD(NM, Nm)
### Sensitivity analyses in response to reviewer comments
# Tom Yates, tidied November 2023

library(mixtools)

################################################################################

# Note, the sensitivity analyses use simpler code
# The underlying model is still normalmixEM
# The purpose here is to explore model instability not generate figures for publication

################################################################################

## Input data
# x is the raw data
# z is the same data smoothed using a three point rolling average

x <- rep(1:23,c(0, 0, 2, 2, 6, 7, 15, 8, 20, 10, 11, 17, 21, 25, 21, 18, 9, 6, 8, 3, 2, 2, 1))
z <- rep(1:24,c(0, 0.666666667,	1.333333333,	3.333333333,	5,	9.333333333,	10,	14.33333333,	12.66666667,	13.66666667,	12.66666667,	16.33333333,	21,	22.33333333,	21.33333333,	16,	11,	7.666666667,	5.666666667,	4.333333333,	2.333333333,	1.666666667, 1, 0.333333333))

################################################################################

## Reproduce main analysis

hist(x)
mixmdl2 <- normalmixEM(x, k = 2, mean.constr = c("a", "b"))
plot(mixmdl2,which=2)
lines(density(x), lty=2, lwd=2)

# This is the same underlying model, the output remains unstable

################################################################################

## Sensitivity analysis 1: pre-specifying various starting values of 'vector of component means'

# 3mm, 14mm

hist(x)
mixmdl3 <- normalmixEM(x, k = 2, mu = c(3,14), mean.constr = c("a", "b"))
plot(mixmdl3,which=2)
lines(density(x), lty=2, lwd=2)

# 3mm, 16mm

hist(x)
mixmdl4 <- normalmixEM(x, k = 2, mu = c(3,16), mean.constr = c("a", "b"))
plot(mixmdl4,which=2)
lines(density(x), lty=2, lwd=2)

# 3mm, 18mm

hist(x)
mixmdl5 <- normalmixEM(x, k = 2, mu = c(3,18), mean.constr = c("a", "b"))
plot(mixmdl5,which=2)
lines(density(x), lty=2, lwd=2)

# 5mm, 14mm

hist(x)
mixmdl6 <- normalmixEM(x, k = 2, mu = c(5,14), mean.constr = c("a", "b"))
plot(mixmdl6,which=2)
lines(density(x), lty=2, lwd=2)

# 5mm, 16mm

hist(x)
mixmdl7 <- normalmixEM(x, k = 2, mu = c(5,16), mean.constr = c("a", "b"))
plot(mixmdl7,which=2)
lines(density(x), lty=2, lwd=2)

# 5mm, 18mm

hist(x)
mixmdl8 <- normalmixEM(x, k = 2, mu = c(5,18), mean.constr = c("a", "b"))
plot(mixmdl8,which=2)
lines(density(x), lty=2, lwd=2)

# 7mm, 14mm

hist(x)
mixmdl9 <- normalmixEM(x, k = 2, mu = c(7,14), mean.constr = c("a", "b"))
plot(mixmdl9,which=2)
lines(density(x), lty=2, lwd=2)

# 7mm, 16mm

hist(x)
mixmdl10 <- normalmixEM(x, k = 2, mu = c(7,16), mean.constr = c("a", "b"))
plot(mixmdl10,which=2)
lines(density(x), lty=2, lwd=2)

# 7mm, 18mm

hist(x)
mixmdl11 <- normalmixEM(x, k = 2, mu = c(7,18), mean.constr = c("a", "b"))
plot(mixmdl11,which=2)
lines(density(x), lty=2, lwd=2)

# All of these models remain unstable, despite pre-specifying starting values

################################################################################

## Sensitivity analysis 2: analysing data smoothed using a three point rolling average

hist(z)
mixmdl12 <- normalmixEM(z, k = 2, mean.constr = c("a", "b"))
plot(mixmdl12,which=2)
lines(density(z), lty=2, lwd=2)

# Model remains unstable

################################################################################

## Sensitivity analysis 3: allowing a third component distribution
# e.g. BCG, environmental mycobacteria, Mtb

hist(x)
mixmdl13 <- normalmixEM(x, k = 3, mean.constr = c("a", "b", "c"))
plot(mixmdl13,which=2)
lines(density(x), lty=2, lwd=2)

# This model appears stable
# Result makes no biological sense in a population not recently exposed to BCG

################################################################################

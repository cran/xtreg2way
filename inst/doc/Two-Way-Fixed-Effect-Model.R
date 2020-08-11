## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(xtreg2way)

## -----------------------------------------------------------------------------
numgroups <- 1000
T <- 200

## -----------------------------------------------------------------------------
observations <- numgroups * T
e <- 1:observations
## Create groups and weights
hhid <- floor((e - 1) / T + 1)
tid <-  e - (hhid - 1) * T
w <- pracma::rand(n = numgroups, m = 1)
w <- w[hhid]

## -----------------------------------------------------------------------------
#Randomly create effects for groups
heffect <- pracma::randn(n = numgroups, m = 1)
teffect <- pracma::randn(n = T, m = 1)
#Generate independent variables 
x1 <- pracma::randn(n = observations, m = 1) + 
  0.5 * heffect[hhid] + 0.25 * teffect[tid]
x2 <- pracma::randn(n = observations, m = 1) - 
  0.25 * heffect[hhid] + 0.5 * teffect[tid]

## -----------------------------------------------------------------------------
#Generate Random Error
autoc <- pracma::rand(n = numgroups, m = 1)
initialv <- pracma::randn(n = numgroups, m = 1)
u <- pracma::randn(n = observations, m = 1)
for (o in 1:observations) {
  if (tid[o] > 1){
    u_1 <- u[o-1]
  } else {
    u_1 <- initialv[hhid[o]]
  }
  u[o] <- autoc[hhid[o]] * u_1 + u[o]
}
# Generate dependent variable
y <- 1 + x1 - x2 + heffect[hhid] + teffect[tid] + u

## -----------------------------------------------------------------------------
#XTREG2WAY
output <- xtreg2way(y, data.frame(x1,x2), hhid, tid, w, noise="1")

## -----------------------------------------------------------------------------
#XTREG2WAY second time
output2 <- xtreg2way(y, x1, struc=output$struc)


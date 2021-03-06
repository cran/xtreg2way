% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xtreg2way.R
\name{xtreg2way}
\alias{xtreg2way}
\alias{xtreg2way.formula}
\alias{xtreg2way.default}
\title{Estimation of Two Way Fixed Effects Model}
\usage{
xtreg2way(y, ...)

\method{xtreg2way}{formula}(
  formula,
  data,
  iid = NULL,
  tid = NULL,
  w = NULL,
  struc = NULL,
  se = "",
  noise = "",
  ...
)

\method{xtreg2way}{default}(
  y,
  X,
  iid = NULL,
  tid = NULL,
  w = NULL,
  struc = NULL,
  se = "",
  noise = "",
  ...
)
}
\arguments{
\item{y}{Either a numeric of data, or a formula}

\item{...}{Other parameters, based on method used}

\item{formula}{Model specifications}

\item{data}{A dataframe with labels from the formula \code{y}}

\item{iid}{(optional) The group ID, size N-by-1 - not needed if \code{struc} is provided}

\item{tid}{(optional) The time ID, size N-by-1 - not needed if \code{struc} is provided}

\item{w}{(optional) The vector of weights, size N-by-1.  If omitted, w will be set to 1 for all observations}

\item{struc}{(optional) This list contains the results from the first step of the algorithm.  To save computational time, you can rerun the algorithm on different columns by providing this struc.}

\item{se}{(optional) This indicates standard error estimate to be calculcated.  Possible values include:
se=="0" : standard errors assuming homoscedasticity and no within  group correlation or serial correlation.
se=="1" : standard errors  proposed by Arellano (1987) robust to heteroscedasticity and serial correlation.
se=="2" : standard errors robust to heteroscedasticity but assumes no correlation within group or serial correlation.
se=="11" : Arellano standard errors with a degree of freedom correction performed by Stata xtreg, fe.
If se is omitted or set to [] then it is set to 1 and the Arellano (1987) estimator is computed.}

\item{noise}{(optional) If noise is set to "1", then results are displayed}

\item{X}{A matrix of data}
}
\value{
\code{betaHat} (K-by-1) vector of estimated coefficients

\code{aVarHat} (K-by-K) estimate of the matrix of variances and covariance of  the estimator.

\code{y} (N-by-1) the residual of the projection of y on the two sets of  dummies.

\code{X} (N-by-K) the residual of the projection of each column of X on the two  sets of dummies.

\code{struc} (list) results of the first step of the algorithm.
}
\description{
\code{xtreg2way} estimates a 2-way fixed effect model absorbing the two set of dummies and reports standard errors.
}
\section{Methods (by class)}{
\itemize{
\item \code{formula}: This function ingests a formula as the first argument,
and requires \code{data} as a data.frame

\item \code{default}: Default Method
}}

\examples{
hhid <- c("a","b","c","a","b","c" ,"a","b","c" ,"a","b","c" ,"a","b","c")
tid <- c("1","1" ,"1" ,"2","2" ,"3","3","3" ,"4","4","5" ,"5","6","6" ,"6")
w <- rep(1, 15)
x1 <- rnorm(15, mean=50, sd=10)   
x2 <- rnorm(15, mean=50, sd=10)   
y <- x1 + rnorm(15, mean=50, sd=10)    
#The most basic way to use this function
output <- xtreg2way(y, x1, hhid, tid, w, se="2", noise="1")
#You can rerun faster with different columns using output$struc
output2 <- xtreg2way(y, data.frame(x1,x2), struc=output$struc)
#Or you can use a formula and specify data=
output3 <- xtreg2way(y~x1+x2, data=data.frame(x1,x2,y), iid=hhid, tid=tid, w=w, 
                     se="2", noise="1")
           
}

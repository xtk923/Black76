% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/src.R
\name{callPrice}
\alias{callPrice}
\title{A function that updates call price according to black 76 formula}
\usage{
callPrice(Vol, F, K, T, r)
}
\arguments{
\item{Vol}{Volatility, in the context of this package, it is a trial value}

\item{F}{Future price of the option}

\item{K}{Strike price of the option}

\item{T}{Time to expiry in year}

\item{r}{Risk free interst rate}
}
\value{
c: The call price
}
\description{
A function that updates call price according to black 76 formula
}

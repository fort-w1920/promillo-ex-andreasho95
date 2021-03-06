% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/promillo.R
\name{get_bodywater}
\alias{get_bodywater}
\title{Calculates the person's body water}
\usage{
get_bodywater(sex = c("male", "female"), age, height, weight)
}
\arguments{
\item{sex}{Character that matches either "male" or "female".}

\item{age}{Number between 10 and 110.}

\item{height}{Number between 100 and 230 [in cm]}

\item{weight}{Number between 40 and 300 [in kg]}
}
\value{
Estimated body water of the person
}
\description{
Calculates the person's body water
}

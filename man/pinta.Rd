% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dog_functions.R
\name{pinta}
\alias{pinta}
\title{pinta}
\usage{
pinta(est, cv, hoja, ruta, salva)
}
\arguments{
\item{est}{The estimations data frame.}

\item{cv}{The coefficients of variation data frame.}

\item{hoja}{Sheet number to print the data painted.}

\item{ruta}{Path to load the templates workbook.}

\item{salva}{Indicates if auto-saves the workbook.}
}
\description{
This function makes the color code for the quality measure of the estimations using the coefficients of variation.
}
\details{
The function prints the painted estimations on the final templates. At the same time saves the workbook with
the new data. It's important to keep in mind that if the parameter salva=T the workbook is auto-saved but for many iterations
this condition could make slower the full process.
}

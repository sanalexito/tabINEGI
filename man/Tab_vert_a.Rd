% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dog_functions.R
\name{Tab_vert_a}
\alias{Tab_vert_a}
\title{Tab_vert_a}
\usage{
Tab_vert_a(xx, etiquetas)
}
\arguments{
\item{xx}{Vector containing the names of the variables that will be used in the calculation. Usually: xx  = c("TOT", paste0("TOT_", 1:n)).}

\item{etiquetas}{Labels for the data frame.}
}
\description{
This function allows you to obtain a list with tabulations corresponding to estimations, coefficients of variation, confidence intervals and standard errors. This data frame is developed dividing between the first entry. It requires a data set with the appropriate expansion factor and the sampling design of the "survey" library.
}
\details{
To use this function the number of labels have to be the same that the totals plus one.
}
\examples{

Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))


}
\keyword{estimator,}
\keyword{survey,}
\keyword{vertical}

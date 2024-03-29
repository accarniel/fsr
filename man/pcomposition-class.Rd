% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_types.R
\docType{class}
\name{pcomposition-class}
\alias{pcomposition-class}
\title{An S4 Class for representing plateau compositions (subclass of \code{pgeometry})}
\description{
An S4 Class for representing plateau compositions (subclass of \code{pgeometry})
}
\details{
A \code{pcomposition} object is composed of a \code{ppoint} object, \code{pline} object, \code{pregion} object and inherits
the attribute \code{supp} from the class \code{pgeometry} (i.e., the support).
}
\section{Slots}{

\describe{
\item{\code{supp}}{It is inherited from \code{pgeometry}.}

\item{\code{ppoint}}{A plateau point object.}

\item{\code{pline}}{A plateau line object.}

\item{\code{pregion}}{A plateau region object.}
}}

\references{
\href{https://ieeexplore.ieee.org/document/9177620}{Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.}
}

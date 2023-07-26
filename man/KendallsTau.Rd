\name{KendallsTau}
\alias{KendallsTau}
\title{
Statistical correlation by Kendall
}
\description{
Calculates the statistical correlation by Kendall. Basically a wrapper to pcaPP::cor.fk
}
\usage{
KendallsTau(InputDists, OutputDists)
}
\arguments{
  \item{InputDists}{
Matrix containing the distances of the first dataset.
}
  \item{OutputDists}{
Matrix containing the distances of the second dataset.
}
}
\value{
Equivalent to \code{\link[pcaPP]{cor.fk}}
}
\author{
Michael Thrun
}
\name{ZrehenMeasure4All}
\alias{ZrehenMeasure4All}
\title{ A generalized version of the zrehen-measure which defines the neighbourhood by gabrielgraph and    is therefore not restricted to grid-based projections.}
\arguments{
\item{weights[1:n,1:m]}{n points in input room with m attributes}
\item{points[1:n,1:3]}{n projected points in output room, with index,x,y or index,line,column}
\item{width}{only necessary if toroid}
\item{height}{only necessary if toroid}
\item{toroid}{is the grid a toroid?}
\item{plotGabriel}{plot the generated GabrielGraph}
}
\value{List with
\item{V$zrehen}{the raw zrehen measure}
\item{V$normedzrehen}{the zrehen measure normed by the number of neighbours}
\item{v$neighbourcounter}{the number of possible neighbours by which the zrehen measure is normed}
}
\description{
 A generalized version of the zrehen-measure which defines the neighbourhood by gabrielgraph and    is therefore not restricted to grid-based projections.}
\author{Florian Lerch 07/2015}

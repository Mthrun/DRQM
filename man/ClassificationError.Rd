\name{ClassificationError}
\alias{ClassificationError}
\title{Classification Error (rate)}
\description{
Compares projected points to a given prior classification.
}
\usage{
ClassificationError(OutputDistances,Cls,k=5)
}
\arguments{
  \item{OutputDistances}{
[1:n,1:n]
}
  \item{Cls}{
[1:n]
}
  \item{k}{
number of k nearest neighbors, in Venna 2010 set to 5 (here default)
}
}
\details{
Projected points are evaluated by k-nearest neighbor classification accuracy (with k = 5), that is, each sample in the
visualization is classified by majority vote of its k nearest neighbors in the visualization, and the
classification is compared to the ground truth label. [Venna 2010].
}
\value{

\item{Error}{Classification Error: 1-Accuracy[1]}
\item{Accuracy}{Accuracy }
\item{NewCls}{[1:n]]}

}
\references{
Venna, J., Peltonen, J., Nybo, K., Aidos, H., and Kaski, S. Information retrieval perspective to nonlinear dimensionality reduction for data visualization. The Journal of Machine Learning Research, 11, 451-490.  (2010)

Gracia, A., Gonzalez, S., Robles, V., and Menasalvas, E. A methodology to compare Dimensionality Reduction algorithms in terms of loss of quality. Information Sciences, 270, 1-27.  (2014)
}
\author{
Michael Thrun
}
\note{
Here, the Outputdistances of the Projected points are used.
}

\keyword{Classification Error}% use one of  RShowDoc("KEYWORDS")
\keyword{ClassificationError}% __ONLY ONE__ keyword per line
\keyword{ClassificationErrorRate}% __ONLY ONE__ keyword per line

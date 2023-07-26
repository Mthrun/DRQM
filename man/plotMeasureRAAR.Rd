\name{plotMeasureRAAR}
\alias{plotMeasureRAAR}
\title{Computes rank-based smoothed precision and recall}
\description{Compares the projection in pData with the original data in Data and
calculates trustworthiness and continuity of the projection for neighborhood
sizes ranging from 1 to NeighborhoodSize}
\usage{plotMeasureRAAR(Raar, label = 'ProjectionMethod',
gPlotList = list(RAARplot = ggplot2::ggplot()), LineType="solid", Shape = 16,
PointsPerE = 10, fancy = FALSE)}
\arguments{
\item{Raar}{Output of RAAR() applied for a projection method.}
\item{label}{Title of plot.}
\item{gPlotList}{Settings for ggplot.}
\item{LineType}{Character - graphic parameter: Line type of ggplot.}
\item{Shape}{Integer: type of point}
\item{PointsPerE}{Numeric graphic parameter: Distance between markers on plot
line}
\item{fancy}{Boolean graphic parameter: Some automatic settings for a more
appealing plot.}
}
\value{ggplot object}
\author{
Michael Thrun
}

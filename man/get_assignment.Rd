\name{get_assignment}
\alias{get_assignment}
\title{Assignment Data Points to Clusters}
\description{Get the assignment of data points to clusters in a \code{DSC}
(nearest-neighbor).}

\usage{
get_assignment(dsc, points, type=c("auto", "micro", "macro"), 
  method="Euclidean", ...)
}

\arguments{
	\item{dsc}{The DSC object with the clusters for assignment.}
	\item{points}{The points to be assigned as a data.frame.}
	\item{type}{Use micro- or macro-clusters in DSC for assignment. Auto
	used the class of dsc to decide.}
	\item{method}{Distance measure used for nearest-neighbor assignment 
  (see \code{dist} in \pkg{proxy} for available measures).}
	\item{...}{Additional arguments are passed on.}
}

\details{
Each data point is assigned to its nearest cluster using the specified
distance measure.
}


\value{
A vector containing the assignment of each point.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSD}}
}

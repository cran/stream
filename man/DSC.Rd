\name{DSC}
\alias{DSC}
\title{Data Stream Clusterer Base Class}
\description{Abstract base class for all DSC (Data Stream
Clusterer) classes.}

\details{
The \code{DSC} class cannot be instantiated, 
but it serves as a base class from which all DSC classes 
inherit. 

Class \code{DSC} provides several generic functions that can 
operate on all DSC subclasses: 
\code{print()}, \code{plot()}, \code{nclusters()} to
get the current number of clusters, 
\code{get_centers()} to get the cluster centers,
and \code{get_weights()} to get the cluster weights 
(if implemented). \code{get_centers()} and \code{get_weights()}
are typically overwritten by subclasses of \code{DSC}.
}

\seealso{
	\code{\link[stream]{plot}},
	\code{\link{nclusters}},
	\code{\link{get_centers}},
	\code{\link{get_weights}},
	\code{\link{get_assignment}},
	\code{\link{get_copy}}
}

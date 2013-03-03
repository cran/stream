\name{DSC_Hierarchical}
\alias{DSC_Hierarchical}
\title{Hierarchical Micro-Cluster Reclusterer}
\description{
    Implementation of hierarchical clustering to recluster a set of micro-clusters.
    }

\usage{
	DSC_Hierarchical(k, method = "complete", members = NULL)
}

\arguments{
	\item{k}{The number of clusters returned by the \code{DSC}.}
	\item{method}{the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward", "single", "complete", "average", "mcquitty", "median" or "centroid".}
	\item{members}{\code{NULL} or a vector with length size of d.}
}

\details{
Please refer to package \code{Hierarchical} for more details on the behavior of the algorithm.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, \code{DSC_Macro}, and \code{DSC_Hierarchical}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{RObj}{The underlying R object.}
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}}
}

\examples{
# Cassini dataset
dsd <- DSD_mlbenchGenerator("cassini")

hierarchical <- DSC_Hierarchical(3, "single")
cluster(hierarchical, dsd, 500)
plot(hierarchical, dsd)

# Hierarchical can also recluster micro-clusters
dsc <- DSC_DenStream()
cluster(dsc, dsd, 500)

recluster(hierarchical,dsc)
plot(hierarchical, dsd)

}

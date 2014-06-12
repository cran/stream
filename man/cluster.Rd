\name{cluster}
\alias{cluster}
\title{Cluster a Data Stream}

\description{
Clusters a number of input points from a data stream into a clustering object.
}

\usage{
cluster(dsc, dsd, n = 1, verbose=FALSE, ...)
}

\arguments{
  \item{dsc}{a DSC object (clusterer).}
  \item{dsd}{a DSD object (data stream).}
  \item{n}{number of points to cluster.}
  \item{verbose}{report progress.}
  \item{...}{extra arguments for clusterer.}
}

\details{
\code{cluster} takes n times a single 
data points out of the DSD and adds it to the 
clustering in the DSC object. Note that cluster directly 
modifies the DSC object (which is a reference class) and 
thus the result does not need to be reassigned to the
DSC.
}

\value{
The updated DSC object is returned invisibly for reassignment
(however, this is not necessary).  To obtain the updated
clustering result, call \code{get_centers()} on the DSC object.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSD}}, and
	\code{\link{animation}} for producing an animation of the clustering process.
}

\examples{
dsd <- DSD_Gaussians(k=3)
dsc <- DSC_DStream(gridsize=.05)  

cluster(dsc, dsd, 500)
plot(dsc, dsd)
}

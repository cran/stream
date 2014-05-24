\name{DSC_Reachability}
\alias{DSC_Reachability}
\title{Reachability Micro-Cluster Reclusterer}
\description{
    Implementation of reachability clustering to recluster a set of micro-clusters.
    Two micro-clusters are rechable and thus clustered together 
    if they are within each others epsilon-neighborhood. 
    Epsilon is typically chosen as the radius of the assignment
    area of the micro-clusters.
}

\usage{
	DSC_Reachability(epsilon, min_weight=NULL)
}

\arguments{
	\item{epsilon}{radius of the epsilon-neighborhood.}
  \item{min_weight}{micro-clusters with a weight less than this will be 
    ignored for reclustering.}
}

%\details{
%}

\value{
An object of class \code{DSC_Reachability}. The object
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

# using reachability cluatering directly on data points
reach <- DSC_Reachability(epsilon=0.1)
cluster(reach, dsd, 500)
reach
plot(reach, dsd)

# Recluster micro-clusters from tNN with reachability (epsilon=r)
tnn <- DSC_tNN(r=0.1, noise=0, lambda=0)
cluster(tnn, dsd, 500)

reach <- DSC_Reachability(epsilon=0.1)
recluster(reach, tnn)
  
plot(reach, dsd, type="macro")  
}

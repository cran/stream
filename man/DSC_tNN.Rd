\name{DSC_tNN}
\alias{DSC_tNN}
\alias{tnn}
\alias{tNN}
\alias{get_shared_density}
\title{Threshold Nearest Neighbor (tNN) Data Stream Clustering Algorithm}
\description{
    Implements the tNN (threshold Nearest Neighbor) data stream clustering algorithm.}

\usage{
DSC_tNN(r, lambda = 0.001, gap_time = 1000L, 
  noise = 0.1, measure = "Euclidean", 
	shared_density = FALSE, alpha=0.1, k=0, minweight = 0)
get_shared_density(x, matrix=FALSE)  
}

\arguments{
	\item{r}{The threshold in the nearest neighborhood algorithm.}
	\item{lambda}{The lambda used in the fading function.}
	\item{gap_time}{weak micro-clusters (and weak shared density entries) 
    are removed every \code{gap_time} points.}
	\item{noise}{The amount of noise that should be removed while clustering.}
	\item{measure}{The measure used to calculate cluster proximity 
	(see package \pkg{proxy}).}
  \item{shared_density}{Record shared density information. If set to \code{TRUE}
    then shared density is used for reclustering, otherwise reachability is
    used (overlapping clusters with less than \eqn{r*(1-alpha)} distance are clustered together). }
	\item{k}{The number of macro clusters to be returned if macro is true.}
  \item{alpha}{For shared density: The minimum proportion of shared points between to 
    clusters to warrant combining them (a suitable value for 2D data is .3). 
    For reachability clustering it is a distance factor.}
  \item{minweight}{The minimum number of weight a micro-cluster
    needs to have.}
  \item{x}{A DSC_tNN object to get the shared density information from.}
  \item{matrix}{get shared density as a matrix.}
}

\details{
The threshold Nearest Neighbor algorithm checks for each new data point in the
incoming stream, if it is below the threshold value of dissimilarity value of
any existing micro-clusters, and if so, merges the point with the 
micro-cluster. Otherwise, a new micro-cluster is created to accommodate 
the new data point.

Note: Although DSC_tNN is a micro clustering algorithm, macro clusters
and weights are available.

\code{plot()} for DSC_tNN has two extra logical parameters called \code{assignment}
  and \code{shared_density} which show the assignment area and
  the shared density graph, rexpectively.
}

\value{
An object of class \code{DSC_tNN} (subclass of  \code{DSC}, \code{DSC_R}, \code{DSC_Micro}). 
}

\references{ 
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In: ICDM '04:
Proceedings of the Fourth IEEE International Conference on Data Mining, pp. 371-374.

M. Hahsler, M. H. Dunham (2010): rEMM: Extensible Markov Model for Data
Stream Clustering in R, Journal of Statistical Software, 35(5), 1-31, URL \url{http://www.jstatsoft.org/v35/i05/}
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}}
}

\examples{
set.seed(0)
dsd <- DSD_Gaussians(k=3, noise=0.05)

# tNN with reachability (increase noise parameter to reduce the micro-clusters
# at the fringes of the Gaussians)
tnn <- DSC_tNN(r=.1, noise=0.1)
cluster(tnn, dsd, 500)
tnn

# check micro-clusters
nclusters(tnn)
head(get_centers(tnn))
plot(tnn, dsd)

# plot macro-clusters
plot(tnn, dsd, type="both")

# plot micro-clusters with assignment area
plot(tnn, dsd, type="micro", assignment=TRUE)


# tNN with shared density 
tnn <- DSC_tNN(r=.1, noise=0.1, shared_density=TRUE)
cluster(tnn, dsd, 500)
tnn
plot(tnn, dsd, type="both")

# plot the shared density graph
plot(tnn, dsd, type="micro", shared_density=TRUE)
plot(tnn, dsd, type="micro", shared_density=TRUE, assignment=TRUE)
plot(tnn, dsd, type="none", shared_density=TRUE, assignment=TRUE)

# see how micro and macro-clusters relate
# each microcluster has an entry with the macro-cluster id
# Note: unassigned micro-clusters (noise) have an NA
microToMacro(tnn)

# evaluate first using macro and then using micro-clusters
\dontrun{
evaluate(tnn, dsd, measure="purity")
evaluate(tnn, dsd, measure="cRand", type="macro")
}
}

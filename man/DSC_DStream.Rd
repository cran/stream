\name{DSC_DStream}
\alias{DSC_DStream}
\alias{dstream}
\alias{d-stream}
\alias{D-Stream}
\alias{get_attraction}
\title{D-Stream Data Stream Clustering Algorithm}
\description{
    Implements the D-Stream data stream clustering algorithm.}

\usage{
DSC_DStream(gridsize, d=NA_integer_, lambda = 1e-3, 
   gaptime=1000L, Cm=3, Cl=.8, attraction=FALSE, epsilon=.3, Cm2=Cm, k=NULL)
get_attraction(x, dist=FALSE, relative=FALSE)
}

\arguments{
	\item{gridsize}{Size of grid cells. Vector of length 
		\code{d} (dimensionality of the data). If only
		one value is specified then the same size is used for 
		all dimensions.}
	\item{d}{dimensionality of the data. If unspecified then it is taken from the 
    first clustered data point.}
	\item{lambda}{The lambda used in the fading function (Note: in the paper the 
    authors use denote the decay factor as lambda, while here the decay factor
    will be computed from lambda using \eqn{2^-lambda}).}
	\item{gaptime}{sporadic grids are removed every
    gaptime points.}
	\item{Cm}{threshold used to detect dense grids (as a proportion of the 
	    average expected density).}
	\item{Cl}{threshold to detect transitional and sporadic grids (0 > Cl > 1).}
  \item{attraction}{compute and store information about the attraction between
    adjacent grids. If \code{TRUE} then attraction is used to create macro-clusters,
  otherwise macro-clusters are created by merging adjacent dense grids.}
  \item{epsilon}{overlap parameter for attraction as a proportion of 
    \code{gridsize}.}
  \item{Cm2}{threshold on attraction to join two dense grid cells (as a 
    proportion on the average expected attraction).}
  \item{k}{alternative to Cm2 (not in the original algorithm). 
    Create k clusters based on attraction. In case 
    of more than k unconnected components, closer groups of MCs are joined.}
  \item{x}{DSC_DStream object to get attraction values from.}
  \item{dist}{make attraction symmetric and transform into a distance.}
  \item{relative}{calculates relative attraction (normalized by the cluster 
    weight).}
}

\details{
D-Stream creates an equally spaced grid and estimates the density in each 
  grid cell using the count of points falling in the cells. 
  The density is faded after every new point by a factor of \eqn{2^-lambda}.
  Every gaptime number of points sporadic grid cells are removed.
  
  For reclustering D-Stream (2007 version) creates grid groups of reachable 
  dense grid cells.
  This can be implemented using \code{DSC_hierarchical} with single-linkage
  and h of just above gridsize (see Examples section below).
  
  The 2009 version of the algorithm adds the concept of attraction between 
  grids cells. If \code{attraction=TRUE} is used then the algorithm also
  produces macro-clusters and weights.
}

\value{
An object of class \code{DSC_DStream} (subclass of  \code{DSC}, \code{DSC_R}, \code{DSC_Micro}). 
}

\references{ 
Yixin Chen and Li Tu. 2007. Density-based clustering for real-time stream data. In Proceedings of the 13th ACM SIGKDD international conference on Knowledge discovery and data mining (KDD '07). ACM, New York, NY, USA, 133-142.

Li Tu and Yixin Chen. 2009. Stream data clustering based on grid density and attraction. ACM Trans. Knowl. Discov. Data 3, 3, Article 12 (July 2009), 27 pages.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}}
}

\examples{
set.seed(0)
dsd <- DSD_Gaussians(k=3, noise=0.05)

dstream <- DSC_DStream(gridsize=.05)
cluster(dstream, dsd, 500)
dstream

# see micro-clusters
nclusters(dstream)
head(get_centers(dstream))

# plot (DStream provides additional type="grid")
plot(dstream, dsd)
plot(dstream, type="grid")
plot(dstream, dsd, type="grid")

# standard D-Stream uses reachability
nclusters(dstream, type="macro")
get_centers(dstream, type="macro")
plot(dstream, type="both")

# use image and add centers
plot(dstream, type="grid")
points(get_centers(dstream, type="macro"), col="blue", pch=3, lwd=2, 
  cex=get_weights(dstream, type="macro", scale=c(1,5)))

# use attraction for reclustering
set.seed(0)
dsd <- DSD_Gaussians(k=3, noise=0.05)

dstream <- DSC_DStream(gridsize=.05, attraction=TRUE, epsilon=.15)
cluster(dstream, dsd, 500)
dstream

plot(dstream, type="both", dsd)
}
\name{DSC_DBSTREAM}
\alias{DSC_DBSTREAM}
\alias{DBSTREAM}
\alias{dbstream}
\alias{get_shared_density}
\alias{change_alpha}
\title{DBSTREAM clustering algorihtm}
\description{
    Implements a simple density-based stream clustering algorithm that assigns data points
  to micro-clusters with a given radius and implements shared-density-based reclustering.}

\usage{
DSC_DBSTREAM(r, lambda = 0.001, gaptime = 1000L, 
  noise = 0.1, shared_density = FALSE, alpha=0.1, k=0, minweight = 0)
get_shared_density(x, use_alpha = TRUE)  
change_alpha(x, alpha)  
}

\arguments{
	\item{r}{The radius of micro-clusters.}
	\item{lambda}{The lambda used in the fading function.}
	\item{gaptime}{weak micro-clusters (and weak shared density entries) 
    are removed every \code{gaptime} points.}
	\item{noise}{The proportion of noise points in the data that will 
	  be removed while clustering.}
  \item{shared_density}{Record shared density information. If set to \code{TRUE}
    then shared density is used for reclustering, otherwise reachability is
    used (overlapping clusters with less than \eqn{r*(1-alpha)} distance 
    are clustered together). }
	\item{k}{The number of macro clusters to be returned if macro is true.}
  \item{alpha}{For shared density: The minimum proportion of shared 
    points between to 
    clusters to warrant combining them (a suitable value for 2D data is .3). 
    For reachability clustering it is a distance factor.}
  \item{minweight}{The minimum number of weight a micro-cluster
    needs to have.}
  \item{x}{A DSC_DBSTREAM object to get the shared density information from.}
  \item{use_alpha}{only return shared density if it exceeds alpha.}
}

\details{
The DBSTREAM algorithm checks for each new data point in the
incoming stream, if it is below the threshold value of dissimilarity value of
any existing micro-clusters, and if so, merges the point with the 
micro-cluster. Otherwise, a new micro-cluster is created to accommodate 
the new data point.

Although DSC_DBSTREAM is a micro clustering algorithm, macro clusters
and weights are available.

\code{plot()} for DSC_DBSTREAM has two extra logical parameters called \code{assignment}
  and \code{shared_density} which show the assignment area and
  the shared density graph, rexpectively.

  Note that \code{DSC_DBSTREAM} can at this point not be saved to disk using
  save() or saveRDS(). This functionality will be added later!

}

\value{
An object of class \code{DSC_DBSTREAM} 
  (subclass of  \code{DSC}, \code{DSC_R}, \code{DSC_Micro}). 
}

%\references{ 
%M. Hahsler, M. H. Dunham (2010): rEMM: Extensible Markov Model for Data
%Stream Clustering in R, Journal of Statistical Software, 35(5), 1-31, URL \url{http://www%.jstatsoft.org/v35/i05/}
%}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}}
}

\examples{
set.seed(0)
stream <- DSD_Gaussians(k = 3, noise = 0.05)

# create clusterer with r = .1
dbstream <- DSC_DBSTREAM(r = .1, noise = 0.05)
update(dbstream, stream, 500)
dbstream 

# check micro-clusters
nclusters(dbstream)
head(get_centers(dbstream))
plot(dbstream, stream)

# plot macro-clusters
plot(dbstream, stream, type = "both")

# plot micro-clusters with assignment area
plot(dbstream, stream, type = "both", assignment = TRUE)


# DBSTREAM with shared density 
dbstream <- DSC_DBSTREAM(r = .1, noise = 0.05, shared_density = TRUE)
update(dbstream, stream, 500)
dbstream
plot(dbstream, stream, type = "both")
# plot the shared density graph
plot(dbstream, stream, type = "both", shared_density = TRUE)

# change alpha
change_alpha(dbstream, .3)
plot(dbstream, stream, type = "both", shared_density = TRUE)
plot(dbstream, stream, type = "micro", shared_density = TRUE)
plot(dbstream, stream, type = "micro", shared_density = TRUE, assignment = TRUE)
plot(dbstream, stream, type = "none", shared_density = TRUE, assignment = TRUE)



# see how micro and macro-clusters relate
# each microcluster has an entry with the macro-cluster id
# Note: unassigned micro-clusters (noise) have an NA
microToMacro(dbstream)

# evaluate first using macro and then using micro-clusters
\dontrun{
evaluate(dbstream, stream, measure="purity")
evaluate(dbstream, stream, measure="cRand", type="macro")
}
}

\name{DSC_CluStream}
\alias{DSC_CluStream}
\alias{CluStream}
\alias{clustream}
\title{CluStream Data Stream Clusterer}
\description{
 Class implements the CluStream cluster algorithm for data streams.    
    }

\usage{
DSC_CluStream(horizon = 1000, k = 100, t = 2)
}

\arguments{
  \item{horizon}{Defines the time window to be used in CluStream}
  \item{k}{Defines the maximum number of micro-clusters used in CluStream}
  \item{t}{Maximal boundary factor (=Kernel radius factor). 
  When deciding to add a new data point to a
  micro-cluster, the maximum boundary is defined as a factor of \code{t} of the
  RMS deviation of the data points in the micro-cluster from the centroid. 
  }
}

\details{
This is an interface to the MOA implementation of CluStream.
}

\value{
An object of class \code{DSC_CluStream} (subclass of 
\code{DSC_Micro}, \code{DSC_MOA} and \code{DSC}). 
}

\references{
Aggarwal CC, Han J, Wang J, Yu PS (2003). "A Framework for Clustering 
Evolving Data Streams." In "Proceedings of the International 
Conference on Very Large Data Bases (VLDB '03)," pp. 81-92.

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T (2010). 
MOA: Massive Online Analysis, a Framework for Stream Classification and 
Clustering. In Journal of Machine Learning Research (JMLR).
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}},
	\code{\link{DSC_MOA}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

dsc <- DSC_CluStream()
cluster(dsc, dsd, 1000)
dsc

# plot micro-clusters
plot(dsc, dsd)

# reclustering
kmeans <- DSC_KmeansW(3)
recluster(kmeans,dsc)
plot(kmeans, dsd)
}

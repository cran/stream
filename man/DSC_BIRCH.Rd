\name{DSC_BIRCH}
\alias{DSC_BIRCH}
\alias{BIRCH}
\alias{birch}
\title{BIRCH Data Stream Clusterer}
\description{Class implements the BIRCH cluster algorithm 
for data streams.}

\usage{
	DSC_BIRCH(radius, compact = radius, keeptree = TRUE)
}

\arguments{
	\item{radius}{The closeness criterion.}
	\item{compact}{The The compactness criterion.}
	\item{keeptree}{A Boolean, whether to keep the CF tree in memory.}
}

\details{
Please refer to package \code{BIRCH} for more details. It is treated as a micro
clusterer within the \code{stream} package.
}

\value{
An object of class  \code{DSC_BIRCH} (subclass of \code{DSC}, \code{DSC_R}, \code{DSC_Micro})
}

\references{
Tian Zhang, Raghu Ramakrishnan, Miron Livny, An Efficient Data Clustering Method for Very Large Databases, In SIGMOD '96: Proceedings of the 1996 ACM SIGMOD international conference on Management of data (1996), pp. 103-114
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

dsc <- DSC_BIRCH(radius = .05)
cluster(dsc, dsd, 1000)
dsc

# plot micro-clusters
plot(dsc, dsd)

# reclustering
kmeans <- DSC_Kmeans(k=3)
recluster(kmeans,dsc)
plot(kmeans,dsd)
}

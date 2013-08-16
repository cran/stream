\name{DSC_ClusTree}
\alias{DSC_ClusTree}
\alias{ClusTree}
\alias{clustree}
\title{ClusTree Data Stream Clusterer}

\description{
Class implements the ClusTree cluster algorithm for data streams.
}

\usage{
	DSC_ClusTree(horizon = 1000, maxHeight = 8, lambda = NULL)
}

\arguments{
	\item{horizon}{Range of the (time) window.}
	\item{maxHeight}{The maximum height of the tree.}
 	\item{lambda}{number used to override computed lambda (decay).}
}

\details{
This is an interface to the MOA implementation of ClusTree.
}

\value{
An object of class \code{DSC_ClusTree} (subclass of \code{DSC}, \code{DSC_MOA}, \code{DSC_Micro}).
}
\references{
Philipp Kranen, Ira Assent, Corinna Baldauf, and Thomas Seidl. 2009.
Self-Adaptive Anytime Stream Clustering. In Proceedings of the 2009 Ninth
IEEE International Conference on Data Mining (ICDM '09). IEEE Computer
Society, Washington, DC, USA, 249-258. DOI=10.1109/ICDM.2009.47
\url{http://dx.doi.org/10.1109/ICDM.2009.47}

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T,
Seidl T (2010). MOA: Massive Online Analysis, a Framework for
Stream Classification and Clustering. In Journal of Machine
 Learning Research (JMLR).

}
\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}},
	\code{\link{DSC_MOA}}
}
\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0)

dsc <- DSC_ClusTree(maxHeight=3)
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(dsc, dsd)

# reclustering
kmeans <- DSC_Kmeans(k=3)
recluster(kmeans,dsc)
plot(kmeans, dsd)
}

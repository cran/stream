\name{DSC_Sample}
\alias{DSC_Sample}
\title{Extract a Fixed-size Sample from a Data Stream}
\description{
    Extracts a sample form a data stream using Reservoir Sampling. 
    The sample is stored
    as a set of micro-clusters to be compatible with other
    data DSC stream clustering algorithms.
}

\usage{
	DSC_Sample(k = 100)
}

\arguments{
	\item{k}{the number of points to be sampled from the stream.}
}

%\details{
%}

\value{
An object of class \code{DSC_Sample} (subclass of
\code{DSC}, \code{DSC_R}, \code{DSC_Micro}).
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}}
}

\references{
Vitter, J. S. (1985): Random sampling with a reservoir. ACM Transactions
on Mathematical Software, 11(1), 37-57.
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_Gaussians(k=3, noise=0.05)

dsc <- DSC_Sample(20)
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(dsc, dsd)

# reclustering (Kmeans is sufficient because DSC_Sample does not return weights)
kmeans <- DSC_Kmeans(3)
recluster(kmeans,dsc)
plot(kmeans,dsd)
}

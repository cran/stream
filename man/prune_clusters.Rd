\name{prune_clusters}
\alias{prune_clusters}
\title{Prune Clusters from a Clustering}

\description{
Creates a (static) copy of a clustering where 
a fraction of the weight or the number of 
clusters with the lowest weights were pruned.
}

\usage{
	prune_clusters(dsc, threshold= 0.05, weight = TRUE)
}

\arguments{
	\item{dsc}{The DSC object to be pruned.}
	\item{threshold}{The numeric vector of probabilities for the quantile.}
	\item{weight}{should a fraction of the total weight in the clustering
	be pruned? Otherwise a fraction of clusters is pruned.}
}

%\details{
%}

\value{
Returns an object of class
\code{DSC_Static}.
}

\seealso{
	\code{\link{DSC_Static}}
}

\examples{
# 3 clusters with 10% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.1)

birch <- DSC_BIRCH(r=0.01)
cluster(birch, dsd, 500)
birch
plot(birch, dsd)

# prune 10% of the cluster weight
static <- prune_clusters(birch, threshold=0.1)
static
plot(static, dsd)
}

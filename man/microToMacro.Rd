\name{microToMacro}
\alias{microToMacro}
\title{
Translate Micro-cluster IDs to Macro-cluster IDs
}
\description{
Translates micro-cluster ids into macro-cluster ids for 
a \code{DSC_Macro} object.
}
\usage{
microToMacro(x, micro=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{DSC_Macro} object that also contains information about 
  micro-clusters.}
  \item{micro}{A vector with micro-cluster ids. If \code{NULL} then
  the assignments for all micro-clusters in x are returned. }
}
%\details{
%}
\value{
A vector of the same length as \code{micro} with the macro-cluster ids.
}
\seealso{
\code{\link{DSC_Macro}}
}
\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

# recluster a micro-clusters
micro <- DSC_CluStream()
cluster(micro, dsd, 500)

macro <- DSC_Kmeans(k=3)
recluster(macro, micro)

# translate all micro-cluster ids
ids <- 1:nclusters(macro, type="micro")
microToMacro(macro, ids)

# plot some data points in grey
plot(dsd, col="grey", cex=.5)
# add micro-clusters and use the macro-cluster ids as color
points(get_centers(macro, type="micro"), col=microToMacro(macro, ids))
# add macro-cluster centers
points(get_centers(macro, type="macro"), pch=3, cex=4, ,lwd=2, col=1:3)
}

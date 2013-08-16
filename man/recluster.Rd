\name{recluster}
\alias{recluster}
\title{Re-clustering micro-clusters}

\description{Use a macro clustering algorithm to recluster micro-clusters into a final clustering.
}

\usage{
recluster(macro, dsc, type="auto", ...)
}

\arguments{
  \item{macro}{a macro clustering algorithm (class "DSC_Macro")}
  \item{dsc}{a DSC object containing micro-clusters. }
  \item{type}{controls which clustering is used from \code{dsc} 
    (typically micro-clusters).}
  \item{...}{additional arguments passed on.}
}

\details{
Takes centers and weights of the micro-clusters and applies the macro clustering algorithm.
}

\value{
The object macro is altered and contains the clustering.
}


%\seealso{
%}

\examples{
### create a data stream and a micro-clustering
dsd <- DSD_GaussianStatic(k=3, d=3)

micro <- DSC_DenStream(initPoints=100)
cluster(micro, dsd, 500)

### recluster using weighted k-means
macro <- DSC_Kmeans(k=3)
recluster(macro, micro)

### plot clustering
plot(macro,dsd, main="Macro-clusters (DenStream + weighted k-means)")
}

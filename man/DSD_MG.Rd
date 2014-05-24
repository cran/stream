\name{DSD_MG}
\title{DSD Moving Generator}
\alias{DSD_MG}
\alias{DSD_MovingGenerator}
\alias{MovingGenerator}
\alias{add_cluster}
\alias{get_clusters}
\alias{remove_cluster}

\description{
  Creates an evolving DSD that consists of several \code{MGC}s.
}

\usage{
DSD_MG(dimension = 2, ...)

add_cluster(x, c)
get_clusters(x) 
remove_cluster(x, i)
}

\arguments{
  \item{dimension}{the dimension of the DSD object}
  \item{...}{initial set of \code{MGC}s}
  \item{x}{A \code{DSD_MG} object.}
  \item{c}{The cluster that should be added to the \code{DSD_MG} object.}
  \item{i}{The index of the cluster that should be removed from the \code{DSD_MG} object.}
}

\details{
  This DSD is able to generate complex datasets that are able to evolve over a period of time.
  Its behavior is determined by the \code{MGC}s it is composed of. 
}

\seealso{
  \code{\link{MGC_Function}}, \code{\link{MGC_Linear}}, \code{\link{MGC_Noise}}, \code{\link{MGC_Random}} for details on the different \code{MGC} objects.
}


\examples{
### create an empty DSD_MG
dsd <- DSD_MG(dim = 2)
dsd  
  
### add two clusters
c1 <- MGC_Random(density=50, center=c(50,50), parameter=1)
add_cluster(dsd, c1)

c2 <- MGC_Noise(density=1, range=rbind(c(-20,120), c(-20,120)))
add_cluster(dsd, c2)
    
get_clusters(dsd)
plot(dsd, xlim=c(-20,120), ylim=c(-20,120))
    
\dontrun{
animate_data(dsd, n=5000, xlim=c(-20,120), ylim=c(-20,120))
}
  
  
### remove cluster 1
remove_cluster(dsd,1)
    
get_clusters(dsd)
plot(dsd, xlim=c(-20,120), ylim=c(-20,120))
    
### simulate the clustering of a splitting cluster  
c1 <- MGC_Linear(dim = 2, keyframelist = list(
  keyframe(time = 1,  dens = 10, center = c(0,0),   param = 10, cluster=1),
  keyframe(time = 50, dens = 10, center = c(50,50), param = 10, cluster=1),
  keyframe(time = 100,dens = 10, center = c(50,100),param = 10, cluster=1)
))
  
c2 <- MGC_Linear(dim = 2, keyframelist = list(
  keyframe(time = 1,  dens = 10, center = c(0,0),   param = 10, cluster=1),
  keyframe(time = 50, dens = 10, center = c(50,50), param = 10, cluster=2),
  keyframe(time = 100,dens = 10, center = c(100,50),param = 10, cluster=2)
))

dsd <- DSD_MG(dim = 2, c1, c2)
dsd
  
tnn <- DSC_tNN(r=10, lambda=0.1)
  
\dontrun{
tnnPurity <- animate_cluster(tnn, dsd, n=2500, type="both", 
  xlim=c(-10,120), ylim=c(-10,120), evaluationMethod="purity", pointInterval=50)
}
}
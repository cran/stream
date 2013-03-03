\name{DSD_GaussianMoving}
\alias{DSD_GaussianMoving}
\title{Moving Gaussians Data Stream Generator}
\description{
A data stream generator that produces a data stream with moving Gaussians . It
has two clusters and a third that moves between them.
}

\usage{
DSD_GaussianMoving(t=10, n=20)
}

\arguments{
  \item{t}{The number of turns or steps taken for the moving cluster to move from the
  	bottom left hand corner to the top right hand corner.}
  \item{n}{The number of points used for each cluster during step or turn.}
}

\details{
\code{DSD_GaussianMoving} is a DSD generator
with 3 clusters in 2 dimensional space. 
Two of the clusters are static and are located at opposite
corners. The third cluster traverses the two. The number of steps it takes for
the third cluster to complete its journey is determined by \code{t}.
}

\value{
An object of class \code{DSD_GaussianMoving} (subclass of
\code{DSD_Wrapper}, \code{DSD_R}, \code{DSD}).
}
	   
\seealso{
	\code{\link{DSD}}
}

\examples{
# create data stream with three clusters in 2D
dsd <- DSD_GaussianMoving()

# show an animation of the data
\dontrun{
animate_data(dsd, 600,  xlim=c(-.2,1.2), ylim=c(-.2,1.2), 
    pointInterval=10)}
}

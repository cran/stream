\name{animation}
\alias{animate}
\alias{animation}
\alias{animate_data}
\alias{animate_cluster}
\title{Animates the plotting of a DSD and the clustering process}

\description{
Generates an animation of a data stream or a data steam clustering.
}

\usage{
animate_data(dsd, n=1000, wait=.1, horizon=100, ...)
animate_cluster(dsc, dsd, macro=NULL, n=1000, wait=.1, horizon=100, 
    evaluationMeasure=NULL, evaluationType="auto", evaluationAssign="micro", 
    evaluationAssignmentMethod = "auto", ...)
}

\arguments{
  \item{dsd}{a DSD object}
  \item{dsc}{a DSC object}
  \item{macro}{a \code{DSC_macro} object used for reclustering when performing evaluations.}
  \item{n}{the number of points to be plotted}
  \item{wait}{the time interval between each frame}
  \item{horizon}{the number of points displayed at once/used for evaluation. }
  \item{evaluationMeasure}{the evaluation measure that should be graphed below the animation}
  \item{evaluationType}{evaluate \code{"micro"} or \code{"macro"}-clusters? \code{"auto"}
      chooses micro if \code{dsc} is of class \code{DSC_micro} and no \code{macro}
      is given. Otherwise macro is used.}
  \item{evaluationAssign}{assign new points to the closest \code{"micro"} or 
    \code{"macro"}-cluster to calculate the evaluation measure.}
  \item{evaluationAssignmentMethod}{how to assign data points to micro-clusters. Options are
  \code{"model"} and \code{"nn"} (nearest neighbor). \code{"auto"} uses model if 
    available and nn otherwise.}
  \item{...}{extra arguments for plot.}
}

\details{
Animations are recorded using the library animation and can be replayed
  (which gives a smoother experience since the is no more computation done) and
  saved in various formats (see Examples section below).
}

\seealso{
  \code{\link{evaluate_cluster}} for stream evaluation without animation.
  See \code{\link[animation]{ani.replay}} for replaying and saving animations.
}


\examples{
\dontrun{
stream <- DSD_Benchmark(1)
animate_data(stream, n=5000,  xlim=c(0,1), ylim=c(0,1), horizon=100)

### animations can be replayed with the animation package
library(animation)
animation::ani.options(interval=.1) ## change speed
ani.replay()
  
### animations can also be saved as HTML, animated gifs, etc.
saveHTML(ani.replay())  
  
### animate the clustering process with evaluation
reset_stream(stream)
tnn <- DSC_tNN(r=.1, lambda=.01, shared_density=TRUE,
  alpha=.2, noise=.1)  

animate_cluster(tnn, stream, n=5000, horizon=100, 
  evaluationMeasure="crand", evaluationType="macro", evaluationAssign="micro",
  xlim=c(0,1),ylim=c(0,1), type="shared")
}
}

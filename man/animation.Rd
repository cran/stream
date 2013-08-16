\name{animation}
\alias{animate}
\alias{animate_data}
\alias{animate_cluster}
\title{Animates the plotting of a DSD and the clustering process}

\description{
Generates 
an animation of a data stream or a data steam clustering.
}

\usage{
animate_data(dsd, n=1000, interval=.1, 
    pointInterval=100, horizon=5*pointInterval, 
    outdir=NULL,...)
animate_cluster(dsc, dsd, macro=NULL, n=1000, interval=.1, 
    pointInterval=100, horizon=5*pointInterval, 
    evaluationMethod=NULL, outdir=NULL,...)
}

\arguments{
  \item{dsd}{a DSD object}
  \item{dsc}{a DSC object}
  \item{macro}{a \code{DSC_macro} object used for reclustering when performing evaluations.}
  \item{n}{the number of points to be plotted}
  \item{interval}{the time interval between each frame}
  \item{pointInterval}{the number of new points needed before a new frame is created}
  \item{horizon}{the max number of data points to be shown at a time}
  \item{evaluationMethod}{the evaluation method that should be graphed below the animation}
  \item{outdir}{the location of where the animation (animated gif) should be saved}
  \item{...}{extra arguments for plot.}
}

\details{
It generates several plots and strings them together to form an animation. This is particularly
useful to see the behavior of moving data sets. If no \code{outdir} is specified
the function will animate the progress while clustering.
}

\seealso{
  \code{\link{evaluate_cluster}} for stream evaluation without animation.
}


\examples{
dsd <- DSD_GaussianMoving()
animate_data(dsd, 500,  xlim=c(-.2,1.2), ylim=c(-.2,1.2), 
    pointInterval=20)

reset_stream(dsd)
dsc <- DSC_tNN(.1, alpha=.2, lambda=.03, decay_interval=1, minweight=.1)

\dontrun{
### Note: type is a parameter passed on to plot (for DSC)
reset_stream(dsd)
animate_cluster(dsc, dsd, n=500, interval=.01,
       pointInterval=10, xlim=c(-.5,1.5),ylim=c(-.5,1.5), 
       type="macro")

reset_stream(dsd)
animate_cluster(dsc, dsd, n=600, interval=.01,
       pointInterval=20, xlim=c(-.5,1.5),ylim=c(-.5,1.5), 
       type="macro", eval="crand")}
}

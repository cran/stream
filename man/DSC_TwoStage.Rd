\name{DSC_TwoStage}
\alias{DSC_TwoStage}
\title{TwoStage Clustering Process}
\description{Combines a micro and a macro clustering algorithm into a
single process.}

\usage{
DSC_TwoStage(micro, macro) 
}

\arguments{
	\item{micro}{Clustering algorithm for online stage (\code{DSC_micro})} 
	\item{macro}{Clustering algorithm for offline stage (\code{DSC_macro})} 
}

\details{
\code{cluster()} runs the micro-clustering stage and if centers/weights are 
requested the reclustering is automatically performed.
}

\value{
An object of class \code{DSC_TwoStage} (subclass of  \code{DSC}, \code{DSC_Macro}). 
}

%\references{ }

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}}
}

\examples{
dsd <- DSD_Gaussians(k=3)

# Create a clustering process that uses a window for the online stage and
# k-means for the offline stage (reclustering)
dsc <- DSC_TwoStage(
  micro=DSC_Window(horizon=100), 
  macro=DSC_Kmeans(k=3)
  ) 
dsc
  
cluster(dsc, dsd, 200) 
dsc
plot(dsc, dsd, type="both")  
evaluate(dsc, dsd, assign="macro")
}
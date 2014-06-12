\name{DSD_Wrapper}
\alias{DSD_Wrapper}
\title{A Data Stream Wrapper for Data.Frames or Matrix-like Objects}
\description{
This class wraps data.frame or matrix-like objects and
provides access to the data in a streaming fashion. 
The data can either be looped or replayed manually to give
the exact same data several times.
The Wrapper can also be used to record and replay a part of a data stream.
}

\usage{DSD_Wrapper(x, n, k=NA, loop=FALSE, assignment = NULL, description=NULL)}

\arguments{
	\item{x}{A stream object, a data frame or other matrix-like object
    with the data to be used in the stream.
	If \code{x} is a DSD object then a wrapper for \code{n} data points
	from this DSD is created. }
	\item{n}{Number of points used if \code{x} is a DSD object. If
	\code{x} is a data frame or matrix then \code{n} is ignored.}
	\item{k}{Optional: The number of clusters}
	\item{loop}{A flag that tells the stream to loop or 
				not to loop over the data frame.}
	\item{assignment}{Index of the column containing the group assignment (ground truth)
	or a vector with the assignment. }
	\item{description}{character string with a description. }
}

\details{
In addition to regular data.frames other matrix-like objects can be used.
This includes also \code{ffdf} (large data.frames 
stored on disk) from package \pkg{ff} 
and \code{big.matrix} from \pkg{bigmemory}.
}

\value{
Returns a \code{DSD_Wrapper} object 
(subclass of \code{DSD_R}, \code{DSD}).
}

\seealso{
	\code{\link{DSD}},
	\code{\link{reset_stream}}
}

\examples{
### wrap 1000 points from a dsd
dsd <- DSD_Gaussians(k=3, d=2)
replayer <- DSD_Wrapper(dsd, k=3, n=1000)
replayer
plot(replayer)  
  
# creating 2 clusterers of different algorithms
dsc1 <- DSC_tNN(r=0.1)
dsc2 <- DSC_DStream(gridsize=0.1)
  
# clustering the same data in 2 DSC objects
reset_stream(replayer) # resetting the replayer to the first position
cluster(dsc1, replayer, 500)
reset_stream(replayer)
cluster(dsc2, replayer, 500)
  
# plot the resulting clusterings
reset_stream(replayer) 
plot(dsc1, replayer, main="tNN")
reset_stream(replayer) 
plot(dsc2, replayer, main="D-Stream")   
  
### use a data.frame to create a stream (3rd col. contains the assignment)
df <- data.frame(x=runif(100), y=runif(100), 
  assignment=sample(1:3, 100, replace=TRUE))
head(df)  

dsd <- DSD_Wrapper(df, assignment=3)  
dsd
plot(dsd, n=100)  
}
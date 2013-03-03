\name{DSD_Wrapper}
\alias{DSD_Wrapper}
\title{A Data Stream Wrapper for Data Frames and Matrices)}
\description{
This class wraps either a data frame or matrix in memory and
provides access to the data as a data stream. 
The data can either be looped or replayed manually to give
the exact same data several times.
}

\usage{DSD_Wrapper(x, n, k=NA, loop=FALSE, assignment = NULL)}

\arguments{
	\item{x}{A data frame or matrix with the data to be used in the stream.
	If \code{x} is a DSD object then a wrapper for \code{n} data points
	from this DSD is created. }
	\item{n}{Number of points used if \code{x} is a DSD object. If
	\code{x} is a data frame or matrix then \code{n} is ignored.}
	\item{k}{Optional: The number of clusters}
	\item{loop}{A flag that tells the stream to loop or 
				not to loop over the data frame.}
	\item{assignment}{A flag that tells the stream to return assignment information.}
}

%\details{
%}

\value{
Returns a \code{DSD_Wrapper} object 
(subclass of \code{DSD_R}, \code{DSD}).
}

\seealso{
	\code{\link{DSD}},
	\code{\link{reset_stream}}
}

\examples{
dsd <- DSD_GaussianStatic(k=3, d=2)
d <- get_points(dsd, 500)
head(d)

# creating the DSD_Wrapper from d
replayer <- DSD_Wrapper(d, k=3)
replayer

# creating 2 clusterers of different algorithms
dsc1 <- DSC_CluStream()
dsc2 <- DSC_DenStream(initPoints=100)

# clustering the same data in 2 DSC objects
cluster(dsc1, replayer, 500)
reset_stream(replayer) # resetting the replayer to the first position
cluster(dsc2, replayer, 500)
}

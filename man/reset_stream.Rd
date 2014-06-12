\name{reset_stream}
\alias{reset_stream}
\title{Reset a Data Stream to its Beginning}

\description{
Resets the counter in a DSD object to the beginning or any other position in
the stream.
}

\usage{
	reset_stream(dsd, pos = 1)
}

\arguments{
	\item{dsd}{An object of class \code{DSD_ReadStream}, \code{DSD_Wrapper}
  \code{DSD_ScaleStream} or \code{DSD_MG}.}
	\item{pos}{Position in the stream (the beginning of the stream is position 1).}
}

\details{
Resets the counter of the stream object. For 
\code{DSD_ReadStream} objects, this is done by calling \code{seek()} on the
underlying connection. For \code{DSD_Wrapper}, the counter stored in the
environment variable is moved back to 1.
}

\seealso{
	\code{\link{DSD_ReadStream}},
	\code{\link{DSD_MG}},
	\code{\link{DSD_ScaleStream}},
	\code{\link{DSD_Wrapper}}
}

\examples{
# initializing the objects
dsd <- DSD_Gaussians(k=3, d=2)
replayer <- DSD_Wrapper(dsd, 100)
replayer

p <- get_points(replayer, 50)
replayer

# resetting the dsd to the begining of the stream
reset_stream(replayer) 
replayer
  
# set the dsd to position 21
reset_stream(replayer, pos=21)  
replayer
}

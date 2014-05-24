\name{DSD_ReadStream}
\alias{DSD_ReadStream}
\alias{close_stream}
\alias{scale_stream}
\title{Read a Data Stream from File}

\description{
A DSD class that reads a data stream from a file or any R connection.
}

\usage{
DSD_ReadStream(file, sep=",", k=NA, d=NA, take=NULL, assignment=NULL, 
     loop=FALSE)
close_stream(dsd)
}

\arguments{
	\item{file}{A file/URL or an open connection.}
	\item{sep}{The character string that separates dimensions in data
	points in the stream.}
	\item{k}{Number of true clusters, if known.}
	\item{d}{Number of dimensions (only used for print).}
	\item{take}{indices of columns to extract from the file.}
	\item{assignment}{column index for the cluster assignment (class attribute).}
	\item{loop}{If enabled, the object will loop through the stream when
	the end has been reached. If disabled, the object will warn the user
	upon reaching the end.}
	\item{dsd}{A object of class \code{DSD_ReadStream}.}
}

\details{
\code{DSD_ReadStream} uses \code{read.table()} to read in data from an R
connection. The connection is responsible for maintaining where the stream
is currently being read from. In general, the connections will consist of files
stored on disk but have many other possibilities (see \code{\link{connection}}).

The position in the file can be reset to the begining using 
\code{reset_stream()}. The connection can be closed using \code{close_stream()}.
}

\value{
    An object of class \code{DSD_ReadStream} (subclass of \code{DSD_R},
    \code{DSD}).
}

\seealso{
	\code{\link{DSD}},
	\code{\link{reset_stream}},

}

\examples{
# creating data and writing it to disk
dsd <- DSD_Gaussians(k=3, d=5)
write_stream(dsd, "data.txt", n=100, sep=",")

# reading the same data back (as a loop)
dsd2 <- DSD_ReadStream("data.txt", sep=",", loop=TRUE)
dsd2

# clean up
close_stream(dsd2)
file.remove("data.txt")

# example with a part of the kddcup1999 data (take only cont. variables)
file <- system.file("examples", "kddcup10000.data.gz", package="stream")
dsd <- DSD_ReadStream(gzfile(file),
        take=c(1, 5, 6, 8:11, 13:20, 23:41), assignment=42, k=7)
dsd

get_points(dsd,5)


# plot 100 points (projected on the first two principal components)
plot(dsd, n=100, method="pc")

close_stream(dsd)
}

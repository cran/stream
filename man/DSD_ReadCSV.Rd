\name{DSD_ReadCSV}
\alias{DSD_ReadCSV}
\alias{close_stream}
\alias{scale_stream}
\title{Read a Data Stream from File}

\description{
A DSD class that reads a data stream from a file or any R connection.
}

\usage{
DSD_ReadCSV(file, k=NA, d=NA, take=NULL, 
  class=NULL, loop=FALSE, sep=",", header=FALSE, skip=0, ...)
close_stream(dsd)
}

\arguments{
	\item{file}{A file/URL or an open connection.}
	\item{k}{Number of true clusters, if known.}
	\item{d}{Number of dimensions (only used for print).}
	\item{take}{indices of columns to extract from the file.}
	\item{class}{column index for the class attribute/cluster label.}
	\item{loop}{If enabled, the object will loop through the stream when
	  the end has been reached. If disabled, the object will warn the user
	  upon reaching the end.}
	\item{sep}{The character string that separates dimensions in data
	  points in the stream.}
	\item{header}{Does the first line contain variable names?}
	\item{skip}{the number of lines of the data file to skip before               
    beginning to read data.}
	\item{...}{Further arguments are passed on to \code{read.table}. 
	  This can for example be used for encoding, quotes, etc.}
	\item{dsd}{A object of class \code{DSD_ReadCSV}.}
}

\details{
\code{DSD_ReadCSV} uses \code{read.table()} to read in data from an R
connection. The connection is responsible for maintaining where the stream
is currently being read from. In general, the connections will consist of files
stored on disk but have many other possibilities (see \code{\link{connection}}).

The position in the file can be reset to the beginning using 
\code{reset_stream()}. The connection can be closed using \code{close_stream()}.
}

\value{
    An object of class \code{DSD_ReadCSV} (subclass of \code{DSD_R},
    \code{DSD}).
}

\seealso{
	\code{\link{DSD}},
	\code{\link{reset_stream}},
	\code{\link{read.table}}.
}

\examples{
# creating data and writing it to disk
stream <- DSD_Gaussians(k=3, d=5)
write_stream(stream, "data.txt", n=100, sep=",")

# reading the same data back (as a loop)
stream2 <- DSD_ReadCSV("data.txt", sep=",", loop=TRUE)
stream2

# clean up
close_stream(stream2)
file.remove("data.txt")

# example with a part of the kddcup1999 data (take only cont. variables)
file <- system.file("examples", "kddcup10000.data.gz", package="stream")
stream <- DSD_ReadCSV(gzfile(file),
        take=c(1, 5, 6, 8:11, 13:20, 23:41), class=42, k=7)
stream

get_points(stream,5)


# plot 100 points (projected on the first two principal components)
plot(stream, n=100, method="pc")

close_stream(stream)
}

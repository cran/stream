\name{write_stream}
\alias{write_stream}
\title{Write a Data Stream to a File}
\description{Writes points from a data stream DSD object to a file or a 
connection.}

\usage{
	write_stream(dsd, file, n=100, block=100000L, 
	assignment=FALSE, sep=",", 
	col.names=FALSE, row.names=FALSE, ...)
}

\arguments{
	\item{dsd}{The DSD object that will generate the data points for output.}
	\item{file}{A file name or a R connection to be written to.}
	\item{n}{The number of data points to be written.}
	\item{block}{Write stream in blocks to improve file I/O speed.}
	\item{assignment}{Save the assignment of the points as the last column.}
	\item{sep}{The character that will separate attributes in a data point.}
	\item{col.names}{A flag that determines if column names will be output.}
	\item{row.names}{A flag that determines if row names will be output.}
	\item{...}{Additional parameters that are passed to \code{write.table()}.}
}

%\details{
%}

\value{
There is no value returned from this operation.
}

\seealso{
	\code{\link{write.table}},
	\code{\link{DSD}}
}

\examples{
# creating data and writing it to disk
dsd <- DSD_GaussianStatic(k=3, d=5)
write_stream(dsd, file="data.txt", n=100)

# clean up
file.remove("data.txt")
}

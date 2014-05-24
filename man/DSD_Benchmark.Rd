\name{DSD_Benchmark}
\alias{DSD_Benchmark}
\title{Data Stream Generator for Benchmark Data}
\description{
A data stream generator that generates 
several dynamic streams indented to be benchmarks to compare 
data stream clustering algorithms.
}

\usage{
DSD_Benchmark(i=1)
}
\arguments{
    \item{i}{number of the benchmark.}
}

%\details{
%}

\value{
Returns a \code{DSD} object.
}
	   
\seealso{
	\code{\link{DSD}}
}

\examples{
dsd <- DSD_Benchmark(i=1)
\dontrun{
animate_data(dsd, n=2000, horizon=100, xlim=c(-.2,1.2), ylim=c(-.2,1.2)) 
}
}

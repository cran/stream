\name{DSD_BarsAndGaussians}
\alias{DSD_BarsAndGaussians}
\title{Data Stream Generator for Bars and Gaussians}
\description{
A data stream generator which creates the shape of two bars and two Gaussians clusters with different density.}

\usage{
DSD_BarsAndGaussians(noise = 0)
}

\arguments{
  \item{noise}{The amount of noise that should be added to the output.}
}

%\details{
%}

\value{
Returns a \code{DSD_BarsAndGaussians} object.
}
	   
\seealso{
	\code{\link{DSD}}
}

\examples{
# create data stream with three clusters in 2D
dsd <- DSD_BarsAndGaussians()

# plotting the data
plot(dsd)
}

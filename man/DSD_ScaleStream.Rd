\name{DSD_ScaleStream}
\alias{DSD_ScaleStream}
\title{Scale a Stream from a DSD}

\description{
  A DSD class that wraps a DSD and scales it.
}

\usage{
  DSD_ScaleStream(dsd,  
                  center=TRUE, scale=TRUE,
                  n=1000)
}

\arguments{
  \item{dsd}{A object of class \code{DSD} that will be scaled.}
  \item{center, scale}{logical or a numeric vector of length equal to
                       the number of columns used for centering/scaling }
  \item{n}{The number of points used to creating the centering/scaling }
}

\details{
  \code{scale_stream()} estimates the values for centering and scaling 
  (see \code{scale} in \pkg{base}) using \code{n} points from the 
  stream.
}

\value{
  An object of class \code{DSD_ScaleStream} (subclass of \code{DSD_R},
                                            \code{DSD}).
}

\seealso{
  \code{\link{DSD}},
  \code{\link{reset_stream}},
  \code{\link{scale}} in \pkg{base},
  
}

\examples{
  # creating data and writing it to disk
  dsd <- DSD_GaussianStatic(k=3, d=5)
  write_stream(dsd, "data.txt", n=100, sep=",")
  
  # reading the same data back (as a loop)
  dsd2 <- DSD_ReadStream("data.txt", sep=",", loop=TRUE)
  plot(dsd2)
  
  dsd3 <- DSD_ScaleStream(dsd2)
  plot(dsd3)
  
  # clean up
  close_stream(dsd2)
  file.remove("data.txt")
}

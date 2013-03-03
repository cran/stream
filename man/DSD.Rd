\name{DSD}
\alias{DSD}
\title{Data Stream Data Generator Base Class}
\description{Abstract base class for all DSD (Data Stream Data
Generator) classes.}

\details{
The \code{DSD} class cannot be instantiated, but it serves
as a base class from which all DSD objects inherit. There is
only one function that needs to be implemented
for a new \code{DSD} subclass:
\code{get_points()}. \code{DSD} also provides
\code{print()} and \code{plot()}.
}

\seealso{
	\code{\link{get_points}},
	\code{\link[stream]{plot}}
}

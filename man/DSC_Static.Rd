\name{DSC_Static}
\alias{DSC_Static}
\title{Create as Static Copy of a Clustering}
\description{
    This representation cannot perform clustering anymore, but it
    also does not need the supporting data structures. It only stores
    the cluster centers and weights.
    }

\usage{
	DSC_Static(x, type=c("auto", "micro", "macro"))
}

\arguments{
	\item{x}{The clustering (a DSD object) to copy.}
	\item{type}{which clustering to copy.}
}

%\details{
%}

\value{
An onject of class \code{DSC_Static} (sub class of  \code{DSC}, \code{DSC_R}).
The list also contains either
 \code{DSC_Micro} or \code{DSC_Macro} depending on what type 
 of clustering was copied.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}},
	\code{\link{DSC_Macro}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

tnn <- DSC_tNN(r=0.05, lambda=.01, macro=FALSE)
cluster(tnn, dsd, 500)
tnn

# create a static copy of the clustering
static <- DSC_Static(tnn)
static

plot(static, dsd)
}

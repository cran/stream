\name{get_centers}
\alias{get_centers}
\title{Get Cluster Centers from a DSC}

\description{
Gets the cluster centers (micro- or macro-clusters) from a DSC object.
}

\usage{
	get_centers(x, type=c("auto", "micro", "macro"), ...)
}

\arguments{
	\item{x}{The DSC object the centers are being requested from.}
	\item{type}{get centers for micro- or macro-clusters. Auto
	        used the class of dsc to decide.}
	\item{...}{Additional parameters.}
}

%\details{
%}

\value{
A data.frame with the micro- or macro-cluster centers as rows.
}

\seealso{
	\code{\link{DSC}}
}

\examples{
# setting up the objects
dsd <- DSD_GaussianStatic(k=3)
dsc <- DSC_Kmeans(k=3)
cluster(dsc, dsd, 500)

# getting the centers (these are macro-clusters since 
# Kmeans is a macro-cluster)
get_centers(dsc)
}

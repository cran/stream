\name{plot}
\alias{plot}
\alias{plot.DSD}
\alias{plot.DSC}
\title{Plotting Data Stream Data and Clusterings}
\description{Methods to plot data stream data and clusterings.}

\usage{
    \method{plot}{DSD}(x, n = 500, col= NULL, 
	    pch= NULL, ..., method="pairs")
    \method{plot}{DSC}(x, dsd = NULL, n = 500,
	    col_points="gray", col_clusters="red",
	    weights=TRUE, scale=c(1,5), cex=1, pch=NULL, ..., method="pairs", 
	    type = c("auto", "micro", "macro"))
}

\arguments{
	\item{x}{the DSD or DSC object to be plotted.}
	\item{dsd}{a DSD object to plot the data in the 
	    background.}
	\item{n}{number of plots taken from the dsd to plot.}
	\item{col, col_points, col_clusters}{colors
	    used for plotting.}
	\item{weights}{the size of the symbols for 
	    micro- and macro-clusters represents its weight.}
	\item{scale}{range for the symbol sizes used.}
	\item{cex}{size factor for symbols.}
	\item{pch}{symbol type.}
	\item{method}{method used for plotting: 
	    "pairs" (pairs plot), 
	    "plot" (plot first two dimensions) or 
	    "pc" (plot first 2 principal components).}
	\item{type}{Plot micro- or macro-clusters. Auto
	        used the class of dsc to decide.}
	\item{\dots}{further arguments are passed on to
	plot or pairs in \pkg{graphics}.}
}

%\details{
%}

%\value{
%}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSD}}
}

\examples{
dsd <- DSD_GaussianStatic(k=3)

## plot data
plot(dsd, n=500)

## create and plot micro-clusters
micro <- DSC_DenStream(initPoints=100)
cluster(micro, dsd, 500)
plot(micro)

## plot on data
plot(micro, dsd)

## create and plot macro-clusters
macro <- DSC_KmeansW(k=3)
recluster(macro, micro)
plot(macro, dsd)
}

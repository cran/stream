\name{plot}
\alias{plot}
\alias{plot.DSD}
\alias{plot.DSC}
\title{Plotting Data Stream Data and Clusterings}
\description{Methods to plot data stream data and clusterings.}

\usage{
    \method{plot}{DSD}(x, n = 500, col= NULL, 
	    pch= NULL, ..., method="pairs", dim = NULL)
    \method{plot}{DSC}(x, dsd = NULL, n = 500,
	    col_points="gray", col_clusters=c("red", "blue"),
	    weights=TRUE, scale=c(1,5), cex=1, pch=NULL, ..., method="pairs", 
	    dim = NULL, type = c("auto", "micro", "macro", "both"))
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
	\item{dim}{an integer vector with the dimensions to plot. If \code{NULL} 
      then for methods \code{"pairs"} and \code{"pc"} all dimensions are used 
      and for \code{"plot"} the first two dimensions are plotted.}
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
set.seed(1000)
dsd <- DSD_Gaussians(k=3, d=3)

## plot data
plot(dsd, n=500)
plot(dsd, method="pc", n=500)
plot(dsd, method="plot", dim=c(1,3), n=500)


## create and plot micro-clusters
dstream <- DSC_DStream(gridsize=0.1)
cluster(dstream, dsd, 500)
plot(dstream)

## plot on data
plot(dstream, dsd)
plot(dstream, dsd, method="pc")

## plot micro and macro-clusters
plot(dstream, dsd, type="both")
}

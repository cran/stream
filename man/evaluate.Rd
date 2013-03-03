\name{evaluate}
\alias{evaluate}
\alias{evaluate_cluster}
\alias{print.stream_eval}
\title{Evaluate Clusterings}

\description{
	Gets evaluation measures for micro or macro-clusters from a DSC object
	given the original DSD object.
}

\usage{
    evaluate(dsc, dsd, method, n = 1000, 
	    type=c("auto", "micro", "macro"), assign="micro", ...)
    evaluate_cluster(dsc, dsd, macro=NULL, method, n = 1000,
	    type=c("auto", "micro", "macro"), assign="micro",
	    horizon=100, ...)
}

\arguments{
	\item{dsc}{The DSC object that the evaluation measure is being requested from.}
	\item{dsd}{The DSD object that holds the initial training data for the DSC.}
	\item{method}{The requested measure.}
	\item{n}{The number of data points being requested.}
	\item{type}{Use micro- or macro-clusters for evaluation. Auto
	        used the class of dsc to decide.}
	\item{assign}{Assign points to micro or macro-clusters? In case 
	assignment is done to micro-clusters while type is macro, micro-cluster
	assignments are translated to macro-cluster assignments using 
	information on what micro-clusters constitute each macro-cluster.}
	\item{macro}{A DSC_macro object for reclustering.}
	\item{horizon}{Number of points after which evaluation is done (see detail section).}
	\item{...}{Unused arguments are ignored.}
}

\details{
	For evaluation, each data points are assigned
	to its nearest cluster (using Euclidean distance to the cluster
	centers). Then
	for each cluster the majority class is determined. Based on 
	the majority class and the class of all assigned points several 
	evaluation measures are available:
	\itemize{
		\item	\code{"numClusters"} number of clusters, 
		\item	\code{"numClasses"} number of classes,
		\item	\code{"precision"}, 
		\item	\code{"recall"},
		\item	\code{"F1"} F1 measure, 
		\item	\code{"purity"} (of found clusters), 
		\item	\code{"classPurity"} (of real clusters; see Wan et al (2009)), 
		\item	\code{"fpr"} false positive rate,
		\item	\code{"SSQ"} sum of squares, 
		\item	\code{"Euclidean"} Euclidean dissimilarity of
		          the memberships (See Dimitriadou, Weingessel and Hornik
			            (2002)), 
		\item	\code{"Manhattan"} Manhattan dissimilarity of
		          the memberships, 
		\item	\code{"Rand"} Rand index (see Rand (1971)),
		\item	\code{"cRand"} Adjusted Rand index (see Hubert and Arabie (1985)),
		\item	\code{"NMI"} Normalized Mutual Information (see Strehl and Ghosh
		          (2002)),
		\item	\code{"KP"} Katz-Powell index (see Katz and
		          Powell (1953)),
		\item	\code{"angle"} maximal cosine of the angle between the agreements,
		\item	\code{"diag"} maximal co-classification rate,
		\item	\code{"FM"} Fowlkes and Mallows's index (see Fowlkes and Mallows (1983)),
		\item	\code{"Jaccard"} Jaccard index, 
		\item	\code{"PS"} Prediction Strength (see Tibshirani and Walter (2005)).
	}

Many measures are the average over all clusters. For example, purity is
the average purity over all clusters.

For \code{DSC_Micro} objects, data points are assigned to micro-clusters
and then each micro-cluster is evaluated. For \code{DSC_Macro} objects, 
data points by default (\code{assign="micro"}) also assigned to
micro-clusters, but these assignments are translated to macro-clusters.
The evaluation is here done for macro-clusters. This is important when 
macro-clustering is done with algorithms which do not create spherical
clusters (e.g, hierarchical clustering with single-linkage or DBSCAN) and
this assignment to the macro-clusters directly (i.e., their center) 
does not make sense.

Using \code{type} and \code{assign}, the user can select how to assign data
points and ad what level (micro or macro) to evaluate.

Many of the above measures are implemented package \pkg{clue} in function
\code{cl_agreement().}

\code{evaluate_cluster()} is used to evaluate an evolving data stream using
the method described by Wan et al. (2009). Of the \code{n} data points
\code{horizon} many points are clustered and then the evaluation
measure is calculated on the same data points. The idea is to find out if 
the clustering algorithm was able to adapt to the changing stream.
}

\value{
    \code{evaluate} returns an object of class \code{stream_eval}
    which is a numeric vector of the values of the requested measures
    and two attribures, \code{"type"} and
    \code{"assign"}, to see at what level the evaluation was done.
    
}

\references{
E. Dimitriadou, A. Weingessel and K. Hornik (2002).  A combination
scheme for fuzzy clustering.  International Journal of Pattern
Recognition and Artificial Intelligence, 16, 901-912.

E. B. Fowlkes and C. L. Mallows (1983).  A method for comparing
two hierarchical clusterings.  Journal of the American
Statistical Association, 78, 553-569.

L. Hubert and P. Arabie (1985).  Comparing partitions.  Journal
of Classification, 2, 193-218.

W. M. Rand (1971).  Objective criteria for the evaluation of
clustering methods.  Journal of the American Statistical
Association, 66, 846-850.

L. Katz and J. H. Powell (1953).  A proposed index of the
conformity of one sociometric measurement to another.
Psychometrika, 18, 249-256.

A. Strehl and J. Ghosh (2002).  Cluster ensembles - A knowledge
reuse framework for combining multiple partitions.  Journal of
Machine Learning Research, 3, 583-617.

R. Tibshirani and G. Walter (2005).  Cluster validation by
Prediction Strength.  Journal of Computational and Graphical
Statistics, 14/3, 511-528.

L Wan, W.K. Ng, X.H. Dang, P.S. Yu and K. Zhang (2009). Density-Based 
Clustering of Data Streams at Multiple Resolutions, 3(3).
}

\seealso{
            \code{\link{animate_cluster}},
            \code{\link[clue]{cl_agreement}} in \pkg{clue}.
}


\examples{
# Create stream
dsd <- DSD_GaussianStatic(k=3, d=2)

micro <- DSC_DenStream()
cluster(micro, dsd, 500)
# Evaluate micro-clusters 
# Note: we use here only n=500 points for evaluation to speed up execution
evaluate(micro, dsd, method=c("purity","crand"), n=500)

macro <- DSC_KmeansW(2)
recluster(macro, micro)
# Evaluate macro-clusters by assigning to micro-clusters
evaluate(macro, dsd, method=c("purity","crand"), n=500)
# Evaluate macro-clusters by assigning to macro-clusters
# This gives a similar result as the micro-cluster assignment since
# the real clusters are spherical (multidimensional Gaussians).
evaluate(macro, dsd, method=c("purity","crand"), n=500, assign="macro")


# Evaluate an evolving data stream
dsd <- DSD_GaussianMoving()
micro <- DSC_DenStream(initPoints=100)
evaluate_cluster(micro, dsd, method=c("purity","crand"), n=600, horizon= 100, 
                 assign="micro")

reset_stream(dsd)
micro <- DSC_tNN(r=.1, macro=FALSE, lambda=.01, decay_interval=100)
macro <- DSC_KmeansW(3)
evaluate_cluster(micro, dsd, macro, method=c("purity","crand"), n=600, 
                 horizon=100, assign="micro")

# visualize
\dontrun{
reset_stream(dsd)
micro <- DSC_tNN(r=.1, macro=FALSE, lambda=.1, decay_interval=20)
animate_cluster(micro, dsd, macro, n=600,
	evaluationMethod=c("purity"), pointInterval=20, 
	xlim=c(-.2,1.2), ylim=c(-.2,1.2))}
}

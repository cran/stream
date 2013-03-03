#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest 
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


kmeans_refClass <- setRefClass("kmeans", 
	fields = list(
		data	    = "data.frame",
		k	    = "numeric",
		iter.max    = "numeric",
		nstart	    = "numeric",
		algorithm   = "character",
		assignment  = "numeric",
		details	    = "ANY",
		clusterCenters = "data.frame",
		weights	    = "numeric",
		clusterWeights = "numeric"
	), 

	methods = list(
		initialize = function(
			iter.max    = 10,
			k	    = 3,
			nstart	    = 1,
			algorithm   = c("Hartigan-Wong", "Lloyd", 
				"Forgy","MacQueen")
			) {

		    iter.max	<<- iter.max 
		    k		<<- k 
		    nstart	<<- nstart
		    algorithm   <<- match.arg(algorithm)
		    assignment	<<- numeric() 
		    weights	<<- numeric() 
		    clusterWeights <<- numeric() 
		    clusterCenters <<- data.frame()
		    data	<<- data.frame()
		    
		    .self
		}

	),
)

kmeans_refClass$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
	    if(length(data)>0) {
		warning("Kmeans: Previous data is being overwritten")
	    }

	    weights <<- weight
	    data <<- x
	    if(nrow(data)>k) {
		km <- kmeans(x=data, centers=k, 
			iter.max = iter.max, nstart = nstart,
			algorithm = algorithm)

		assignment <<- km$cluster
		clusterCenters <<- data.frame(km$centers)
		details <<- km
	    } else {
		assignment <<- 1:nrow(data)
		clusterCenters <<- x
		details <<- NULL
	    }

	    clusterWeights <<- sapply(1:k, FUN =
		    function(i) sum(assignment==i))

	}
	)

### creator    
DSC_Kmeans <- function(k, iter.max = 10, nstart = 1,
	algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
		"MacQueen")) {
    
    algorithm <- match.arg(algorithm)

    structure(list(description = "k-Means",
		    RObj = kmeans_refClass$new(
			    iter.max = iter.max, k=k, nstart = nstart,
			    algorithm = algorithm)),
	    class = c("DSC_Kmeans","DSC_Macro","DSC_R","DSC"))
}

get_macroclusters.DSC_Kmeans <- function(x) x$RObj$clusterCenters
get_macroweights.DSC_Kmeans <- function(x) x$RObj$clusterWeights

get_microclusters.DSC_Kmeans <- function(x) x$RObj$data
get_microweights.DSC_Kmeans <- function(x) x$RObj$weights

microToMacro.DSC_Kmeans <- function(x, micro=NULL){ 
    if(is.null(micro)) micro <- 1:nclusters(x, type="micro")
    structure(x$RObj$assignment[micro], names=micro)
}  


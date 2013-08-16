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


DBSCAN <- setRefClass("DBSCAN", 
	fields = list(
		eps	        = "numeric",
		MinPts	    = "numeric",
    weighted    = "logical",
		assignment  = "numeric",
		details	    = "ANY",
		data        = "data.frame",
    weights	    = "numeric",
		clusterCenters = "data.frame",
		clusterWeights = "numeric"
		), 

	methods = list(
		initialize = function(eps = .1, MinPts	= 5, weighted = TRUE) {

        eps     <<- eps
		    MinPts  <<- MinPts  
        weighted <<- weighted
        
        data    <<- data.frame()
		    weights <<- numeric()
		    clusterWeights <<- numeric()
		    clusterCenters <<- data.frame()

		    .self
		}

		),
	)

DBSCAN$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
	    if(length(data)>0) {
		warning("DBSCAN: Previous data is being overwritten")
	    }

	    ### save micro-clusters
      weights <<- weight
	    data <<- x

      if(!weighted) weight <- NULL
      
      ### internal dbscan uses weights
	    DBSCAN <- .dbscan(data, eps, MinPts = MinPts, scale = FALSE, 
		    method = "hybrid", seeds = TRUE, showplot = FALSE, countmode = NULL,
        weight=weight)

	    assignment <<- DBSCAN$cluster

	    ### FIXME: we currently remove unassigned data!
	    row_sub <- unlist(lapply(assignment, function(x) all(x !=0 )))
	    data <<- data[row_sub,]
	    assignment <<- assignment[row_sub]
	    details <<- DBSCAN


	    if(length(assignment>0)) {
		k <- max(assignment)
		clusterCenters <<- as.data.frame(t(sapply(1:k, FUN=
					function(i) colMeans(data[assignment==i,]))))
		clusterWeights <<- sapply(1:k, FUN =
			function(i) sum(weights[assignment==i], na.rm=TRUE))
	    }else{ ### no clusters found
		k <- 0
		clusterCenters <<- data.frame()
		clusterWeights <<- numeric(0)
	    }


	}
	)

### creator    
DSC_DBSCAN <- function(eps, MinPts = 5, weighted = TRUE) {

    DBSCAN <- DBSCAN$new(eps=eps, MinPts = MinPts, weighted = weighted)

    if(weighted) desc <- "DBSCAN (weighted)" else desc <- "DBSCAN (unweighted)"
    l <- list(description = desc,
	    RObj = DBSCAN)

    class(l) <- c("DSC_DBSCAN","DSC_Macro","DSC_R","DSC")
    l
}

get_macroclusters.DSC_DBSCAN <- function(x) x$RObj$clusterCenters
get_macroweights.DSC_DBSCAN <- function(x) x$RObj$clusterWeights

get_microclusters.DSC_DBSCAN <- function(x) x$RObj$data
get_microweights.DSC_DBSCAN <- function(x) x$RObj$weights

microToMacro.DSC_DBSCAN <- function(x, micro=NULL){
    if(is.null(micro)) micro <- 1:nclusters(x, type="micro")
    structure(x$RObj$assignment[micro], names=micro)
}

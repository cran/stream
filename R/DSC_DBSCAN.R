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
		data	    = "data.frame",
		eps	    = "numeric",
		MinPts	    = "numeric",
		scale	    = "logical",
		method	    = "character",
		seeds	    = "logical",
		showplot    = "logical",
		countmode   = "ANY",
		assignment  = "numeric",
		details	    = "ANY",
		weights	    = "numeric",
		clusterCenters = "data.frame",
		clusterWeights = "numeric"
		), 

	methods = list(
		initialize = function(
			MinPts	= 5,
			scale	= FALSE,
			method	= c("hybrid", "raw","dist"),
			seeds	= TRUE,
			showplot = FALSE,
			countmode = NULL
			) {

		    data    <<- data.frame()
		    weights <<- numeric()
		    MinPts  <<- MinPts
		    scale   <<- scale
		    method  <<- method
		    seeds   <<- seeds
		    showplot <<- showplot
		    countmode <<- countmode
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

	    weights <<- weight
	    data <<- x

	    DBSCAN <- dbscan(data, eps, MinPts = 5, scale = FALSE, 
		    method = c("hybrid", "raw", "dist"), seeds = TRUE, 
		    showplot = FALSE, countmode = NULL)

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
DSC_DBSCAN <- function(eps, MinPts = 5, scale = FALSE, method = c("hybrid", "raw",
		"dist"), seeds = TRUE, showplot = FALSE, countmode = NULL) {

    DBSCAN <- DBSCAN$new(
	    MinPts = MinPts, scale = scale,
	    method = method,seeds=seeds,showplot=showplot,countmode=countmode)

    DBSCAN$eps <- eps

    l <- list(description = "DBSCAN",
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

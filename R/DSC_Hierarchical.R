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


hierarchical <- setRefClass("hierarchical", 
	fields = list(
		data	= "data.frame",
		dataWeights = "numeric",
		d	= "matrix",
		method  = "character",
		members	= "ANY",
		k	= "integer",
		assignment = "numeric",
		details = "ANY",
		centers	= "data.frame",
		weights = "numeric"
	), 

	methods = list(
		initialize = function(
			k,
			method	= "complete",
			members = NULL
			) {
		    
		    data	<<- data.frame()
		    dataWeights	<<- numeric()
		    weights	<<- numeric()
		    centers	<<- data.frame()
		    method	<<- method 
		    members	<<- members
		    k		<<- k

		    .self
		}

	),
)

hierarchical$methods(cluster = function(x,  weight = rep(1,nrow(x)), ...) {
	    if(length(data)>0) {
		warning("Hierarchical: Previous data is being overwritten")
	    }
	    
	    dataWeights <<- weight
	    data <<- x
	    
	    if(nrow(data)>=2) {
		hierarchical <-hclust(d=dist(x), method = method, 
			members= members)
		
		if(k < length(unlist(hierarchical['height'])))
		    memb <- cutree(hierarchical, k = k)
		else
		    memb <- 1
		
		assignment <<- memb
		details <<- hierarchical
	    
		centers <<- as.data.frame(t(sapply(1:k, FUN=
			function(i) colMeans(data[assignment==i,]))))
		weights <<- sapply(1:k, FUN =
			function(i) sum(dataWeights[assignment==i], na.rm=TRUE))

	    }
	}
	)

### creator    
DSC_Hierarchical <- function(k, method = "complete", members = NULL) {

    hierarchical <- hierarchical $new( 
	    k=as.integer(k), method = method, members = members)

    l <- list(description = paste("Hierarchical -", method),
	    RObj = hierarchical)

    class(l) <- c("DSC_Hierarchical","DSC_Macro","DSC_R","DSC")
    l
}

get_microclusters.DSC_Hierarchical <- function(x) x$RObj$data
get_microweights.DSC_Hierarchical <- function(x) x$RObj$dataWeights

get_macroclusters.DSC_Hierarchical <- function(x) x$RObj$centers
get_macroweights.DSC_Hierarchical <- function(x) x$RObj$weights

microToMacro.DSC_Hierarchical <- function(x, micro=NULL){ 
    if(is.null(micro)) micro <- 1:nclusters(x, type="micro")
    structure(x$RObj$assignment[micro], names=micro)
}  

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


kmeansW_refClass <- setRefClass("kmeansW", 
	fields = list(
		data	    = "data.frame",
		dataWeights = "numeric",
		iter.max    = "numeric",
		nstart	    = "numeric",
		assignment  = "numeric",
		k	    = "numeric",
		centers	    = "data.frame",
		weights	    = "numeric",
		details	    = "ANY"
		), 

	methods = list(
		initialize = function(
			k	    = 3,
			iter.max    = 10,
			nstart	    = 1
			) {

		    iter.max	<<- iter.max 
		    nstart	<<- nstart
		    k		<<- k
		    data	<<- data.frame()
		    dataWeights	<<- numeric()
		    assignment	<<- numeric()
		    weights	<<- numeric()
		    centers	<<- data.frame()

		    .self
		}
		)
	)

kmeansW_refClass$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
	    if(length(data)>0) {
		warning("KmeansW: Previous data is being overwritten")
	    }

	    data <<- x
	    dataWeights <<- weight

	    if(nrow(x)>k) {
		km <- kmeansW(x=data, weight=dataWeights, centers=k, 
			iter.max = iter.max, nstart = nstart)

		assignment <<- km$cluster
		centers <<- data.frame(km$centers)
		details <<- km
	    } else {
		assignment <<- 1:nrow(data)
		centers <<- x
		details <<- NULL
	    }

	    weights <<- sapply(1:k, FUN =
		    function(i) sum(dataWeights[assignment==i], na.rm=TRUE))
	}
	)

### creator    
DSC_KmeansW <- function(k, iter.max = 10, nstart = 1) {
    structure(list(
		    description = "Weighted k-Means",
		    RObj = kmeansW_refClass$new(k=k, iter.max = iter.max, 
			    nstart = nstart)), 
	    class=  c("DSC_KmeansW","DSC_Macro","DSC_R","DSC"))
}

get_macroclusters.DSC_KmeansW <- function(x) x$RObj$centers
get_macroweights.DSC_KmeansW <- function(x) x$RObj$weights

get_microclusters.DSC_KmeansW <- function(x) x$RObj$data
get_microweights.DSC_KmeansW <- function(x) x$RObj$dataWeights

microToMacro.DSC_KmeansW <- function(x, micro=NULL){ 
    if(is.null(micro)) micro <- 1:nclusters(x, type="micro")
    structure(x$RObj$assignment[micro], names=micro)
}   


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
                                 k	    = "numeric",
                                 weighted = "logical",
                                 iter.max    = "numeric",
                                 nstart	    = "numeric",
                                 algorithm   = "character",
                                 assignment  = "numeric",
                                 data      = "data.frame",
                                 weights	    = "numeric",
                                 clusterCenters = "data.frame",
                                 clusterWeights = "numeric",
                                 details      = "ANY"
                               ), 
                               
                               methods = list(
                                 initialize = function(
                                   k      = 3,
                                   weighted = TRUE,
                                   iter.max    = 10,
                                   nstart	    = 1,
                                   algorithm   = c("Hartigan-Wong", "Lloyd", 
                                                   "Forgy","MacQueen")
                                 ) {
                                   
                                   k  	<<- k 
                                   weighted <<- weighted
                                   iter.max	<<- iter.max 
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
    if(weighted) km <- kmeansW(x=data, weight=weights, centers=k, 
                      iter.max = iter.max, nstart = nstart)
    else km <- kmeans(x=data, centers=k, 
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
                              function(i) sum(weights[assignment==i]))
  
}
)

### creator    
DSC_Kmeans <- function(k, weighted = TRUE, iter.max = 10, nstart = 1,
                       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                                     "MacQueen")) {
  
  algorithm <- match.arg(algorithm)
  if(weighted) desc <- "weighted k-Means"
  else desc <-"k-Means"
  
  
  structure(list(description = desc,
                 RObj = kmeans_refClass$new(
                   k=k, weighted=weighted, iter.max = iter.max, nstart = nstart,
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


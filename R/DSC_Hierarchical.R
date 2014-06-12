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

### creator    
DSC_Hierarchical <- function(k=NULL, h=NULL, method = "complete", 
  min_weight=NULL, description=NULL) {
  
  hierarchical <- hierarchical$new( 
    k=k, h=h, method=method, min_weight=min_weight)
  
  if(is.null(description)) description <- paste("Hierarchical clustering (", method, ")", 
    sep='')
  
  l <- list(description = description, RObj = hierarchical)
  
  class(l) <- c("DSC_Hierarchical","DSC_Macro","DSC_R","DSC")
  l
}


### calculate centroids
.centroids <- function(centers, weights, assignment){
  macroID <- unique(assignment)
  macroID <- macroID[!is.na(macroID)]
  assignment[is.na(assignment)] <- -1 ### prevent NAs in matching
  
  cs <- as.data.frame(t(sapply(macroID, FUN=
      function(i) {
        take <- assignment==i
        colSums(centers[take,]*rep(weights[take], times=length(take)))/sum(weights[take])
      })))
  
  ws <- sapply(macroID, FUN =
      function(i) sum(weights[assignment==i], na.rm=TRUE))
  
  list(centers=cs, weights=ws)
}


hierarchical <- setRefClass("hierarchical", 
  fields = list(
    data	= "data.frame",
    dataWeights = "numeric",
    d	= "matrix",
    method  = "character",
    k	= "ANY",
    h = "ANY",
    assignment = "numeric",
    details = "ANY",
    centers	= "data.frame",
    weights = "numeric",
    min_weight = "numeric"
  ), 
  
  methods = list(
    initialize = function(
      k=NULL,
      h=NULL,
      method	= "complete",
      min_weight = NULL
    ) {
      
      if(is.null(k) && is.null(h)) stop("Either h or k needs to be specified.") 
      if(!is.null(k) && !is.null(h)) stop("Only h or k  can be specified.") 
      
      if(is.null(min_weight)) min_weight <<- 0
      else min_weight <<- as.numeric(min_weight)
      
      data	<<- data.frame()
      dataWeights	<<- numeric()
      weights	<<- numeric()
      centers	<<- data.frame()
      method	<<- method 
      k	<<- k
      h <<- h
      
      .self
    }
    
  ),
)

hierarchical$methods(
  cluster = function(x,  weight = rep(1,nrow(x)), ..., overwrite=FALSE) {
    if(length(data)>0 && !overwrite) warning("Hierarchical: Previous data is being overwritten")
    
    ### filter weak clusters
    if(min_weight>0) {
      x <- x[weight>min_weight,]
      weight <- weight[weight>min_weight]
    }
    
    data <<- x
    dataWeights <<- weight
    
    if((!is.null(k) && nrow(data) <=k) || nrow(data)<2) {
      centers <<- x
      weights <<- weight
    }else{
      hierarchical <- hclust(d=dist(x), method = method)
      details <<- hierarchical
      
      if(is.null(k) || k < length(unlist(hierarchical['height'])))
        assignment <<- cutree(hierarchical, k = k, h = h)
      else
        assignment <<- 1
      
      ### find centroids
      centroids <- .centroids(x, weight, assignment)
      centers <<- centroids$centers
      weights <<- centroids$weights
    }
  },
  
  get_microclusters = function(...) { data },
  get_microweights = function(...) { dataWeights }, 
  
  get_macroclusters = function(...) { centers },
  get_macroweights = function(...) { weights },
  
  microToMacro = function(micro=NULL, ...){ 
    if(is.null(micro)) micro <- 1:nrow(data)
    structure(assignment[micro], names=micro)
  }  
)



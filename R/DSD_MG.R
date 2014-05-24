#######################################################################
# Moving Generator -  Infrastructure for Moving Streams
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

add_cluster <- function(x, c) 
  UseMethod("add_cluster")
get_clusters <- function(x) 
  UseMethod("get_clusters")
remove_cluster <- function(x, i) 
  UseMethod("remove_cluster")

dsd_MG_refClass <- setRefClass("dsd_MG", 
  fields = list(
    t = "numeric",
    dimension = "numeric",
    clusters = "list",
    numberPerStep = "numeric"
  ),
  methods = list(
    initialize = function(d) {
      dimension  <<- d
      t <<- 1
      .self
    }
    
  ),
)

dsd_MG_refClass$methods(
  add_cluster = function(c) {
    if(c$RObj$dimension != dimension) stop("Cluster dimensions do not match!")
    clusters <<- append(clusters, list(c))
  },
  
  get_points = function(n, assignment = FALSE) {
    if(length(clusters)==0) stop("DSD_MG does not contain any clusters!")

    if(assignment) a <- integer(n)
    data <- matrix(NA_real_, nrow=n, ncol=dimension)
    
    j <- 0L
    while(j < n) {
      attributes <- as.matrix(sapply(clusters, function(x) x$RObj$get_attributes(t)))
      
      cluster <- attributes["cluster",]
      density <- attributes["density",]
      
      density[is.na(density)] <- 0
      if(all(density==0)) stop("No MGC is producing points for this time point.")

      pointsPerSecond <- sum(density)
      pointsLeftInSecond <- pointsPerSecond - (t - floor(t))*pointsPerSecond
      if((j + pointsLeftInSecond) <= n) k <- pointsLeftInSecond
      else k <- n-j
      
      ### got to next timestep...
      if(pointsLeftInSecond<1) {
        t <<- ceiling(t)
        next
      }
      
      k <- floor(k)
      
      if(k>=1) {
        clusterOrder <- sample(x=1:length(clusters), size=k, replace=TRUE, 
          prob=density/sum(density))
        
        data[(j+1):(j+k),] <- t(sapply(clusterOrder, FUN = function(i) {
          clusters[[i]]$RObj$get_points(t)
        }))
        
        if(assignment) {
          cl <- cluster[clusterOrder]
          cl[is.na(cl)] <- clusterOrder[is.na(cl)]
          a[(j+1):(j+k)] <- cl
        }
      }
      
      t <<- t + k/pointsPerSecond
      j <- j+k
    }
    
    data <- data.frame(data)
    
    if(assignment) attr(data,"assignment") <- a
    
    data
  }
)

### creator    
DSD_MG<- function(dimension = 2, ...) {
  
  desc <- "Moving Data Generator"
  
  x <- structure(list(description = desc,
                 RObj = dsd_MG_refClass$new(d = dimension)),
            class = c("DSD_MG","DSD_R","DSD"))
  
  lapply(list(...), function(c) add_cluster(x, c))
  
  x
}

get_points.DSD_MG <- function(x, n=1, assignment = FALSE,...) {
  x$RObj$get_points(n,assignment)
}

add_cluster.DSD_MG <- function(x, c) {
  x$RObj$add_cluster(c)
}

reset_stream.DSD_MG <- function(dsd, pos=1) {
  dsd$RObj$t <- pos
}

print.DSD_MG <- function(x, ...) {
  cat(paste(x$description, " (", paste(class(x), collapse=", "), ")", 
    '\n', sep=""))
  cat(paste('With', length(x$RObj$clusters), 'clusters', 'in', 
    x$RObj$dimension, 'dimensions. Time is', round(x$RObj$t, 3), '\n'))
}

get_clusters.DSD_MG <- function(x) {
  x$RObj$clusters
}

remove_cluster.DSD_MG  <- function(x,i) {
  x$RObj$clusters[[i]] <- NULL
}

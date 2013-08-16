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


## wrapper for cluster functions

cluster <- function(dsc, dsd, n=1, verbose=FALSE, ...) { 
  if (n < 1)
    stop("numPoints must be >= 1")
  
  # looping through the stream, feeding the new datapoints into 
  # the algorithm
  .cluster(dsc, dsd, n, verbose, ...)
  
  # so cl <- cluster(cl, ...) also works
  invisible(dsc)
}

### Workers
.cluster <- function(dsc, x, n, verbose=FALSE, ...) UseMethod(".cluster")

.cluster.DSC_MOA <- function(dsc, dsd, n, verbose=FALSE, ...) {
  ## data has to be all doubles for MOA clusterers!
  for (i in 1:n) {
    
    
    d <- get_points(dsd, 1)
    x <- .jcast(
      .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(d))),
      "weka/core/Instance"
    )
    
    .jcall(dsc$javaObj, "V", "trainOnInstanceImpl", x)
    
    if(verbose && !i%%1000) cat("Processed", i, "points -",
                                nclusters(dsc), "clusters\n")
    
  }	
}

### geting a block of data improves performance the R implementation
### needs to make sure that points are processed sequencially
### (make especially BIRCH faster by passing block data points at once)
.cluster.DSC_R <- function(dsc, dsd, n, verbose=FALSE, 
                           block=100000L, ...) {
  ### dsc contains an RObj which is  a reference object with a cluster method
  
  
  for(bl in .make_block(n, block)) {
    dsc$RObj$cluster(get_points(dsd, bl), ...)
    if(verbose) cat("Processed", bl, "points -",
                    nclusters(dsc), "clusters\n")
    
  }
}


.cluster.DSC_Macro <- function(dsc, dsd, n, ...) {
  d <- get_points(dsd, n=n)
  dsc$RObj$cluster(d, weight=rep(1, nrow(d)), ...)
}



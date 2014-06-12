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


### Implement a new clusterer
### Create an S3 class with elements description and RObj
### RObj needs to be a reference class with methods
###  * cluster(newdata, ...)
###  * get_microclusters(...), get_microweights(...)
###  * get_macroclusters(...), get_macroweights(...), microToMacro(micro, ...)

DSC_R <- function(...) stop("DSC_R is an abstract class and cannot be instantiated!")


### cluster worker
### geting a block of data improves performance the R implementation
### needs to make sure that points are processed sequencially
### (make especially BIRCH faster by passing block data points at once)
.cluster.DSC_R <- function(dsc, dsd, n, verbose=FALSE, 
  block=100000L, ...) {
  ### dsc contains an RObj which is  a reference object with a cluster method
  
  ### TODO: Check data
  
  for(bl in .make_block(n, block)) {
    dsc$RObj$cluster(get_points(dsd, bl), ...)
    if(verbose) cat("Processed", bl, "points -",
      nclusters(dsc), "clusters\n")
    
  }
}

### accessors
get_microclusters.DSC_R <- function(x, ...) x$RObj$get_microclusters(...)
get_microweights.DSC_R <- function(x, ...) x$RObj$get_microweights(...)
get_macroclusters.DSC_R <- function(x, ...) x$RObj$get_macroclusters(...)
get_macroweights.DSC_R <- function(x, ...) x$RObj$get_macroweights(...)
microToMacro.DSC_R <- function(x, micro=NULL, ...)  x$RObj$microToMacro(micro, ...)


### make a deep copy of the reference class in RObj 
get_copy.DSC_R <- function(x) {
	temp <- x
	temp$RObj <- x$RObj$copy(TRUE)
	temp
}


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



# implements a ring-buffer. pos is the current insert position
Window <- setRefClass("Window", 
  fields = list(
    horizon	= "integer",
    pos	= "integer",
    lambda = "numeric",
    centers	= "data.frame"
  ), 
  
  methods = list(
    initialize = function(horizon	= 100L, lambda = 0) {
      
      horizon	<<- horizon
      centers <<- data.frame() ### don't know dimensions yet!
      pos	<<- 1L 
      lambda <<- lambda
      
      .self
    }
    
  ),
)

DSC_Window <- function(horizon = 100, lambda=0) 
  structure(list(description = if(lambda>0) "Damped sliding window" else "Sliding window",
    RObj = Window$new(horizon = as.integer(horizon), lambda=lambda)),
    class = c("DSC_Window","DSC_Micro","DSC_R","DSC"))
  
Window$methods(
  cluster = function(x, ...) {
    
    n <- nrow(x)
    
    i <- 0L
    while(i < n) {
      
      ## process the next m points: all or to fill the current horizon
      m <- min(horizon - pos + 1L, n-i)
      
      ## first points? copy to get dim!
      if(nrow(centers)==0L) centers <<- x[(i+1L):(i+m),]
      else centers[pos:(pos+m-1L),] <<- x[(i+1L):(i+m),] 
      
      i <- i+m
      pos <<- pos+m
      if(pos>horizon) pos <<- 1L
    }
    
    # fix row names 
    rownames(centers) <<- NULL
  },
  
  get_microclusters = function(x, ...) {
    if(pos==1 || nrow(centers)<horizon) return(centers)
    cen <- centers[c(pos:(horizon), 1L:(pos-1L)),]
    rownames(cen) <- NULL
    cen
  },
  
  get_microweights = function(x, ...) {
    if(lambda <= 0) rep(1, nrow(centers))
    else 2^(-lambda*((nrow(centers)-1L):0))
  }   
)
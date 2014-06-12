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


DSC_Sample <- function(k = 100, biased = FALSE) 
  structure(list(description = 
      if(biased) "Reservoir sampling (biased)" else "Reservoir sampling",
    RObj = Sample$new(k = k, biased = biased)),
    class = c("DSC_Sample","DSC_Micro","DSC_R","DSC"))


Sample <- setRefClass("Sample", 
  fields = list(
    k	= "integer",
    biased = "logical",
    stream_size	= "integer",
    centers	= "data.frame"
  ), 
  
  methods = list(
    initialize = function(
      k	= 100L,
      biased = FALSE
    ) {
      
      k	<<- as.integer(k)
      biased	<<- biased
      stream_size	<<- 0L 
      
      .self
    }
    
  ),
)

### Reservoir sampling: 
### unbiased: all values in the stream have the same prob. to be sampled
### biased: more recent values have a higher probability
Sample$methods(
  cluster = function(x, ...) {
    
    
    if(!biased) {
      ### fast initialization
      if(nrow(centers) == 0L) {
        if(nrow(x) <= k) centers <<- x
        else centers <<- x[sample(1:nrow(x), k),]
        stream_size <<- nrow(x)
      }else{
        
        ### reservoir sampling
        for(i in 1:nrow(x)){
          ### fill with first values
          if(nrow(centers) < k) {
            centers <<- rbind(centers, x[i,])
            
          }else{ ### replace values with decreasing probabilities
            r <- sample.int(stream_size+1L, size=1L)
            if(r <= k) centers[r, ] <<- x[i,]
            ### Note: we do not need to replace weight (is already 1)
          }	 
          
          stream_size <<- stream_size + 1L
        }
      }
      
    }else{ ### biased
      for(i in 1:nrow(x)){
        ### add all new points and replace point in reservoir with prob=size/k  
        prob <- nrow(centers)/k
        if(sample(c(TRUE, FALSE), 1L, prob=c(prob, 1-prob))) {
          centers[sample.int(nrow(centers), 1L),] <<- x[i,]
        }else{
          centers <<- rbind(centers, x[i,])
        }
        
        stream_size <<- stream_size + 1L
      }
    }
  },
  
  get_microclusters = function(...) { centers },
  get_microweights = function(...) { rep(1, nrow(centers)) }
)




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


Sample <- setRefClass("Sample", 
	fields = list(
		k		= "integer",
		stream_size	= "integer",

		centers		= "data.frame"
	), 

	methods = list(
		initialize = function(
			k	= 100L
			) {
		    
		    k		<<- k
		    stream_size	<<- 0L 
		    
		    .self
		}

	),
)

### Reservoir sampling: all values in the stream have the same prob. to
### be sampled
Sample $methods(cluster = function(x, ...) {
	    
	    ### fill with first values
	    if(nrow(centers) < k) {
		centers <<- rbind(centers,x)

	    }else{ ### replace values with decreasing probabilities
		r <- as.integer(runif(1, min=1, max=stream_size+1))
		if(r < k) centers[r, ] <<- x
	    }	 

	    stream_size <<- stream_size + 1L
	}
	)

   
DSC_Sample <- function(k = 100) {

    sample <- Sample$new(k = as.integer(k))


    l <- list(description = "Sample",
	    RObj = sample)

    class(l) <- c("DSC_Sample","DSC_Micro","DSC_R","DSC")
    l
}



### get centers
get_microclusters.DSC_Sample <- function(x, ...) {
    x$RObj$centers
}

get_microweights.DSC_Sample <- function(x, ...) {
    rep(1, nclusters(x))
}

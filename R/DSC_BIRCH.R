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


BIRCH <- setRefClass("BIRCH", 
	fields = list(
		data 		= "data.frame",
		weights 	= "numeric",
		BIRCH	    = "ANY",
		radius	    = "numeric",
		compact	    = "numeric",
		keeptree    = "logical"
#		assignment	= "numeric"
		), 

	methods = list(
		initialize = function(keeptree = FALSE, radius, compact) {

		    data <<- data.frame()
#		    assignment <<- numeric()
		    weights <<- numeric()
		    BIRCH <<- NULL
		    keeptree <<- keeptree 
		    radius <<- radius
		    compact <<- compact

		    .self
		},
		finalize = function() {
		    ### Seems to be not necessary!
		    #    if(!is.null(BIRCH)) birch::birch.killTree(BIRCH)
		},

		cluster = function(points,  weight = rep(1,nrow(points)), ...) {
		    if(any(is.na(points))) {
			warning("BIRCH: Throwing out point with NA.")
		    } else {
			data <<- rbind(data,points)
			weights <<- c(weights,weight)
			if(is.null(BIRCH)) {
			    BIRCH <<- birch::birch.getTree(birch::birch(data.matrix(points), 
				    radius, compact=compact, keeptree=keeptree, 
				    columns=columns))
			} else {
			    birch::birch.addToTree(data.matrix(points), BIRCH)
			    BIRCH <<- birch::birch.getTree(BIRCH)
			}

#			for(i in 1:length(BIRCH$members)) 
#			    assignment[BIRCH$members[[i]]] <<- i

		    }
		}
		)
	)

### creator    
DSC_BIRCH <- function(radius, compact=radius, keeptree = TRUE) {

    l <- list(description = "BIRCH",
	    RObj = BIRCH$new(keeptree = keeptree, 
		    radius=radius, compact=compact)
	    )

    class(l) <- c("DSC_BIRCH","DSC_Micro","DSC_R","DSC")
    l
}

### get centers, etc.
get_microclusters.DSC_BIRCH <- function(x) {
    centers <- x$RObj$BIRCH$sumXi/x$RObj$BIRCH$N
    as.data.frame(centers)
}

get_microweights.DSC_BIRCH <- function(x) {
    weight <- x$RObj$BIRCH$N
    weight
}

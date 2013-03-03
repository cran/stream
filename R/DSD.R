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


## DSD - Data Stream Data interface
## all DSD classes have these functions
## and an additional function to create the DSD
##
## A new DSD class (e.g., myDSD) needs the following:
## 1. a constructor function. myDSD <- function(PARAMETERS) which
## returns an object with the class  c("DSD_myDSD","DSD_R","DSD")
## 2. get_points.myDSD <- function(x, n=1, ...)
##
## See DSD_Gaussian_Static.R for an example

get_points <- function(x, n=1, ...) UseMethod("get_points")
get_points.default <- function(x, n=1, ...) {
    stop(gettextf("get_points not implemented for class '%s'.",
		    paste(class(x), collapse=", ")))
}

### in case the stream can be reset (e.g., a stream from a file)
reset_stream <- function(dsd) UseMethod("reset_stream")
reset_stream.DSD <- function(dsd) {
    stop(gettextf("reset_stream not implemented for class '%s'.",
		    paste(class(dsd), collapse=", ")))
}


### end of interface
#############################################################
### helper
print.DSD <- function(x, ...) {
    cat(paste(x$description, " (", paste(class(x), collapse=", "), ")", 
		    '\n', sep=""))
    cat(paste('With', x$k, 'clusters', 'in', x$d, 'dimensions', '\n'))
}

plot.DSD <- function(x, n = 500, col= NULL, pch= NULL, 
	..., method="pairs") {
    ## method can be pairs, plot or pc (projection with PCA)
    d <- get_points(x, n, assignment = TRUE)
   
    ### make sure to plot noise
    assignment <- attr(d,"assignment")
   
    if(is.null(col)) {
	col <- as.integer(assignment)
	col[assignment==0 | is.na(assignment)] <-  which(palette()=="gray")
    } 
 
    if(is.null(pch)) {
	pch <- rep(1, n)
	pch[assignment==0 | is.na(assignment)] <- 3L
    }
    
    if(ncol(d)>2 && method=="pairs") {
   		pairs(d, col=col, pch=pch, ...)
    }
    else if(ncol(d)>2 && method=="pc") {
		## we assume Euclidean here
		p <- prcomp(d)
		plot(p$x, col=col, pch=pch, ...)
    } else {
		plot(d[,1:2],col=col, pch=pch, ...)
    }
}

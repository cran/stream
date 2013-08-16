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


### DSC - Data Stream Clusterer interface

### all DSC classes have these interface methods

get_centers <- function(x, type = c("auto", "micro", "macro"), ...) 
    UseMethod("get_centers")
get_centers.default <- function(x, type = c("auto", "micro", "macro"), ...) {
    stop(gettextf("get_centers not implemented for class '%s'.",
		                        paste(class(x), collapse=", ")))
}

### get MC weights. In case it is not implemented it returns 1s
get_weights <- function(x, type=c("auto", "micro", "macro"), scale=NULL, ...) 
    UseMethod("get_weights")
get_weights.default <- function(x, type=c("auto", "micro", "macro"), 
	scale=NULL, ...) {
    m <- rep(1,nclusters(x, type=type))
    if(!is.null(scale)) {
	if(length(unique(m)) ==1)  w <- rep(mean(scale), length(w))
	else m <- map(m, range=scale, from.range=c(0, 
			max(m, na.rm=TRUE)))
    }
    m
}

### End of interface
#####################################################################3

### make a deep copy of the 
get_copy <- function(x) UseMethod("get_copy")
get_copy.default <- function(x, ...) {
    stop(gettextf("get_copy not implemented for class '%s'.",
		    paste(class(x), collapse=", ")))
}

get_microclusters <- function(x) UseMethod("get_microclusters")
get_microclusters.DSC <- function(x) {
    stop(gettextf("No micro-clusters available for class '%s'.",
		    paste(class(x), collapse=", ")))
}

get_macroclusters <- function(x) UseMethod("get_macroclusters")
get_macroclusters.DSC <- function(x) {
    stop(gettextf("No macro-clusters available for class '%s'.",
		    paste(class(x), collapse=", ")))
}

get_microweights <- function(x) UseMethod("get_microweights")
get_microweights.DSC <- function(x) {
    stop(gettextf("No weights for micro-clusters available for class '%s'.",
		    paste(class(x), collapse=", ")))
}

get_macroweights <- function(x) UseMethod("get_macroweights")
get_macroweights.DSC <- function(x) {
    stop(gettextf("No weights for macro-clusters available for class '%s'.",
		    paste(class(x), collapse=", ")))
}


### derived functions, plot and print
nclusters <- function(x, type=c("auto", "micro", "macro"), ...) 
    UseMethod("nclusters")
nclusters.DSC <- function(x, type=c("auto", "micro", "macro"), ...) {
    nrow(get_centers(x, type=type))
}

get_assignment <- function(dsc, points, type=c("auto", "micro", "macro"), ...) 
    UseMethod("get_assignment")
get_assignment.DSC <- function(dsc, points, type=c("auto", "micro", "macro"), 
	...) {
    d <- points
    
    c <- get_centers(dsc, type=type)
    
    if(nrow(c)>0) {
	dist <- dist(d,c)
	#Find the minimum distance and save the class
	predict <- apply(dist, 1, which.min)
    } else {
	warning("There are no clusters!")
	predict <- rep(1L, nrow(d))
    }
    predict	
}

print.DSC <- function(x, ...) {
    cat(paste(x$description, " (", paste(class(x), collapse=", "), ")", 
		    '\n', sep=""))
    if(!is(nc <- try(nclusters(x, type="micro"), silent=TRUE), "try-error")) 
	cat(paste('Number of micro-clusters:', nc, '\n'))
    if(!is(nc <- try(nclusters(x, type="macro"), silent=TRUE), "try-error")) 
	cat(paste('Number of macro-clusters:', nc, '\n'))
}


#plot.DSC will call super question.
plot.DSC <- function(x, dsd = NULL, n = 500, 
	col_points="gray",  
	col_clusters="red", 
	weights=TRUE,
	scale=c(1,5),
	cex =1,
	pch=NULL,
	..., 
	method="pairs", 
	type=c("auto", "micro", "macro")) {

    ## method can be pairs, plot or pc (projection with PCA)
    k <- nclusters(x, type=type)
    
    if(k<1) {
      warning("No clusters, no plot produced!")
      return()
    }
    
    centers <- get_centers(x, type=type)
    if(weights) cex_clusters <- get_weights(x, type=type, scale=scale)
    else cex_clusters <- rep(cex, k)
	col <- rep(col_clusters, k)
    mpch <- rep(1, k)

    ### prepend data if given
    if(!is.null(dsd)) {
	d <- get_points(dsd, n, assignment = TRUE)
  #	names(d) <- names(centers)
	# fix center names
  names(centers) <- names(d)
  centers <- rbind(d, centers)
  
  col <- c(rep(col_points,n)[1:n], col)
	cex_clusters <- c(rep(cex, n), cex_clusters)
	mpch <- c(attr(d, "assignment"), mpch)
	
	### handle noise
	noise <- is.na(mpch)
	mpch[noise] <- 20
	cex_clusters[noise] <- cex_clusters[noise]*.3

    }

    if(!is.null(pch)) mpch <- pch

    ### plot
    if(ncol(centers)>2 && method=="pairs") {
	    pairs(centers, col=col, cex=cex_clusters, pch=mpch, ...)
    }
    else if(ncol(centers)>2 && method=="pc") {
	## we assume Euclidean here
	p <- prcomp(centers)
	    plot(p$x, col=col, cex=cex_clusters, pch=mpch, ...)
    } else { ## plot first 2 dimensions
	    plot(centers[,1:2], col=col, cex=cex_clusters, pch=mpch, ...)
    }

}



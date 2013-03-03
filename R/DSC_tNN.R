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


tNN <- setRefClass("tNN",
	fields = list(
		### parameters (micro-clustering)
		r			= "numeric",
		measure			= "character",
		lambda			= "numeric",
		decay_interval		= "integer",
		### noise: min. weight for micro-clusters given as a 
		### percentile of the total weight of the clustering (i.e.,
		### noise% of the data points is considered noise)
		noise			= "numeric", 
		
		### used internally
		distFun			= "ANY",
		decay_factor		= "numeric",
		debug			= "logical",
		
		### data
		weights			= "numeric",
		total_weight		= "numeric",
		npoints			= "integer",
		centers			= "data.frame",
		relations 		= "hash",
		
		### Macro-clustering
		macro			= "logical",	# do macro?
		### alpha: intersection factor (area of the intersection)
		alpha			= "numeric",
		### minweights: min. weight for macro-clusters 	
		minweight		= "numeric",
		### k: number of macro-clusters (alternative to )
		k			= "numeric"
		),


	methods = list(
		initialize = function(
			r		= 0.1,
			k		= 0,
			lambda		= 1e-3,
			decay_interval  = 1000L,
			minweight	= 0.1,
			noise		= 0.01,
			alpha 		= 0.25,
			measure		= "Euclidean",
			macro		= TRUE
			) {

		    relations 		<<- hash()
		    r			<<- r
		    lambda		<<- lambda
		    decay_interval	<<- decay_interval
		    decay_factor	<<- 2^(-lambda*decay_interval)
		    minweight		<<- minweight
		    noise		<<- noise
		    alpha		<<- alpha
		    measure		<<- measure
		    macro		<<- macro

		    if(is.null(k))
			k		<<- 0
		    else
			k		<<- k

		    weights		<<- numeric()
		    total_weight	<<- 0
		    npoints		<<- 0L
		    centers		<<- data.frame()

		    distFun		<<- pr_DB[[measure]]

		    .self
		}

		),
	)


DSC_tNN <- function(r = 0.1, k=0, alpha = 0, minweight = 0, lambda = 1e-3, 
	decay_interval=1000L, noise = 0.01, measure = "Euclidean", macro = TRUE) {

    if(k==0 && alpha==0 && macro) {
	warning("You have to specify at least k or alpha! Using default alpha=.25 and minweight=0.1.")
	minweight <- 0.1
	alpha <- 0.25
    }

    tNN <- tNN$new(r, k, lambda, as.integer(decay_interval), 
	    minweight, noise, alpha, measure, macro)

    l <- list(description = "tNN", RObj = tNN)

    class(l) <- c("DSC_tNN", "DSC_Micro", "DSC_R", "DSC")

    l
}

tNN$methods(cluster = function(newdata, debug = FALSE) {
	    'Cluster new data.' ### online help
      
	    newdata <- as.data.frame(newdata)

	    if(debug) cat("Debug cluster for tNN!\n")

	    for(i in 1:nrow(newdata)) {
		npoints <<- npoints + 1L

		if(debug && !i%%100) cat("Processed",i,"points\n")

		### decay and remove clusters
		if(decay_factor<1 && !npoints%%decay_interval) {
		    #decrease weight for microclusters
		    weights <<- weights * decay_factor
		
		    total_weight <<- total_weight * decay_factor

		    weight_remove <- .5
		    remove <- which(weights <= weight_remove)

		    # we need the keys in the following
		    keys <- keys(relations)
		    
		    if(length(remove)>0) {
			### get mc names
			mcs <- rownames(centers)

			if(debug) cat("  - Removing clusters",
				paste(mcs[remove], collapse=", "), 
				"\n")

			#remove microclusters
			weights <<- weights[-remove]
			centers <<- centers[-remove,]

			#remove microclusters in relations
			removekeys_id <- c(
				grep(paste("^",mcs[remove],"-",
						sep="",collapse="|"), 
					keys, value = FALSE), 
				grep(paste("-",mcs[remove],"$"
						,sep="",collapse="|"), 
					keys, value = FALSE)
				)
			removekeys_id <- unique(removekeys_id)
			removekeys <- keys[removekeys_id]

			if(length(removekeys)>0) {
			    if(debug) cat("  - Removing relation (cluster)",
				    paste(removekeys, collapse=", "), 
				"\n")

			    del(removekeys, relations)
			}
		    }

		    ### decay and remove weak relations
		    if(macro) {  
			if(length(relations) > 0) {
			    keys <- keys(relations)
			    new_val <- values(relations) * decay_factor
			    values(relations) <<- new_val

			    removekeys <- keys[new_val <= weight_remove]

			    if(length(removekeys)>0) {
				if(debug) cat("  - Removing relation (relation)",
					paste(removekeys, collapse=", "), 
					"\n")

				del(removekeys, relations)
			    }


			}
		    }
		}

		### process new point
		point <- newdata[i,]
		mcs <- rownames(centers) ### names
		
		total_weight <<- total_weight +1


		if(nrow(centers)==0) {
		    #create first micro-cluster
		    weights <<- 1
		    centers <<- as.data.frame(point)
		    rownames(centers) <<- 1
		} else {
		    inside <- which(dist(point,centers,method=distFun)<r)


		    if(length(inside)<1) { ### new cluster
			weights <<- c(weights,1)

			centers <<- rbind(centers,point)
			rownames(centers)[nrow(centers)] <<-
			as.integer(rownames(centers)[nrow(centers)-1])+1L

			if(debug) cat("  + Creating Cluster",
				rownames(centers)[nrow(centers)], "\n")

		    }else{ ### update existing cluster

			partialweight <- 1/length(inside) 

			newCenters <- data.frame(matrix((as.numeric(as.matrix(
								centers[inside,])*rep(weights[inside])+rep(as.numeric(point)*partialweight,each=length(inside))))/rep(partialweight+weights[inside],ncol(point)),
					ncol=ncol(point)),
				row.names=rownames(centers[inside,]))

			distance <- dist(newCenters,method=distFun)

			test <- apply(distance,1,function(x){all(x>r|x==0)})
			if(length(which(test)) > 0) {
			    centers[inside[which(test)],] <<- newCenters[which(test),]
			}

			weights[inside] <<- weights[inside] + partialweight

			if(macro && length(inside)>1) {
			    relationUpdate <- outer(mcs[inside], mcs[inside], 
				    function(x,y){paste(x,y,sep="-")})
			    relationUpdate <- relationUpdate[upper.tri(relationUpdate)]
			
			    if(length(relationUpdate)>0){
				if(debug) cat("  + Updating/Create Relations",
					paste(relationUpdate, collapse=", "), "\n")

				for(rel in relationUpdate) {
				    count <- relations[[rel]]
				    if(is.null(count)) count <- 1
				    else count <- count +1
					relations[[rel]] <<- count
				}
			    }

			}
		    }
		}	   
	    }
	}
	)
    
get_microclusters.DSC_tNN <- function(x) {
    ### we have to rename the micro-clusters
    mc <- x$RObj$centers
    if(nrow(mc)<1) return(data.frame())
    
    mc <- mc[strong_mcs(x),]

    rownames(mc) <- NULL
    mc
}

get_microweights.DSC_tNN <- function(x) {
    x$RObj$weights[strong_mcs(x)]
}



get_macroclusters.DSC_tNN <- function(x) {
    if(!x$RObj$macro) stop("No macro-clusters available!")
    
    mw <-  get_membership_weights(x)
    assignment <- mw$assignment
    weights <- mw$weight
    uniqueassign <- na.omit(unique(assignment))
    
    if(length(uniqueassign) <1) return(data.frame())
    
    mcs <- get_centers(x, type="micro")
    mcw <- get_weights(x, type="micro")

    ### find weighted centroids
    as.data.frame(t(sapply(uniqueassign, FUN=function(i) {
		take <- which(assignment==i)
		colSums(mcs[take,]*mcw[take])/sum(mcw[take])	
	    })))
}

get_macroweights.DSC_tNN <- function(x) {
    if(!x$RObj$macro) stop("No macro-clusters available!")
    get_membership_weights(x)$weight
}


microToMacro.DSC_tNN <- function(x, micro=NULL) {
    if(is.null(micro)) micro <- 1:nclusters(x, type="micro")
    mw <- get_membership_weights(x)
   
    structure(mw$assignment[micro], names=micro)
}


### tNN cannot recluster other clusterings!
recluster.tNN <- function(macro, dsc, type="auto", ...) {
    stop(gettextf("recluster not implemented for class '%s'.",
		    paste(class(macro), collapse=", ")))
}


###########################################################################
### helpers

# find strong MCs
strong_mcs <- function(x, weak=FALSE) {
    o <- order(x$RObj$weights, decreasing=FALSE)
    
    # first element represents weight of already deleted MCs!
    cs <- cumsum(c(x$RObj$total_weight-sum(x$RObj$weights), x$RObj$weights[o]))

    if(weak)
	o[(cs < x$RObj$total_weight*x$RObj$noise)[-1]]
    else	
	o[(cs >= x$RObj$total_weight*x$RObj$noise)[-1]]
}


### FIXME: this is not exported yet
get_connectivity <- function(dsc, matrix=FALSE) {
    mc_weights <- dsc$RObj$weights
    mcs <- rownames(dsc$RObj$centers)

    rel <- t(sapply(strsplit(keys(dsc$RObj$relations), "-"), as.integer))
    rel <-  matrix(match(rel, mcs), ncol=2) ### translate from names to index
    val <- values(dsc$RObj$relations)
    
    if(nrow(rel) <1) return(matrix(nrow=0, ncol=0))
   
    avg_weight <- apply(rel, MARGIN=1, FUN= function(x) mean(mc_weights[x]))

    ### similarity
    ss <- val/avg_weight
    ### create a distance
    
    ### unconnected is 2 times the largest distance
    s <- matrix(0, ncol=length(mcs), nrow=length(mcs))

    for(i in 1:nrow(rel)) {
	s[rel[i,1], rel[i,2]] <- ss[i]
	s[rel[i,2], rel[i,1]] <- ss[i]
    }

    strong <- strong_mcs(dsc)
    s <- s[strong,strong]
    if(!matrix) s <- as.simil(s)
    s
}

get_membership_weights <- function(dsc) {
    s <- get_connectivity(dsc)

    if(nrow(s)<2) assignment <- 1:nclusters(dsc, type="micro")
    else if(dsc$RObj$alpha>0) { ### use alpha
	s[s < dsc$RObj$alpha] <- 0
	s[s>0] <- 1
	d <- 1-s
	assignment <- cutree(hclust(d, method="single"), h=.5)
    }else{ ### use k
	if(dsc$RObj$alpha<0) warning("You need to specify at leasy alpha or k!")
	d <- 1/(1+s)

	### FIXME: If k>number of connected components then components would
	###  be merged randomly! So we add for these the redular distance!
	
	d2 <- dist(get_centers(dsc, type="micro"), method=dsc$RObj$distFun) 
	unconnected <- d==1
	d[unconnected] <- d[unconnected] + d2[unconnected]

	assignment <- cutree(hclust(d, method="single"), k=dsc$RObj$k)
    }

    ### aggregate macro-cluster weights
    w <- get_weights(dsc, type="micro")
    w <- aggregate(w, by=list(assignment), FUN=sum)$x
	
    ### deal with k and minweight (only if alpha is given!)
    if(dsc$RObj$alpha>0) {
	if(dsc$RObj$k>0 && length(w) > dsc$RObj$k) {
	    take <- order(w, decreasing=TRUE)[1:dsc$RObj$k]
	    w <- w[take]
	    assignment <- match(assignment, take)
	}
	if(dsc$RObj$minweight>0) {
	    take <- which(w>=(dsc$RObj$minweight*sum(w)))
	    w <- w[take]
	    assignment <- match(assignment, take)
	}
    }

    return(list(assignment=assignment, weight=w))
}


### special plotting for DSC_tNN
### FIXME: only show edges that really are used
plot.DSC_tNN <- function(x, dsd = NULL, n = 1000,
	col_points="gray",
	col_clusters="red",
	weights=TRUE,
	scale=c(1,5),
	cex =1,
	pch=NULL,
	...,
	method="pairs",
	type=c("auto", "micro", "macro")) {
	
    NextMethod()


    if(x$RObj$macro && type %in% c("macro")
		&& (ncol(x$RObj$centers)<=2 || method=="plot")) {

	    p <- get_centers(x, type="micro")

	    if(nrow(p)>0) {
		points(p, col="black")

		### add threshold circles
		for(i in 1:nrow(p)){
		    lines(ellipsePoints(x$RObj$r, x$RObj$r, 
				    loc=as.numeric(p[i,]), n=60),
			    col = "black", lty=3)
		}

		### add edges connecting macro-clusters
		s <- get_connectivity(x, matrix=TRUE)
		s[lower.tri(s)] <- NA
		edges <- which(s>0, arr.ind=TRUE)
		
		if(length(edges)>0) { # length instead of nrow (s can be empty!)
		    edges <- cbind(edges, 
			    w=apply(edges, MARGIN=1, FUN=function(ij) s[ij[1], ij[2]]))

		    edges <- cbind(edges, stream:::map(edges[,3], range=c(1,5)))

		    for(i in 1:nrow(edges)){
			lines(rbind(p[edges[i,1],],p[edges[i,2],]),
				col="black",lwd=edges[i,4])
		    }
		}

	    }
	}

    }

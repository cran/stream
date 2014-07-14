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


DSC_tNN <- function(r, lambda = 1e-3,  gap_time=1000L, noise = 0.1, 
  measure = "Euclidean", 
  shared_density = FALSE, alpha = 0.1, k = 0, minweight = 0) {
  
  tNN <- tNN$new(r, lambda, as.integer(gap_time), 
    noise, measure, shared_density, alpha, k, minweight)
  
  macro <- new.env()
  macro$newdata <- TRUE
  
  structure(
    list(
      description = "Threshold nearest-neighbor (tNN)", 
      RObj = tNN,
      macro = macro
    ), class = c("DSC_tNN", "DSC_Micro", "DSC_R", "DSC")
  )
}



### fast euclidean (currently not used!)
.inside <- function(x, centers, r) {
  which(rowSums(sapply(1:2, FUN=function(i) (centers[[i]]- x[[i]])^2)) < r^2)
}

tNN <- setRefClass("tNN",
  fields = list(
    ### parameters (micro-clustering)
    r			= "numeric",
    measure			= "character",
    lambda			= "numeric",
    gap_time		= "integer",
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
    t			      = "integer",
    centers			= "data.frame",
    last_update  = "integer",
    relations 		= "hash",
    
    ### Macro-clustering
    shared_density		= "logical",
    ### alpha: intersection factor (area of the intersection)
    alpha			= "numeric",
    ### k: number of macro-clusters (alternative to alpha)
    k			= "integer",
    ### minweights: min. weight for macro-clusters 	
    minweight		= "numeric"
  ),
  
  
  methods = list(
    initialize = function(
      r		= 0.1,
      lambda		= 1e-3,
      gap_time  = 1000L,
      noise		= 0.1,
      measure		= "Euclidean",
      shared_density		= FALSE,
      alpha 		= 0.1,
      k		= 0,
      minweight	= 0
    ) {
      
      if(alpha <0 || alpha>1) stop("alpha needs to be in [0,1]")
      if(noise <0 || noise>1) stop("noise needs to be in [0,1]")
      if(lambda <0 || lambda>1) stop("lambda needs to be in [0,1]")
      if(minweight <0 ||minweight>1) stop("minweight needs to be in [0,1]")
      
      gap_time <<- as.integer(gap_time)
      if(gap_time<1) stop("gap_time needs to be 1, 2, 3,...")
      
      relations 		<<- hash()
      r			<<- r
      lambda		<<- lambda
      decay_factor	<<- 2^(-lambda)
      noise		<<- noise
      measure		<<- measure
      shared_density		<<- shared_density
      alpha		<<- alpha
      minweight		<<- minweight
      
      if(is.null(k))
        k		<<- 0L
      else
        k		<<- as.integer(k)
      
      weights		<<- numeric()
      t		<<- 0L
      centers		<<- data.frame()
      
      distFun		<<- pr_DB[[measure]]
      
      .self
    }
    
  ),
)


tNN$methods(list(
  
  cluster = function(newdata, debug = FALSE) {
    'Cluster new data.' ### online help
    
    newdata <- as.data.frame(newdata)
    
    if(debug) cat("Debug clustering for tNN!\n")
    
    for(i in 1:nrow(newdata)) {
      t <<- t + 1L
      
      if(debug && !i%%100) cat("Processed",i,"points\n")
      
      ### decay and remove clusters
      if(decay_factor<1 && !t%%gap_time) {
        ### remove clusters
        w_min <- decay_factor^gap_time
        remove <- which(get_current_weights() <= w_min)
        
        if(debug) cat("  - Removing", length(remove), "micro-clusters:",
          paste(remove, collapse=", "), "\n")
        
        if(length(remove)) {
          # remove relations
          if(shared_density) {  
            keys <- keys(relations)
            mcs <- rownames(centers)
            
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
              if(debug) cat("  - Removing", length(removekeys), "relations (cluster):",
                paste(removekeys, collapse=", "), 
                "\n")
              
              del(removekeys, relations)
            }
          }
          
          # remove weak relations
          if(length(relations) > 0) {
            keys <- keys(relations)
            vals <- values(relations)
            
            ### FIXME: does not work!
            ws <- vals[2,] * decay_factor^(t-vals[1,])
            #removekeys <- keys[ws <= w_min]
            removekeys <- keys[ws <= w_min/2]
            
            #removekeys <- keys[t-sapply(vals, "[", 1L)>gap_time]
            
            if(length(removekeys)>0) {
              if(debug) cat("  - Removing", length(removekeys), "relations (relation):",
                paste(removekeys, collapse=", "), 
                "\n")
              
              del(removekeys, relations)
            }
          }
          
          # remove microclusters
          weights <<- weights[-remove]
          last_update <<- last_update[-remove]
          centers <<- centers[-remove,]
        }
      }
      
      ### process new point
      point <- newdata[i,]
      mcs <- rownames(centers) ### names
      
      if(nrow(centers)<1) {   ### create first micro-cluster
        weights <<- 1
        centers <<- as.data.frame(point)
        rownames(centers) <<- 1
        last_update <<- t
        
        if(debug) cat("  + Creating micro-cluster with ID: 1\n")
        
      } else {
        inside <- which(dist(point, centers, method=distFun) < r)
        
        if(length(inside) < 1) { ### new cluster (cannot have shared density)
          weights <<- c(weights, 1)
          centers <<- rbind(centers,point)
          rownames(centers)[nrow(centers)] <<-
            as.integer(rownames(centers)[nrow(centers)-1]) + 1L
          last_update <<- c(last_update, t)
          
          if(debug) cat("  + Creating micro-cluster(s) with ID: ",
            rownames(centers)[nrow(centers)], "\n")
          
        }else{ ### update existing cluster
          
          ### for moving
          #partialweight <- weights[inside] %in% min(weights[inside]) * 10
          
          partialweight <- 1 
          
          #partialweight <- 1/length(inside) 
          
          #partialweight <- (1-dist(point, centers, method=distFun)[inside]/r)^2
          #partialweight <- partialweight/sum(partialweight)
          
          ### decay weights first
          ws <- weights[inside] * 
            decay_factor^(t-last_update[inside])
          
          newCenters <- data.frame(
            (as.matrix(centers[inside,]) * ws + 
                matrix(as.numeric(point), ncol=ncol(point), nrow=length(inside), 
                  byrow=TRUE) * partialweight) / (ws + partialweight),
            row.names=rownames(centers[inside,]))
          
          ### weight the clusters get
          #partialweight <- 1/length(inside) 
          partialweight <- rep.int(0, length(inside))
          partialweight[which.min(dist(point, centers[inside,], method=distFun))] <- 1
          
          ### update weights
          weights[inside] <<- ws + partialweight
          last_update[inside] <<- t
          
          ### check overlap
          distance <- dist(newCenters,method=distFun)
          
          test <- apply(distance, 1, function(x) all(x>r|x==0) )
          if(length(which(test)) > 0) 
            centers[inside[which(test)],] <<- newCenters[which(test),]
          
          ### shared density
          if(shared_density && length(inside) > 1) {
            relationUpdate <- outer(mcs[inside], mcs[inside], 
              function(x,y) paste(x,y,sep="-") )
            relationUpdate <- relationUpdate[upper.tri(relationUpdate)]
            
            if(length(relationUpdate) > 0){
              if(debug) cat("  + Updating/Create Relations",
                paste(relationUpdate, collapse=", "), "\n")
              
              partialweight <- 1/length(inside) 
              
              for(j in relationUpdate) {
                ### relation is c(last_update, count)
                rel <- relations[[j]]
                #                if(is.null(rel)) rel <- c(t, 1)
                if(is.null(rel)) rel <- c(t, partialweight)
                else rel <- c(t, 
                  #                  rel[2] * decay_factor^(t-rel[1]) + 1)
                  rel[2] * decay_factor^(t-rel[1]) + partialweight)
                relations[[j]] <<- rel
              }
            }
          }
        }
      }
    }
  },
  
  get_current_weights = function() {
    weights * decay_factor^(t-last_update) 
  },
  
  # find strong MCs
  strong_mcs = function(weak=FALSE) {
    
    ws <- get_current_weights()
    
    # without noise all are strong!
    if(noise==0) {
      if(weak) return(integer(0))
      else return(seq(1, length(ws)))
    }  
    
    o <- order(ws, decreasing=FALSE)
    # first element represents weight of already deleted MCs!
    # sum will approach 1/(1-decay_factor) 
    if(decay_factor<1) w_total <- (1-decay_factor^(t-1))/(1-decay_factor)
    else w_total <- npoints
    
    cs <- cumsum(c(w_total-sum(ws), ws[o]))
    
    if(weak)
      o[(cs < w_total*noise)[-1]]
    else	
      o[(cs >= w_total*noise)[-1]]
  },
  
  get_shared_density = function(matrix=FALSE) {
    
    if(!shared_density) stop("No shared density available (use shared_density=TRUE)!")
    
    mc_weights <- get_current_weights()
    mcs <- rownames(centers)
    
    rel <- t(sapply(strsplit(keys(relations), "-"), as.integer))
    rel <-  matrix(match(rel, mcs), ncol=2) ### translate from names to index
    
    s <- matrix(0, ncol=length(mcs), nrow=length(mcs))
    
    if(nrow(rel) > 0){
      ### fix decay
      vals <- values(relations)
      vals <- vals[2,] * decay_factor^(t-vals[1,])
      
      ### get shared density relative to the density in the participating clusters
      ### use average weight
      avg_weight <- apply(rel, MARGIN=1, FUN= function(x) mean(mc_weights[x]))
      ss <- vals/avg_weight
      
      ### use max weight
      #max_weight <- apply(rel, MARGIN=1, FUN= function(x) max(mc_weights[x]))
      #ss <- vals/max_weight
      
      ### unconnected is 2 times the largest distance
      for(i in 1:nrow(rel)) s[rel[i,2], rel[i,1]] <- s[rel[i,1], rel[i,2]] <- ss[i]
    }
    
    
    strong <- strong_mcs()
    s <- s[strong, strong]
    
    ### filter alpha    
    if(alpha>0) s[s < alpha] <- 0
    
    ### add lenght 2 paths
    #s <- s%*%s + s
    
    if(!matrix) s <- as.simil(s)
    s
  },
  
  
  get_microclusters = function(cluster_type=c("strong", "all"), ...) {
    cluster_type <- match.arg(cluster_type)
    
    mc <- centers
    if(cluster_type=="strong") mc <- mc[strong_mcs(),]
    rownames(mc) <- NULL
    
    if(nrow(mc)<1) return(data.frame())
    
    mc
  },
  
  get_microweights = function(cluster_type=c("strong", "all"), ...) {
    cluster_type <- match.arg(cluster_type)

    w <- get_current_weights()
    if(cluster_type=="strong") w <- w[strong_mcs()]
    w
  },
  
  get_macro_clustering = function() {
    mcs <- get_microclusters()
    w <- get_microweights()
    nclusters <- nrow(mcs)
    
    if(nclusters < 1L) 
      return(list(centers=data.frame(), microToMacro=integer(0L), weight=numeric(0L)))
    if(nclusters == 1L) return(list(centers=mcs, microToMacro=1L, weight=w[1L]))
    
    if(shared_density) { ### use shared density
      s <- get_shared_density()
      
      if(k > 0L) { ### use k
        d <- 1/(1+s)
        
        hc <- hclust(d, method="single")
        ### find connected components
        assignment <- cutree(hc, h=1-1e-9)
        
        ### not enought components?
        if(length(unique(assignment)) < k) assignment <- cutree(hc, k=k)
        
        ### only take the largest k...
        #if(length(unique(assignment)) > k) {
        #  order(table(assignment), decreasing=TRUE)[1:k]
        #}
        
        ### or join using distance
        ### FIXME: If k>number of connected components then components would
        ###  be merged randomly! So we add for these the redular distance!
        #d2 <- dist(mcs, method=distFun) 
        #unconnected <- d==1
        #d[unconnected] <- d[unconnected] + d2[unconnected]
        
      }else{ ### use alpha
        s[s>0] <- 1
        d <- 1-s
        assignment <- cutree(hclust(d, method="single"), h=.5)
      }
      
      
    }else{ ### use adjacent clusters overlap by alpha (packing factor)
      ### create a distance between 0 and inf 
      ### (<1 means reachable, i.e., assignment areas overlap)
      d_pos <- dist(mcs, method=distFun)/r -1
      
      ### alpha = 0 -> 1    reachability at r
      ### alpha = 1 -> 0     highest packing
      h <- 1-alpha
      assignment <- cutree(hclust(d_pos, method="single"), h=h)
      
      ### use k if we don't get enough components!
      if(length(unique(assignment)) < k) {
        assignment <- cutree(hclust(d_pos, method="single"), k=k)
      }
    }
    
    ### use minweight filtering of macro-clusters
    if(minweight>0) {
      w_macro <- aggregate(w, by=list(assignment), FUN=sum)$x
      take <- which(w_macro>=(minweight*sum(w_macro)))
      assignment <- match(assignment, take)
    }
    
    ### find centroids
    macro <- .centroids(mcs, w, assignment)
    macro$microToMacro <- assignment
    
    macro
  }
))



get_macroclusters.DSC_tNN <- function(x) {
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$centers
}

get_macroweights.DSC_tNN <- function(x) {
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$weights
}

microToMacro.DSC_tNN <- function(x, micro=NULL){
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  assignment <- x$macro$macro$microToMacro
  if(!is.null(micro)) assignment <- assignment[micro]
  assignment
}

get_shared_density <- function(x, matrix=FALSE) x$RObj$get_shared_density(matrix=matrix)

### special plotting for DSC_tNN
### FIXME: only show edges that really are used
plot.DSC_tNN <- function(x, dsd = NULL, n = 500,
  col_points="gray",
#  col_clusters=c("red", "blue"),
#  weights=TRUE,
#  scale=c(1,5),
#  cex =1,
#  pch=NULL,
#  ...,
  method="pairs",
  type=c("auto", "micro", "macro", "both", "none"),
  shared_density=FALSE, assignment=FALSE, ...) {
  
  type <- match.arg(type)
  
  if(type=="none") r <- plot(dsd, col=col_points, ...)
  #r <- NextMethod()
  else r <- plot.DSC(x=x, dsd=dsd, n=n, col_points=col_points, 
    method=method, type=type, ...)
  
  if(!shared_density && !assignment) return(invisible(r))
  
  p <- get_centers(x, type="micro")
  
  if(assignment) {
    ### add threshold circles
    if(!is.numeric(assignment)) assignment <- 3L
    if(nrow(p)>0) {
      points(p, col="black", pch=3L)
      for(i in 1:nrow(p)){
        lines(ellipsePoints(x$RObj$r, x$RObj$r, 
          loc=as.numeric(p[i,]), n=60),
          col = "black", lty=assignment)
      }
    }
  }
  
  if(shared_density) {  
    if(!x$RObj$shared_density) stop("No shared density available!")
    
    if(ncol(x$RObj$centers)>2 && method!="scatter") stop("Only available 
    to plot 2D data or the first 2 dimensions!")
    
    if(nrow(p)>0) {
      #points(p, col="black")
      ### add edges connecting macro-clusters
      s <- get_shared_density(x, matrix=TRUE)
      s[lower.tri(s)] <- NA
      
      edges <- which(s>x$RObj$alpha, arr.ind=TRUE)
      
      if(length(edges)>0) { # length instead of nrow (s can be empty!)
        edges <- cbind(edges, 
          w=apply(edges, MARGIN=1, FUN=function(ij) s[ij[1], ij[2]]))
        
        edges <- cbind(edges, map(edges[,3], range=c(.5,3)))
        
        for(i in 1:nrow(edges)){
          lines(rbind(p[edges[i,1],],p[edges[i,2],]),
            col="black",lwd=edges[i,4])
        }
      }   
    }
  }
}

get_assignment.DSC_tNN <- function(dsc, points, type=c("auto", "micro", "macro"), 
  method=c("auto", "model", "nn"), ...) {
  
  type <- match.arg(type)
  method<- match.arg(method)
  
  if(method=="auto") method <- "model"
  if(method!="model") return(NextMethod())
  
  c <- get_centers(dsc, type="micro", ...)
  
  if(nrow(c)>0L) {
    dist <- dist(points, c, method=dsc$RObj$measure)
    # Find the minimum distance and save the class
    assignment <- apply(dist, 1L, which.min)
    
    # dist>threshold means no assignment
    assignment[apply(dist, 1L, min) > dsc$RObj$r] <- NA_integer_
    
  } else {
    warning("There are no clusters!")
    assignment <- rep(NA_integer_, nrow(points))
  }
  
  if(type=="macro") assignment <- microToMacro(dsc, assignment)
  
  attr(assignment, "method") <- "model"
  assignment
}

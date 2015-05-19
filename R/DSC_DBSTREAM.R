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


DSC_DBSTREAM <- function(r, lambda = 1e-3,  gaptime=1000L, noise = 0.1, 
  shared_density = FALSE, alpha = 0.1, k = 0, minweight = 0) {
  
  dbstream <- dbstream$new(r, lambda, as.integer(gaptime), 
    noise, shared_density, alpha, k, minweight)
  
  macro <- new.env()
  macro$newdata <- TRUE
  
  structure(
    list(
      description = "DBSTREAM", 
      RObj = dbstream,
      macro = macro
    ), class = c("DSC_DBSTREAM", "DSC_Micro", "DSC_R", "DSC")
  )
}



dbstream <- setRefClass("dbstream",
  fields = list(
    ### parameters (micro-clustering)
    r			      = "numeric",
    lambda			= "numeric",
    gaptime		= "integer",
    ### noise: min. weight for micro-clusters given as a 
    ### percentile of the total weight of the clustering (i.e.,
    ### noise% of the data points is considered noise)
    noise			  = "numeric", 
    
    ### used internally
    decay_factor = "numeric",
    
    ### Macro-clustering
    shared_density		= "logical",
    ### alpha: intersection factor (area of the intersection)
    alpha			= "numeric",
    ### k: number of macro-clusters (alternative to alpha)
    k			= "integer",
    ### minweights: min. weight for macro-clusters 	
    minweight		= "numeric",
    
    ### micro-clusters
    micro = "ANY"
  ),
  
  
  methods = list(
    initialize = function(
      r		      = 0.1,
      lambda		= 1e-3,
      gaptime  = 1000L,
      noise		  = 0.1,
      shared_density		= FALSE,
      alpha 		= 0.1,
      k		      = 0,
      minweight	= 0
    ) {
      
      if(alpha <0 || alpha>1) stop("alpha needs to be in [0,1]")
      if(noise <0 || noise>1) stop("noise needs to be in [0,1]")
      if(lambda <0) stop("lambda needs to be >=0")
      if(minweight <0 ||minweight>1) stop("minweight needs to be in [0,1]")
      
      gaptime <<- as.integer(gaptime)
      if(gaptime<1) stop("gaptime needs to be 1, 2, 3,...")
      
      r			<<- r
      lambda		<<- lambda
      noise		<<- noise
      alpha		<<- alpha
      minweight		<<- minweight
      shared_density		<<- shared_density
      decay_factor	<<- 2^(-lambda)
      
      if(is.null(k)) k		<<- 0L
      else k		<<- as.integer(k)
      
      micro <<- new(DBSTREAM, r, decay_factor, gaptime, 
        shared_density, alpha)

      .self
    }
    
  )
)


dbstream$methods(list(
  # overload copy
  copy = function(...) {
    #callSuper(...)
    ### copy S4 object
    n <- dbstream$new(r, lambda, gaptime, noise, shared_density, alpha,
      k, minweight)
    
    ### copy Rcpp object  
    n$micro <- new(DBSTREAM, micro$serializeR())
    
    n  
  },
  
  cluster = function(newdata, debug = FALSE) {
    'Cluster new data.' ### online help
    
    micro$update(as.matrix(newdata), debug)
  },
  
  # find strong MCs
  strong_mcs = function(weak=FALSE) {
    
    ws <- micro$weights()
    
    # without noise all are strong!
    if(noise<=0) {
      if(weak) return(integer(0))
      else return(seq(1, length(ws)))
    }  
    
    o <- order(ws, decreasing=FALSE)
    # first element represents weight of already deleted MCs!
    # sum will approach 1/(1-decay_factor) 
    #if(decay_factor<1) w_total <- (1-decay_factor^(t-1))/(1-decay_factor)
    #else w_total <- npoints
    #cs <- cumsum(c(w_total-sum(ws), ws[o]))
    
    #if(weak)
    #  o[(cs < w_total*noise)[-1]]
    #else  
    #  o[(cs >= w_total*noise)[-1]]
    
    # we do double (or multiple) counting!!!
    #w_total <- sum(ws)
    #cs <- cumsum(c(w_total-sum(ws), ws[o]))
    
    # we do double counting, but not likely for noise!
    if(decay_factor<1) w_total <- (1-decay_factor^(micro$t+1))/(1-decay_factor)
    ### for t -> inf this becomes 1/(1-decay_factor)
    else w_total <- npoints
    
    
    ## FIXME: correct weight using S
    #sd <- rowSums(get_shared_density(use_alpha=FALSE))
    
    cs <- cumsum(ws[o])
    
    ### decay_factor takes care of previousely removed points
    ##wk <- cs < w_total * decay_factor^gaptime * noise
    wk <- cs + w_total * noise * decay_factor^gaptime  < w_total * noise
    if(weak) o[wk]
    else o[!wk]
  },
  
  get_shared_density = function(use_alpha=TRUE) {
    if(!shared_density) stop("No shared density available (use shared_density=TRUE)!")
   
    vals <- as.matrix(micro$getSharedDensity())
    ws <- micro$weights()
   
    avg_weights <- outer(ws, ws, FUN = function(x, y) (x+y)/2)
    ###avg_weights <- outer(ws, ws, FUN = function(x, y) max(x,y))
    s <- vals/avg_weights
    
    strong <- strong_mcs()
    s <- s[strong, strong]
      
    ### filter using alpha    
    if(is.logical(use_alpha)) {
      if(use_alpha) s[s < alpha] <- 0
    }else s[s < use_alpha] <- 0
    
    s
  },
  
  
  get_microclusters = function(cluster_type=c("strong", "all"), ...) {
    cluster_type <- match.arg(cluster_type)
    
    mc <- as.data.frame(micro$centers())
    
    if(cluster_type=="strong") mc <- mc[strong_mcs(),]
    rownames(mc) <- NULL
    
    
    if(nrow(mc)<1) return(data.frame())
    
    mc
  },
  
  get_microweights = function(cluster_type=c("strong", "all"), ...) {
    cluster_type <- match.arg(cluster_type)

    w <- micro$weights()
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
      
      if(k > 0L) { ### use k not alpha!
        s <- get_shared_density(use_alpha = FALSE)
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
        ###  be merged randomly! So we add for these the regular distance!
        #d2 <- dist(mcs, method=distFun) 
        #unconnected <- d==1
        #d[unconnected] <- d[unconnected] + d2[unconnected]
        
      }else{ ### use alpha and connected components
        s <- get_shared_density(use_alpha=TRUE)
        s[s>0] <- 1
        d <- as.dist(1-s)
        assignment <- cutree(hclust(d, method="single"), h=.5)
      }
      
      
    }else{ ### use adjacent clusters overlap by alpha (packing factor)
      ### create a distance between 0 and inf 
      ### (<1 means reachable, i.e., assignment areas overlap)
      d_pos <- dist(mcs, method="Euclidean")/r -1
      
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



get_macroclusters.DSC_DBSTREAM <- function(x) {
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$centers
}

get_macroweights.DSC_DBSTREAM <- function(x) {
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$weights
}

microToMacro.DSC_DBSTREAM <- function(x, micro=NULL){
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  assignment <- x$macro$macro$microToMacro
  if(!is.null(micro)) assignment <- assignment[micro]
  assignment
}

get_shared_density <- function(x, use_alpha=TRUE) 
  x$RObj$get_shared_density(use_alpha=use_alpha)


change_alpha <- function(x, alpha) {
  x$RObj$alpha <- alpha
  x$RObj$micro$alpha <- alpha
  x$macro$newdata <- TRUE ### so macro clustering is redone
}

### special plotting for DSC_DBSTREAM
### FIXME: only show edges that really are used
plot.DSC_DBSTREAM <- function(x, dsd = NULL, n = 500,
  col_points=NULL,
#  col_clusters=c("red", "blue"),
#  weights=TRUE,
#  scale=c(1,5),
#  cex =1,
#  pch=NULL,
#  ...,
  method="pairs",
  type=c("auto", "micro", "macro", "both", "none"),
  shared_density=FALSE, use_alpha=TRUE, assignment=FALSE, ...) {
  
  type <- match.arg(type)
  
  if(is.null(col_points)) col_points <- .points_col
  
  if(type=="none") r <- plot(dsd, col=col_points, n=n, ...)
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
    
    if(ncol(p)>2 && method!="scatter") stop("Only available 
    to plot 2D data or the first 2 dimensions!")
    
    if(nrow(p)>0) {
      #points(p, col="black")
      ### add edges connecting macro-clusters
      s <- x$RObj$get_shared_density(use_alpha=use_alpha)
      s[lower.tri(s)] <- NA
      
      edges <- which(s>x$RObj$alpha, arr.ind=TRUE)
      
      if(length(edges)>0) { # length instead of nrow (s can be empty!)
        edges <- cbind(edges, 
          w=apply(edges, MARGIN=1, FUN=function(ij) s[ij[1], ij[2]]))
        
        edges <- cbind(edges, map(edges[,3], range=c(1,4)))
        
        for(i in 1:nrow(edges)){
          lines(rbind(p[edges[i,1],],p[edges[i,2],]),
            col="black",lwd=edges[i,4])
        }
      }   
    }
  }
}

get_assignment.DSC_DBSTREAM <- function(dsc, points, type=c("auto", "micro", "macro"), 
  method=c("auto", "model", "nn"), ...) {
  
  type <- match.arg(type)
  method<- match.arg(method)
  
  if(method=="auto") method <- "model"
  if(method!="model") return(NextMethod())
  
  c <- get_centers(dsc, type="micro", ...)
  
  if(nrow(c)>0L) {
    dist <- dist(points, c, method="Euclidean")
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


if(FALSE) {
  set.seed(0)
  stream <- DSD_Gaussians(k=3, noise=0.05)
  stream <- DSD_Memory(stream, 1000000)
  
  tnn <- DSC_tNN(r=.1, noise=0.1, shared=TRUE)
  system.time(update(tnn, stream, 1000))
  reset_stream(stream)
  plot(tnn, type="both", stream, assign=T, shared=T, xlim=c(0,1), ylim=c(0,1))
  
  reset_stream(stream)
  db <- DSC_DBSTREAM(r=.1, noise=0.1, shared=T)
  system.time(update(db, stream, 1000000))
  reset_stream(stream)
  plot(db, type="both", stream, assign=T, shared=T, xlim=c(0,1), ylim=c(0,1))
  }

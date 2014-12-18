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

DSC_DStream <- function(gridsize, d=NA_integer_, lambda = 1e-3, 
  gaptime=1000L, Cm=3, Cl=.8, attraction=FALSE, epsilon=.3, 
  Cm2=Cm, k=NULL) {
  
  dstream <- DStream$new(gridsize, as.integer(d), lambda, 
    as.integer(gaptime), Cm, Cl, as.logical(attraction), epsilon, Cm2, k)
  
  l <- list(
    description = "D-Stream", 
    RObj = dstream,
    macro = new.env()
  )
  
  l$macro$macro <- list(centers=data.frame(), 
    weights=numeric(0), microToMacro=integer(0))
  l$macro$newdata <- FALSE
  
  class(l) <- c("DSC_DStream", "DSC_Micro", "DSC_R", "DSC")
  l
}

DStream <- setRefClass("DStream",
  fields = list(
    ### this is a vector of length d (size of a grid cell)
    gridsize		    = "numeric",
    ### dimensionality
    d	              = "integer",
    ### decay (note lambda is different than in the paper!)
    lambda			    = "numeric",
    gaptime         = "integer",
    ### dense grid threshold Cm > 1 -> Dm = Cm/(N*(1-decay_factor))
    Cm              = "numeric",
    ### sparse grid threshold 0<Cl<1 -> Dl = Cl/(N*(1-decay_factor))
    Cl              = "numeric",
    ### other grid types
    ### transitional grid Cl < d < Cm
    ### sporadic grid pi = (Cl * (1-decay_factor))/(N*(1-decay_factor))
    
    ### attraction boundary (FIXME: Needs to be implemented)
    attraction      = "logical",
    epsilon		      = "numeric",
    Cm2		          = "numeric",
    k               = "integer",
    
    ### store the grid
    grid	 		      = "hash",
    npoints         = "integer",
    decay_factor		= "numeric",
    mins            = "numeric", ### we need mins and maxs to get N
    maxs            = "numeric"
  ),
  
  methods = list(
    initialize = function(
      gridsize = 0.1,
      d = NA_integer_,
      lambda = 1e-3,
      gaptime = 1000L,
      Cm = 3,
      Cl = .8,
      attraction = FALSE,
      epsilon = .3,
      Cm2 = 3,
      k = NULL
    ) {
      
      d  <<- d
      gridsize <<- gridsize
      lambda <<- lambda
      gaptime	<<- as.integer(gaptime)
      Cm <<- Cm
      Cl <<- Cl
      attraction <<- attraction
      epsilon <<- epsilon
      Cm2 <<- Cm2
      if(is.null(k)) k <<- 0L
      else k <<- as.integer(k)
      
      grid  <<- hash()
      npoints <<- 0L
      ### this is what the paper calls lambda!
      decay_factor <<- 2^(-lambda)
      mins <<- NA_real_
      maxs <<- NA_real_
      
      .self
    }
  )
)

DStream$methods(list(
  cluster = function(newdata, debug = FALSE) {
    'Cluster new data.' ### online help
    
    if(debug) cat("Debug cluster for DStream\n")
    
    newdata <- as.matrix(newdata)
    
    ### first data point (guess dimensions)
    if(is.na(d)) {
      d <<- ncol(newdata)
      if(length(gridsize) != d) gridsize <<- rep(gridsize[1], d)
      if(length(epsilon) != d) epsilon <<- rep(epsilon[1], d)
    }
    
    if(ncol(newdata) != d) stop("Dimensionality mismatch!")
    
    for(i in 1:nrow(newdata)) {
      point <- newdata[i, ]
      npoints <<- npoints + 1L
      
      ### remove sporadic grids
      if(decay_factor<1 && !npoints%%gaptime) {
        if(length(grid)>0) {
          ### remove sporadic grids
          N <- prod(maxs-mins+1L)
          
          remove <- sapply(values(grid, simplify=FALSE), FUN=function(gv) {
            gv[["weight"]]*decay_factor^(npoints-gv[["t"]]) < 
              Cl*(1-decay_factor^(npoints-gv[["t"]]+1)/N/(1-decay_factor))
          })
          
          if(debug) cat("Removing ", sum(remove) ," sporadic grids\n")
          for (k in keys(grid)[remove]) {
            grid[[k]] <<- NULL
          }
        } 
      }
      
      # find grid cell and insert point
      grid_id <- floor(point/gridsize)
      key <- paste(grid_id, collapse=":")
      
      ### new entry?
      val <- grid[[key]]
      if(is.null(val)) {
        val <- list(t=npoints, weight=1)
        if(attraction) val[["attr"]] <- numeric(d*2)
      } else {
        val[["weight"]] <- val[["weight"]] * decay_factor^(npoints-val[["t"]]) + 1
      }
      
      ### attraction
      if(attraction) {   
        cell_vol <- prod(gridsize)
        eps <- gridsize*epsilon
        cube_vol <- prod(eps)
        
        ### calculate the volume of the intersection of the cube (for the point)
        ### with the grid cells adjacent to grid_id
        get_center <- function(gid) gid*gridsize + gridsize/2 
        
        ### go over all adjacent cells
        ### rows are dimensions and cols are prev/next
        overlap <- matrix(0, ncol=2, nrow=d)
        
        if(attraction) {
          for(j in 1:d) {
            center <- get_center(grid_id)  
            ### previous cell
            o <- -point[j]+center[j] - (gridsize[j]/2-eps[j])
            if(o>0) overlap[j,1] <- o
            
            ### next cell
            o <- point[j]-center[j] - (gridsize[j]/2-eps[j])
            if(o>0) overlap[j,2] <- o
          }        
          
          if(any(overlap>0)) {
            ### calculate intersection hypercube
            overlap_max <- apply(overlap, MARGIN=1, max)  
            overlap2 <- overlap
            for(j in 1:d) {
              overlap2[j,] <- overlap2[j,]*prod(2*eps[-j]-overlap_max[-j])
            }
            
            ### overlap is vector with two entries by dimension (prev/next)
            overlap2 <- as.vector(overlap2/prod(2*eps))
            
            ### save attraction (including decay)
            if(is.null(val[["attr"]])) val[["attr"]] <- overlap2
            else val[["attr"]] <- val[["attr"]] * 
              decay_factor^(npoints-val[["t"]]) + overlap2
          }
        }
      }
      
      
      ### update t and data structure
      val[["t"]] <- npoints
      grid[[key]] <<- val
      
      ### update maxs/mins
      maxs <<- apply(cbind(maxs, grid_id), MARGIN=1, max, na.rm=TRUE)
      mins <<- apply(cbind(mins, grid_id), MARGIN=1, min, na.rm=TRUE)
      
      if(debug && !i%%100) cat("Processed",i,"points\n")
    }
  },
  
  ### This is for plotting images.
  toMatrix = function(grid_type="used", dim=NULL) {
    
    coords <- get_micro(weight=TRUE, translate=FALSE, grid_type=grid_type) 
    weights <- attr(coords, "weight")
    
    if(!is.null(dim)) coords <- coords[, dim]
    else coords <- coords[, 1:2]
    
    ns <- (maxs-mins)+1L
    mat <- matrix(0, nrow=ns[1], ncol=ns[2])
    
    ### coords can be negative!
    coords[,1] <- coords[,1] - mins[1]+1L
    coords[,2] <- coords[,2] - mins[2]+1L
    
    for(i in 1:nrow(coords)) {
      mat[coords[i,1], coords[i,2]] <- weights[i]
    }
    
    rownames(mat) <- (mins[1]:maxs[1]) * gridsize[1]+gridsize[1]/2
    colnames(mat) <- (mins[2]:maxs[2]) * gridsize[2]+gridsize[2]/2
    attr(mat, "varnames") <- colnames(coords)    
    
    mat
  },
  
  get_attraction = function(relative=FALSE, grid_type="dense") {
    
    if(!attraction) stop("No attraction values stored. Create the DSC_DStream with attraction=TRUE.")
    mc_ids <- get_micro(translate=FALSE, grid_type=grid_type)
    
    n <- nrow(mc_ids)
    attr_matrix <- matrix(0, ncol=n, nrow=n)
    
    ### not enough mcs for attraction
    if(n<2) {
      if(dist) return(as.dist(attr_matrix))
      else return(attr_matrix)
    }
    
    .findID <- function(id, ids) {
      which(apply(sapply(1:length(id), FUN=function(k) ids[,k]==id[k]), MARGIN=1, 
        FUN=function(z) {
          r <- z[1] 
          for(i in 1:length(z)) r <- r & z[i]
          r
        }
      ))
    }
    
    gv <- values(grid, simplify=FALSE)
    ids <- do.call(rbind, lapply(strsplit(names(gv), ":"), as.numeric))
    
    for(i in 1:length(gv)) {
      id <- ids[i,]
      vals <- matrix(gv[[i]][["attr"]], nrow=d, ncol=2)
      ### decay
      vals <- vals * decay_factor ^ (npoints - gv[[i]][["t"]]) 
      
      for(j in 1:d) {
        ### prev
        id2 <- id
        id2[j] <- id2[j] - 1L
        v <- vals[j,1]
        if(v>0) {
          attr_matrix[.findID(id, mc_ids), .findID(id2, mc_ids)] <- v
        }
        
        id2 <- id
        id2[j] <- id2[j] + 1L
        v <- vals[j,2]
        if(v>0) {
          attr_matrix[.findID(id, mc_ids), .findID(id2, mc_ids)] <- v
        }
      }
    }
    
    if(relative) {
      w <- attr(get_micro(weight=TRUE), "weight")
      attr_matrix <- attr_matrix/w
    }
    
    attr_matrix  
  },
  
  get_micro = function(weight=FALSE, cluster_type=FALSE, translate=TRUE, 
    grid_type=c("used", "dense", "transitional", "sparse", "all")) {
    
    grid_type <- match.arg(grid_type)
    
    if(length(grid)<1) {
      ret <- data.frame()
      if(weight) attr(ret, "weight") <- numeric(0)
      if(cluster_type) attr(ret, "cluster_type") <- factor(levels = 
          c("dense", "transitional", "sparse"))
      else return(ret)
    }
    
    ### raw coordinates
    coords <- as.data.frame(t(sapply(keys(grid), 
      FUN=function(y) as.numeric(unlist(strsplit(y, ':'))),
      USE.NAMES=FALSE)))
    
    N <- prod(maxs-mins+1L)
    gv <- values(grid, simplify=FALSE)
    
    ### add missing decay
    ws <- sapply(gv, FUN=function(g) {
      g[["weight"]] * decay_factor ^ (npoints - g[["t"]])
    }) 
    
    c_type <- factor(rep.int("sparse", times=length(ws)), 
      levels=c("dense", "transitional", "sparse"))
    c_type[ws > Cl/N * (1-decay_factor^(npoints+1L))/(1-decay_factor)] <- "transitional"
    c_type[ws > Cm/N * (1-decay_factor^(npoints+1L))/(1-decay_factor)] <- "dense"
    
    if(grid_type=="used") {
      # dense + adjacent transitional
      dense <- c_type=="dense"
      trans <- c_type=="transitional"
      used <- dense
      
      if(any(dense) && any(trans)) {
        take <- dist(coords[trans,], coords[dense,], method="Manhattan") <= 1
        take <- apply(take, 1L, any)
        used[which(trans)[take]] <- TRUE
      }
      
      coords <- coords[used,]
      ws <- ws[used]
      c_type <- c_type[used]
      
    } else if(grid_type=="all") {
      #nothing to do
    } else {
      take <- c_type==grid_type
      coords <- coords[take,]
      ws <- ws[take]
      c_type <- c_type[take]
    } 
    
    ### translate coordinates?
    if(translate && nrow(coords)>0) coords <- coords * gridsize+gridsize/2
    
    if(weight) attr(coords, "weight") <- ws
    if(cluster_type) attr(coords, "cluster_type") <- c_type 
    rownames(coords) <- NULL
    coords
  },
  
  get_microclusters = function(...) {  
    get_micro(...)
  },
  
  get_microweights = function(...){ 
    attr(get_micro(weight=TRUE, ...), "weight")
  },
  
  get_macro_clustering = function(...) {
    
    mcs <- get_micro(grid_type="used", translate=FALSE,
      weight=TRUE, cluster_type=TRUE)
    w <- attr(mcs, "weight")    
    c_type <- attr(mcs, "cluster_type")    
    
    ### no mcs
    if(nrow(mcs) < 1)
      return(list(centers=data.frame(), weights=numeric(0), microToMacro=integer(0)))
    
    ### single mc
    if(nrow(mcs) == 1)
      return(centers=mcs, weights=w, microToMacro=structure(1L, names="1"))
    
    denseID <- c_type=="dense"
    dense <- mcs[denseID,]
    transID <- c_type=="transitional"
    trans <- mcs[transID,]
    
    
    if(attraction) { ### use attraction
      a <- get_attraction(grid_type="dense")
      d_attr <- as.dist(-a-t(a))
      
      ### FIXME: transitional grids!
      if(k > 0L)  { ### use k?
        
        hc <- hclust(d_attr, method="single")
        ### find unconnected components
        assignment <- cutree(hc, h=0-1e-9)
        
        maxk <- min(k, nrow(mcs))
        ### not enought components?
        if(length(unique(assignment)) < maxk) assignment <- cutree(hc, k=maxk)
        
        ### FIXME: If k>number of connected components then components would
        ###  be merged randomly! So we add for these the regular distance!      
        
        #d_dist <- dist(mcs) 
        #unconnected <- d_attr==0 ### an attraction count of 0!
        #d_attr[unconnected] <- d_attr[unconnected] + d_dist[unconnected]
        #assignment <- cutree(hclust(d_attr, method="single"), k=k)
        
      }else{ ### use Cm2 
        
        P <- 2*sum(maxs-mins) ### number of possible attraction values
        ### actually we should check each direction independently
        assignment <- cutree(hclust(d_attr, method="single"), 
          h=-2*Cm2/P/(1+decay_factor))
      }
    }else{ ### use adjacency 
      ### FIXME: use dense and then assign transitional
      d_pos <- dist(dense)
      assignment <- cutree(hclust(d_pos, method="single"), 
        h=1.1) ### anything less than 2^.5 is fine
      
    }
    
    ### assign transitional grids
    if(nrow(trans)>0) {
      ass <- rep.int(NA_integer_, length(c_type))
      ass[denseID] <- assignment
      
      # this assigns it to one of the neighboring macro clusters
      take <- dist(trans, dense, method="Manhattan") <= 1
      take <- apply(take, 1L, FUN=function(x) which(x)[1])
      ass[transID] <- assignment[take]
      assignment <- ass
    }
    
    ### translate mcs
    mcs <- mcs*gridsize+gridsize/2
    
    m2m <-  structure(assignment, names=1:length(assignment))
    
    ### find centroids
    macro <- .centroids(mcs, w, m2m)
    macro$microToMacro <- m2m 
    
    macro
  }
)
)

get_attraction <- function(x, dist=FALSE, relative=FALSE) 
  x$RObj$get_attraction(dist=dist, relative=relative)

get_macroclusters.DSC_DStream <- function(x,...){
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering(...)
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$centers
}

get_macroweights.DSC_DStream <- function(x, ...) {
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering(...)
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$weights
}

microToMacro.DSC_DStream <- function(x, micro=NULL, ...) {
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  assignment <- x$macro$macro$microToMacro
  if(!is.null(micro)) assignment <- assignment[micro]
  assignment
}

### add plot as a grid
plot.DSC_DStream <- function(x, dsd=NULL, n=500, 
  type=c("micro", "macro", "both"), 
  grid=FALSE, grid_type="used", assignment=FALSE,
  ...) {
  
  ### find type
  dim <- list(...)$dim
  main <- list(...)$main
  
  ### grid uses a darker color for the points
  col_points <- list(...)$col_points
  if(is.null(col_points)) col_points <- gray(.1, alpha=.3)
  
  type <- match.arg(type)

  ### assignment == grid
  if(assignment) grid=TRUE
  
  ### implements grid and grid_both
  if(!grid) return(plot.DSC(x, dsd=dsd, n=n, type=type, ...))
  
  if(is.na(x$RObj$d)) {
    warning("No data clustered yet")
    return(invisible(NULL))
  }
  
  if(x$RObj$d!=2 && (is.null(dim) 
    || length(dim)!=2)) stop("Image visualization only works for 2D data! Set dim in plot.") 
  
  #  mat <- x$RObj$toMatrix("transitional")
  mat <- x$RObj$toMatrix(grid_type, dim)
  mat[mat==0] <- NA
  
  varnames <- attr(mat, "varnames")
  
  ### FIXME: this fails for a single grid!
  image(x=as.numeric(rownames(mat)), 
    y=as.numeric(colnames(mat)), 
    z=mat, 
    col=rev(gray.colors(100, alpha=1)), axes=TRUE, 
    xlab=varnames[1], ylab=varnames[2], main=main)
  
  if(!is.null(dsd)) {
    ps <- get_points(dsd, n=n, cluster=TRUE)
    pch <- attr(ps, "cluster")
    
    if(!is.null(dim)) ps <- ps[, dim]
    
    ### handle noise (samll circle)
    pch[is.na(pch)] <- .noise_pch
    points(ps, col= col_points, pch=pch)
  }
  
  ### add macro-clusters?
  if(type=="both" || type=="macro") {
    points(get_centers(x, type="macro"), col="blue", lwd=2, pch=3, 
      cex=get_weights(x, type="macro", scale=c(1,5)))
  }
}

get_assignment.DSC_DStream <- function(dsc, points, type=c("auto", "micro", "macro"), 
  method=c("auto", "model", "nn"), ...) {
  
  type <- match.arg(type)
  method<- match.arg(method)

  if(method=="auto") method <- "model"
  if(method!="model") return(NextMethod())
  
  c <- get_centers(dsc, type="micro", ...)
  
  if(nrow(c)>0L) {
    dist <- dist(points, c, method="max")
    # Find the minimum distance and save the class
    assignment <- apply(dist, 1L, which.min)
    
    # dist>threshold means no assignment
    assignment[apply(dist, 1L, min) > dsc$RObj$gridsize/2] <- NA_integer_
    
  } else {
    warning("There are no clusters!")
    assignment <- rep(NA_integer_, nrow(points))
  }
  
  if(type=="macro") assignment <- microToMacro(dsc, assignment)
  
  attr(assignment, "method") <- "model"
  
  assignment 
}

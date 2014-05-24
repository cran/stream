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


DSC_DStream <- function(gridsize, d=NA_integer_, lambda = 1e-3, 
  gaptime=1000L, Cm=3, Cl=.8, attraction=FALSE, epsilon=.3, 
  Cm2=Cm, k=NULL) {
  
  dstream <- DStream$new(gridsize, as.integer(d), lambda, 
    as.integer(gaptime), Cm, Cl, as.logical(attraction), epsilon, Cm2, k)
  
  l <- list(
    description = "DStream", 
    RObj = dstream,
    macro = new.env()
    )
  
  l$macro$macro <- list(centers=data.frame(), 
    weights=numeric(0), microToMacro=integer(0))
  l$macro$newdata <- FALSE
  
  class(l) <- c("DSC_DStream", "DSC_Micro", "DSC_R", "DSC")
  l
}

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
  
  ### This is for plotting images. Could be toArray!
  #  toMatrix = function(grid_type=c("transitional", "dense", "all")) {
  toMatrix = function(grid_type=c("dense", "transitional", "all")) {
    grid_type <- match.arg(grid_type)
    
    coords <- get_micro(weight=TRUE, translate=FALSE, grid_type=grid_type)
    
    ns <- (maxs-mins)+1L
    mat <- matrix(0, nrow=ns[1], ncol=ns[2])

    ### coords can be negative!
    coords[,1] <- coords[,1] - mins[1]+1L
    coords[,2] <- coords[,2] - mins[2]+1L
    
    for(i in 1:nrow(coords)) {
      mat[coords[i,1], coords[i,2]] <- coords[["weight"]][i]
    }
    
    rownames(mat) <- (mins[1]:maxs[1]) * gridsize[1]+gridsize[1]/2
    colnames(mat) <- (mins[2]:maxs[2]) * gridsize[2]+gridsize[2]/2
    mat
  },
  
  
  get_attraction = function(relative=FALSE, 
    #    grid_type="transitional") {
    grid_type="dense") {
    
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
      w <- get_micro(weight=TRUE)[["weight"]]
      attr_matrix <- attr_matrix/w
    }
    
    attr_matrix  
  },
  
  get_micro = function(weight=FALSE, translate=TRUE, 
    #   grid_type=c("transitional","dense", "all")) {
    grid_type=c("dense", "transitional", "all")) {
    grid_type <- match.arg(grid_type)
    
    if(length(grid)<1) {
      if(weight) return(data.frame(weight=numeric(0)))
      else return(data.frame())
    }
    
    if(translate) {
      coords <- as.data.frame(t(sapply(keys(grid), 
        FUN=function(y) as.numeric(unlist(strsplit(y, ':'))), 
        USE.NAMES=FALSE)*gridsize+gridsize/2))
    }else{
      coords <- as.data.frame(t(sapply(keys(grid), 
        FUN=function(y) as.numeric(unlist(strsplit(y, ':'))),
        USE.NAMES=FALSE)))
    }
    
    N <- prod(maxs-mins+1L)
    gv <- values(grid, simplify=FALSE)
    
    ### add missing decay
    ws <- sapply(gv, FUN=function(g) {
      g[["weight"]] * decay_factor ^ (npoints - g[["t"]])
    }) 
    
    if(grid_type=="transitional") {
      ### sparse grid threshold 0<Cl<1 -> Dl = Cl/(N*(1-decay_factor))
      #take <- ws > Cl/N/(1-decay_factor)
      ### for small t we use the exact total weight!
      take <- ws > Cl/N * (1-decay_factor^(npoints+1L))/(1-decay_factor)
      coords <- coords[take,]
      ws <- ws[take]
    } else if(grid_type=="dense") {
      ### dense grid threshold Cm > 1 -> Dm = Cm/(N*(1-decay_factor))
      ### for small t we use the exact total weight!
      #take <- ws > Cm/N/(1-decay_factor)
      take <- ws > Cm/N * (1-decay_factor^(npoints+1L))/(1-decay_factor)
      coords <- coords[take,]
      ws <- ws[take]
    }
    
    if(weight) coords[["weight"]] <- ws
    rownames(coords) <- NULL
    coords
  },
  
  microToMacro = function(micro=NULL, grid_type="dense") {
    
    mcs <- get_micro()
    if(nrow(mcs) < 1) return(integer(0)) ### no mcs
    
    if(nrow(mcs) == 1) { ### single mc
      assignment <- 1L
      names(assignment) <- rownames(mcs)
    } else{ 
      
      if(attraction) { ### use attraction
        
        if(k > 0L)  { ### use k?
          a <- get_attraction(grid_type=grid_type)
          d_attr <- as.dist(-a-t(a))
          
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
          a <- get_attraction(grid_type=grid_type)
          d_attr <- as.dist(-a-t(a))
          
          P <- 2*sum(maxs-mins) ### number of possible attraction values
          ### actually we should check each direction independently
          assignment <- cutree(hclust(d_attr, method="single"), 
            h=-2*Cm2/P/(1+decay_factor))
        }
      }else{ ### use adjacency 
        mcs <- get_micro(grid_type=grid_type)
        d_pos <- dist(mcs)
        assignment <- cutree(hclust(d_pos, method="single"), 
          h=gridsize[1]+gridsize[1]*1e-9)
      }
    }
    
    if(!is.null(micro)) assignment <- assignment[micro]
    else micro <- 1:length(assignment)
    
    structure(assignment, names=micro)
  },
  
  get_macro_clustering = function() {
    
    mcs <- get_micro(weight=TRUE)
    
    ### no mcs
    if(nrow(mcs) < 1)
      return(list(centers=data.frame(), weights=numeric(0), microToMacro=integer(0)))
    
    ### single mc
    if(nrow(mcs) == 1)
      return(centers=mcs, weights=mcs[,-ncol(mcs)], microToMacro=structure(1L, names="1"))
    
    ### general case
    mc <- mcs[,-ncol(mcs)]
    w <- mcs[,ncol(mcs)]
    m2m <- microToMacro(grid_type="dense")
    
    ### find centroids
    macro <- .centroids(mc, w, m2m)
    macro$microToMacro <- m2m 
    
    macro
  }
)
)

get_microclusters.DSC_DStream <- function(x, ...)  
  x$RObj$get_micro(weight=FALSE, ...)

get_microweights.DSC_DStream <- function(x, ...) 
  x$RObj$get_micro(weight=TRUE, ...)[["weight"]]

get_attraction <- function(x, dist=FALSE, relative=FALSE) 
  x$RObj$get_attraction(dist=dist, relative=relative)

get_macroclusters.DSC_DStream <- function(x, ...){
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$centers
}
  
get_macroweights.DSC_DStream <- function(x, ...) {
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
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
plot.DSC_DStream <- function(x, dsd=NULL, n=500, ...) {
  ### find type
  type <- list(...)$type
  
  if(is.null(type) || !pmatch(tolower(type), "grid", nomatch=0)) 
    return(plot.DSC(x, dsd=dsd, n=n, ...))

  if(is.na(x$RObj$d)) {
    warning("No data clustered yet")
    return(invisible(NULL))
  }
    
  if(x$RObj$d!=2) stop("Image visualization only works for 2D data!") 
  
  #  mat <- x$RObj$toMatrix("transitional")
  mat <- x$RObj$toMatrix("dense")
  mat[mat==0] <- NA
  
  image(x=as.numeric(rownames(mat)), 
    y=as.numeric(colnames(mat)), 
    z=mat, 
    col=rev(gray.colors(100)), axes=TRUE, 
    xlab="", ylab="")

  if(!is.null(dsd)) {
    ps <- get_points(dsd, n=n, assignment=TRUE)
    pch <- attr(ps, "assignment")
    
    ### handle noise (samll circle)
    pch[is.na(pch)] <- 20
    points(ps, col=rgb(0,0,0,alpha=.3), cex=.5, pch=pch)
  }
    
}


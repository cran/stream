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


#' DBSTREAM Clustering Algorithm
#'
#' Micro Clusterer with reclustering.
#' Implements a simple density-based stream clustering algorithm that assigns
#' data points to micro-clusters with a given radius and implements
#' shared-density-based reclustering.
#'
#' The DBSTREAM algorithm checks for each new data point in the incoming
#' stream, if it is below the threshold value of dissimilarity value of any
#' existing micro-clusters, and if so, merges the point with the micro-cluster.
#' Otherwise, a new micro-cluster is created to accommodate the new data point.
#'
#' Although DSC_DBSTREAM is a micro clustering algorithm, macro clusters and
#' weights are available.
#'
#' [update()]  invisibly return the assignment of the data points to clusters.
#' The columns are `.class` with the index of the strong micro-cluster and `.mc_id`
#' with the permanent id of the strong micro-cluster.
#'
#' `plot()` for DSC_DBSTREAM has two extra logical parameters called
#' `assignment` and `shared_density` which show the assignment area
#' and the shared density graph, respectively.
#'
#' [predict()] can be used to assign new points to clusters. Points are assigned to a micro-cluster if
#' they are within its assignment area (distance is less then `r` times `noise_multiplier`).
#'
#' `DSOutlier_DBSTREAM` classifies points as outlier/noise if they that cannot be assigned to a micro-cluster
#' representing a dense region as a outlier/noise. Parameter `outlier_multiplier` specifies
#' how far a point has to be away from a micro-cluster as a multiplier for the radius `r`.
#'  A larger value means that outliers have to be farther away from dense
#' regions and thus reduce the chance of misclassifying a regular point as an outlier.
#'
#' @aliases DSC_DBSTREAM DBSTREAM dbstream
#' @family DSC_Micro
#' @family DSC_TwoStage
#' @family DSOutlier
#'
#' @param formula `NULL` to use all features in the stream or a model [formula] of the form `~ X1 + X2`
#'   to specify the features used for clustering. Only `.`, `+` and `-` are currently
#'   supported in the formula.
#' @param r The radius of micro-clusters.
#' @param lambda The lambda used in the fading function.
#' @param gaptime weak micro-clusters (and weak shared density entries) are
#' removed every `gaptime` points.
#' @param Cm minimum weight for a micro-cluster.
#' @param metric metric used to calculate distances.
#' @param shared_density Record shared density information. If set to
#' `TRUE` then shared density is used for reclustering, otherwise
#' reachability is used (overlapping clusters with less than \eqn{r * (1 - alpha)}
#' distance are clustered together).
#' @param k The number of macro clusters to be returned if macro is true.
#' @param alpha For shared density: The minimum proportion of shared points
#' between to clusters to warrant combining them (a suitable value for 2D data
#' is .3).  For reachability clustering it is a distance factor.
#' @param minweight The proportion of the total weight a macro-cluster needs to
#' have not to be noise (between 0 and 1).
#' @param x A DSC_DBSTREAM object to get the shared density information from.
#' @param use_alpha only return shared density if it exceeds alpha.
#' @param noise_multiplier,outlier_multiplier multiplier for radius `r` to declare noise or outliers.
#' @param ...	further arguments are passed on to plot or pairs in graphics.
#'
#' @return An object of class `DSC_DBSTREAM` (subclass of [DSC],
#' [DSC_R], [DSC_Micro]).
#' @author Michael Hahsler and Matthew Bolanos
#' @references Michael Hahsler and Matthew Bolanos. Clustering data streams
#' based on shared density between micro-clusters. _IEEE Transactions on
#' Knowledge and Data Engineering,_ 28(6):1449--1461, June 2016
#' @examples
#' set.seed(1000)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#'
#' # create clusterer with r = .05
#' dbstream <- DSC_DBSTREAM(r = .05)
#' update(dbstream, stream, 500)
#' dbstream
#'
#' # check micro-clusters
#' nclusters(dbstream)
#' head(get_centers(dbstream))
#' plot(dbstream, stream)
#'
#' # plot micro-clusters with assignment area
#' plot(dbstream, stream, type = "none", assignment = TRUE)
#'
#'
#' # DBSTREAM with shared density
#' dbstream <- DSC_DBSTREAM(r = .05, shared_density = TRUE, Cm = 5)
#' update(dbstream, stream, 500)
#' dbstream
#'
#' plot(dbstream, stream)
#' # plot the shared density graph (several options)
#' plot(dbstream, stream, type = "micro", shared_density = TRUE)
#' plot(dbstream, stream, type = "none", shared_density = TRUE, assignment = TRUE)
#'
#' # see how micro and macro-clusters relate
#' # each micro-cluster has an entry with the macro-cluster id
#' # Note: unassigned micro-clusters (noise) have an NA
#' microToMacro(dbstream)
#'
#' # do some evaluation
#' evaluate_static(dbstream, stream, measure = "purity")
#' evaluate_static(dbstream, stream, measure = "cRand", type = "macro")
#'
#' # use DBSTREAM also returns the cluster assignment
#' # later retrieve the cluster assignments for each point)
#' data("iris")
#' dbstream <- DSC_DBSTREAM(r = 1)
#' cl <- update(dbstream, iris[,-5], return = "assignment")
#' dbstream
#'
#' head(cl)
#'
#' # micro-clusters
#' plot(iris[,-5], col = cl$.class, pch = cl$.class)
#'
#' # macro-clusters (2 clusters since reachability cannot separate two of the three species)
#' plot(iris[,-5], col = microToMacro(dbstream, cl$.class))
#'
#' # use DBSTREAM with a formula (cluster all variables but X2)
#' stream <- DSD_Gaussians(k = 3, d = 4, noise = 0.05)
#' dbstream <- DSC_DBSTREAM(formula = ~ . - X2, r = .2)
#'
#' update(dbstream, stream, 500)
#' get_centers(dbstream)
#'
#' # use DBSTREAM for outlier detection
#' stream <- DSD_Gaussians(k = 3, d = 4, noise = 0.05)
#' outlier_detector <- DSOutlier_DBSTREAM(r = .2)
#'
#' update(outlier_detector, stream, 500)
#' outlier_detector
#'
#' plot(outlier_detector, stream)
#'
#' points <- get_points(stream, 20)
#' points
#' which(is.na(predict(outlier_detector, points)))
#' @export
DSC_DBSTREAM <- function(formula = NULL,
  r,
  lambda = 1e-3,
  gaptime = 1000L,
  Cm = 3,
  metric = "Euclidean",
  noise_multiplier = 1,
  shared_density = FALSE,
  alpha = 0.1,
  k = 0,
  minweight = 0)
  structure(
    list(
      description = "DBSTREAM",
      ## this is the micro clusterer
      formula = formula,
      RObj = dbstream$new(
        r,
        lambda,
        as.integer(gaptime),
        Cm,
        shared_density,
        alpha,
        k,
        minweight,
        metric
      ),
      macro = DSC_Static(x = list(centers = data.frame()), type = "macro")
    ),
    class = c("DSC_DBSTREAM", "DSC_Micro", "DSC_R", "DSC")
  )


dbstream <- setRefClass(
  "dbstream",
  fields = list(
    ### parameters (micro-clustering)
    r			        = "numeric",
    lambda			  = "numeric",
    gaptime		    = "integer",
    Cm			      = "numeric",

    ### used internally
    decay_factor  = "numeric",

    ### Macro-clustering
    shared_density = "logical",
    ### alpha: intersection factor (area of the intersection)
    alpha			    = "numeric",
    ### k: number of macro-clusters (alternative to alpha)
    k			        = "integer",
    ### minweights: min. weight for macro-clusters
    minweight		  = "numeric",
    metric        = "integer",
    metric_name   = "character",

    ### micro-clusters
    CppObj        = "ANY",
    serial        = "ANY",

    ### column names for centers
    colnames      = "ANY",

    ### do we need to rerun the reclusterer
    newdata         = "logical"
  ),


  methods = list(
    initialize = function(r		      = 0.1,
      lambda		= 1e-3,
      gaptime   = 1000L,
      Cm		    = 3,
      shared_density = FALSE,
      alpha 		= 0.1,
      k		      = 0L,
      minweight	= 0,
      metric    = "Euclidean") {
      if (alpha < 0 || alpha > 1)
        stop("alpha needs to be in [0,1]")
      if (Cm < 0)
        stop("Cm needs to be in >=0")
      if (lambda < 0)
        stop("lambda needs to be >=0")
      if (minweight < 0 ||
          minweight > 1)
        stop("minweight needs to be in [0,1]")

      gaptime <<- as.integer(gaptime)
      if (gaptime < 1L)
        stop("gaptime needs to be 1, 2, 3,...")

      metrics <- c("euclidean", "manhattan", "maximum")
      m <- pmatch(tolower(metric), metrics) - 1L
      if (is.na(m))
        stop("Unknow metric! Available metrics: ",
          paste(metrics, collapse = ", "))
      metric <<- m
      metric_name <<- metrics[m + 1L]

      if (shared_density && m != 0L)
        stop("Shared density only works in Euclidean space!")


      r			  <<- r
      lambda	<<- lambda
      Cm		  <<- Cm
      alpha		<<- alpha
      minweight		    <<- minweight
      shared_density	<<- shared_density
      decay_factor	  <<- 2 ^ (-lambda)

      colnames  <<- NULL
      newdata  <<- TRUE

      if (is.null(k))
        k <<- 0L
      else
        k <<- as.integer(k)

      CppObj <<- new(DBSTREAM, r, decay_factor, gaptime,
        shared_density, alpha, m)

      .self
    }
  )
)

dbstream$methods(
  list(
    # overload copy
    copy = function(...) {
      #callSuper(...)
      ### copy S4 object
      n <- dbstream$new(r,
        lambda,
        gaptime,
        Cm,
        shared_density,
        alpha,
        k,
        minweight,
        metric)

      ### copy Rcpp object
      n$CppObj <- new(DBSTREAM, CppObj$serializeR())

      n
    },

    cache = function() {
      serial <<- CppObj$serializeR()
    },

    uncache = function() {
      CppObj <<- new(DBSTREAM, serial)
      serial <<- NULL
      newdata <<- TRUE
    },

    cluster = function(newdata,
      debug = FALSE,
      assignments = TRUE) {
      'Cluster new data.' ### online help

      newdata <<- TRUE

      if (is.null(colnames))
        colnames <<- colnames(newdata)

      assignment <-
        CppObj$update(as.matrix(newdata), debug, assignments)

      if (!assignments)
        return (NULL)

      ### remove weak MCS
      strong <- strong_mcs()
      ids <- attr(CppObj$centers(), "ids")
      assignment[assignment %in% ids[!strong]] <- NA
      ids <- ids[strong]
      ids <- match(assignment, ids)

      data.frame(.class = assignment, .mc_id = ids)
    },

    # find strong MCs
    strong_mcs = function(weak = FALSE) {
      ws <- CppObj$weights()

      # without noise all are strong!
      if (Cm <= 0) {
        if (weak)
          return(rep(FALSE, length.out = length(ws)))
        else
          return(rep(TRUE, length.out = length(ws)))
      }

      if (weak)
        ws < Cm
      else
        ws >= Cm
    },

    get_shared_density = function(use_alpha = TRUE) {
      if (!shared_density)
        stop("No shared density available (use shared_density=TRUE)!")

      vals <- as.matrix(CppObj$getSharedDensity())
      ws <- CppObj$weights()

      ## normalize weight (avg, min, max)
      norm_weights <- outer(
        ws,
        ws,
        FUN = function(x, y)
          (x + y) / 2
      )
      #norm_weights <- outer(ws, ws, FUN = function(x, y) pmax(x,y))
      #norm_weights <- outer(ws, ws, FUN = function(x, y) pmin(x,y))
      s <- vals / norm_weights



      strong <- strong_mcs()
      s <- s[strong, strong]

      ### filter using alpha
      if (is.logical(use_alpha)) {
        if (use_alpha)
          s[s < alpha] <- 0
      } else
        s[s < use_alpha] <- 0

      s
    },


    get_microclusters = function(cluster_type = c("strong", "all"), ...) {
      cluster_type <- match.arg(cluster_type)

      mc <- data.frame(CppObj$centers())

      if (cluster_type == "strong")
        mc <- mc[strong_mcs(), , drop = FALSE]
      rownames(mc) <- NULL

      if (nrow(mc) < 1)
        return(data.frame())

      colnames(mc) <- colnames
      mc
    },

    get_microweights = function(cluster_type = c("strong", "all"), ...) {
      cluster_type <- match.arg(cluster_type)

      w <- CppObj$weights()
      if (cluster_type == "strong")
        w <- w[strong_mcs()]
      w
    },

    get_macro_clustering = function() {
      mcs <- get_microclusters()
      w <- get_microweights()

      nclusters <- nrow(mcs)

      if (nclusters < 1L)
        return(list(
          centers = empty_df(colnames),
          microToMacro = integer(0L),
          weights = numeric(0L)
        ))

      if (nclusters == 1L)
        return(list(
          centers = mcs,
          microToMacro = 1L,
          weights = w[1L]
        ))

      if (shared_density) {
        ### use shared density

        if (k > 0L) {
          ### use k not alpha!
          s <- get_shared_density(use_alpha = FALSE)
          d <- as.dist(1 / (1 + s))

          hc <- hclust(d, method = "single")
          #assignment <- cutree(hc, k=k)

          ### find connected components
          assignment <- cutree(hc, h = 1 - 1e-9)

          ### not enough components?
          if (length(unique(assignment)) < k)
            assignment <- cutree(hc, k = k)

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

        } else{
          ### use alpha and connected components
          s <- get_shared_density(use_alpha = TRUE)
          s[s > 0] <- 1
          d <- as.dist(1 - s)
          assignment <- cutree(hclust(d, method = "single"), h = .5)
        }


      } else{
        ### use adjacent clusters overlap by alpha (packing factor)
        ### create a distance between 0 and inf
        ### (<1 means reachable, i.e., assignment areas overlap)
        d_pos <- dist(mcs, method = metric_name) / r - 1

        ### alpha = 0 -> 1    reachability at r
        ### alpha = 1 -> 0     highest packing
        h <- 1 - alpha
        assignment <-
          cutree(hclust(d_pos, method = "single"), h = h)

        ### use k if we don't get enough components!
        if (length(unique(assignment)) < k) {
          assignment <- cutree(hclust(d_pos, method = "single"), k = k)
        }
      }

      ### use minweight filtering of macro-clusters
      if (minweight > 0) {
        w_macro <- aggregate(w, by = list(assignment), FUN = sum)$x
        take <- which(w_macro >= (minweight * sum(w_macro)))
        assignment <- match(assignment, take)
      }

      ### find centroids
      macro <- .centroids(mcs, w, assignment)
      macro$microToMacro <- assignment

      colnames(macro$centers) <- colnames
      macro
    }
  )
)

# helper to memorize macro clusterings
.dbstream_update_macro <- function(x) {
  if (!x$RObj$newdata)
    return()

  cluster_list <- x$RObj$get_macro_clustering()
  x$macro$RObj$centers <- cluster_list$centers
  x$macro$RObj$weights <- cluster_list$weights
  x$macro$RObj$microToMacro <- cluster_list$microToMacro
  x$RObj$newdata <- FALSE
}


#' @export
get_macroclusters.DSC_DBSTREAM <- function(x, ...) {
  .dbstream_update_macro(x)
  get_centers(x$macro)
}

#' @export
get_macroweights.DSC_DBSTREAM <- function(x, ...) {
  .dbstream_update_macro(x)
  get_weights(x$macro)
}

#' @export
microToMacro.DSC_DBSTREAM <- function(x, micro = NULL, ...) {
  .nodots(...)
  .dbstream_update_macro(x)
  assignment <- x$macro$RObj$microToMacro

  if (!is.null(micro))
    assignment <- assignment[micro]
  assignment
}


#' @export
get_assignment.DSC_DBSTREAM <- function(dsc,
  points,
  type = c("auto", "micro", "macro"),
  method = c("auto", "model", "nn"),
  ...) {
  type <- match.arg(type)
  method <- match.arg(method)

  points <- remove_info(points)

  ## apply formula
  if (!is.null(dsc$RObj$colnames))
    points <- points[, dsc$RObj$colnames, drop = FALSE]

  if (method == "auto")
    method <- "model"
  if (method != "model")
    return(NextMethod())

  c <- get_centers(dsc, type = "micro", ...)

  if (nrow(c) > 0L) {
    dist <- dist(points, c, method = dsc$RObj$metric_name)
    # Find the minimum distance and save the class
    assignment <- apply(dist, 1L, which.min)

    # dist>threshold means no assignment
    #assignment[apply(dist, 1L, min) > dsc$RObj$r] <- NA_integer_
    # If we have an outlier_multiplier then we increase the radius
    r_multiplier <- 1
    if (!is.null(dsc$outlier_multiplier))
      r_multiplier <-
      dsc$outlier_multiplier
    if (!is.null(dsc$noise_multiplier))
      r_multiplier <-
      dsc$noise_multiplier
    assignment[apply(dist, 1L, min) > dsc$RObj$r * r_multiplier] <-
      NA_integer_

  } else {
    #warning("There are no clusters!")
    assignment <- rep(NA_integer_, nrow(points))
  }

  if (type == "macro")
    assignment <- microToMacro(dsc, assignment)

  attr(assignment, "method") <- "model"
  assignment
}

### DBSTREAM specific functions

#' @rdname DSC_DBSTREAM
#' @export
get_shared_density <- function(x, use_alpha = TRUE)
  x$RObj$get_shared_density(use_alpha = use_alpha)


#' @rdname DSC_DBSTREAM
#' @export
change_alpha <- function(x, alpha) {
  x$RObj$alpha <- alpha
  x$RObj$CppObj$alpha <- alpha
  x$state$newdata <- TRUE ### so macro clustering is redone
}

### special plotting for DSC_DBSTREAM
### FIXME: only show edges that really are used
#' @rdname DSC_DBSTREAM
#' @param dsd a data stream object.
#' @param n	number of plots taken from the dsd to plot.
#' @param col_points color used for plotting.
#' @param dim	an integer vector with the dimensions to plot. If NULL then for methods "pairs" and "pc" all dimensions are used and for "scatter" the first two dimensions are plotted.
#' @param method plot method.
#' @param type Plot micro clusters (`type="micro"`), macro clusters (`type="macro"`), both micro and macro clusters (`type="both"`), outliers(`type="outliers"`), or everything together (`type="all"`). `type="auto"` leaves to the class of DSC to decide.
#' @param assignment logical; show assignment area of micro-clusters.
#' @export
plot.DSC_DBSTREAM <- function(x,
  dsd = NULL,
  n = 500,
  col_points = NULL,
  #  col_clusters=c("red", "blue"),
  #  weights=TRUE,
  #  scale=c(1,5),
  #  cex =1,
  #  pch=NULL,
  #  ...,
  dim = NULL,
  method = "pairs",
  type = c("auto", "micro", "macro", "both", "none"),
  shared_density = FALSE,
  use_alpha = TRUE,
  assignment = FALSE,
  ...) {
  type <- match.arg(type)

  if (is.null(col_points))
    col_points <- .points_col

  if (type == "none")
    r <- plot(dsd,
      col = col_points,
      n = n,
      dim = dim,
      ...)
  #r <- NextMethod()
  else
    r <- plot.DSC(
      x = x,
      dsd = dsd,
      n = n,
      col_points = col_points,
      dim = dim,
      method = method,
      type = type,
      ...
    )

  if (!shared_density && !assignment)
    return(invisible(r))

  p <- get_centers(x, type = "micro")

  if (assignment) {
    ### add threshold circles
    if (!is.numeric(assignment))
      assignment <- 3L
    if (nrow(p) > 0) {
      points(p, col = "black", pch = 3L)
      for (i in 1:nrow(p)) {
        lines(
          ellipsePoints(x$RObj$r, x$RObj$r,
            loc = as.numeric(p[i, ]), n = 60),
          col = "black",
          lty = assignment
        )
      }
    }
  }

  if (shared_density) {
    if (!x$RObj$shared_density)
      stop("No shared density available!")

    if (ncol(p) > 2 &&
        !(!is.null(dim) && length(dim) != 2) && method != "scatter")
      stop("Only available to plot 2D data or the first 2 dimensions!")

    if (nrow(p) > 0) {
      #points(p, col="black")
      ### add edges connecting macro-clusters
      s <- x$RObj$get_shared_density(use_alpha = use_alpha)
      s[lower.tri(s)] <- NA

      edges <- which(s > x$RObj$alpha, arr.ind = TRUE)

      if (length(edges) > 0) {
        # length instead of nrow (s can be empty!)
        edges <- cbind(edges,
          w = apply(
            edges,
            MARGIN = 1,
            FUN = function(ij)
              s[ij[1], ij[2]]
          ))

        edges <- cbind(edges, map(edges[, 3], range = c(1, 4)))

        for (i in 1:nrow(edges)) {
          lines(rbind(p[edges[i, 1], ], p[edges[i, 2], ]),
            col = "black", lwd = edges[i, 4])
        }
      }
    }
  }
}



#' @rdname DSC_DBSTREAM
#' @export
DSOutlier_DBSTREAM <- function(formula = NULL,
  r,
  lambda = 1e-3,
  gaptime = 1000L,
  Cm = 3,
  metric = "Euclidean",
  outlier_multiplier = 2) {
  cl <- DSC_DBSTREAM(formula, r, lambda, gaptime, Cm, metric)
  class(cl) <- c("DSOutlier", class(cl))

  cl$outlier_multiplier <- outlier_multiplier

  cl
}


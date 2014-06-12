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

## evaluate clusterings

### FIXME: calculate dist only once

.eval_measures_fpc  <- c(
  ### internal
  "average.between",        
  "average.within",          
  "max.diameter",        
  "min.separation",
  "ave.within.cluster.ss",
  "g2", "pearsongamma",
  "dunn", "dunn2", 
  "entropy", "wb.ratio", 
  
  ### external
  "corrected.rand",
  "vi"
)

.eval_measures  <- c(
  # info
  "numMicroClusters","numMacroClusters","numClasses",
  # internal
  "SSQ",
  "silhouette",
  # external
  "precision", "recall", "F1",
  "purity", "fpr",
  "classPurity", 
  "Euclidean", "Manhattan", "Rand", "cRand",
  "NMI", "KP", "angle", "diag", "FM", "Jaccard", "PS" 
)

.eval_measures_special  <- c(  
  "tNNRelations", "tNNTotalMicroClusters"
)

evaluate <- function (dsc, dsd, measure, n = 1000, 
  type=c("auto", "micro", "macro"), assign="micro", 
  assignmentMethod=c("auto","model", "nn"), ...) {
  
  assignmentMethod <- match.arg(assignmentMethod)
  type <- get_type(dsc, type)  

  all_measures <- c(.eval_measures, .eval_measures_fpc)
  if(missing(measure) || is.null(measure)) measure <- all_measures
  else measure <- all_measures[pmatch(tolower(measure), tolower(all_measures))] 
  
  centers <- get_centers(dsc, type=type) 
  
  if(nrow(centers)<1) {
    warning("No centers available!")
    e <- rep.int(NA_real_, length(measure))
    names(e) <- measure
    return(structure(e, type=type, assign=assign, class="stream_eval"))
  }
  
  points <- get_points(dsd, n, assignment = TRUE)
  actual <- attr(points, "assignment")
  predict <- get_assignment(dsc, points, type=assign, method=assignmentMethod, ...)
  
  ### translate micro to macro cluster ids if necessary
  if(type=="macro" && assign=="micro") predict <- microToMacro(dsc, predict)
  else if (type!=assign) stop("type and assign are not compatible!")
  
  fpc <- measure %in% .eval_measures_fpc
  if(any(fpc)) {
    # noise cluster has the highest index and need to renumber
    withnoise <- any(is.na(actual))
    actual[is.na(actual)] <- max(actual, na.rm=TRUE) + 1L
    actual <- match(actual, unique(sort(actual)))
    predict[is.na(predict)] <- max(predict, na.rm=TRUE) + 1L 
    predict <- match(predict, unique(sort(predict)))

    e <- fpc::cluster.stats(d=dist(points), clustering=predict, 
      alt.clustering = actual,
      noisecluster=TRUE,
      silhouette = TRUE,
      G2 = TRUE, G3 = FALSE,
      wgap=FALSE, sepindex=FALSE, sepprob=0.1,
      sepwithnoise=withnoise,
      compareonly = FALSE,
      aggregateonly = TRUE)
    e <- unlist(e)
  }else e <- numeric()
    
  if(any(!fpc)) {
    m <- sapply(measure[!fpc], function(x) .evaluate(x, predict, actual, 
      points, centers, dsc))
    e <- c(e, m)
  }
  
  e <- e[measure]

  structure(e, type=type, assign=assign, class="stream_eval")
  
}

print.stream_eval <-  function(x, ...) {
  cat("Evaluation results for ", attr(x, "type"),"-clusters.\n", sep="")
  cat("Points were assigned to ", attr(x, "assign"),"-clusters.\n\n", sep="")
  names <- names(x)
  x <- as.numeric(x)
  names(x) <- names 
  print(x)
}

### evaluate during clustering 
evaluate_cluster <- function(dsc, dsd, macro=NULL, measure, 
  n=1000, type=c("auto", "micro", "macro"), assign="micro",
  assignmentMethod =  c("auto", "model", "nn"),
  horizon=100, verbose=FALSE, ...) {
  
  evaluations <- data.frame()
  for(i in 1:(n/horizon)) {
    wrapper <- DSD_Wrapper(dsd, n=horizon, loop=FALSE)
    cluster(dsc, wrapper, horizon)
    
    reset_stream(wrapper)
    if(is.null(macro)) {
      e <- evaluate(dsc, wrapper, measure, 
        horizon, type, assign, assignmentMethod, ...)
    } else {
      suppressWarnings(recluster(macro, dsc)) ### we get reclustering warnings
      e <- evaluate(macro, wrapper, measure,
        horizon, type, assign, ...)
    }
    
    if(i==1) evaluations <- e
    else evaluations <- rbind(evaluations,e)
    
    if(verbose) {
      print(c(points=i*horizon,e))
    }
  }
  
  rownames(evaluations) <- NULL
  evaluations <- cbind(points=(1:(n/horizon))*horizon, evaluations)
  
  evaluations
}

# work horse
.evaluate <- function(measure, predict, actual, points, 
  centers, dsc) {
  
  #make a vector of all of the measures and then do a lot of if statements
  measures <- .eval_measures
  
  #finds index of partial match in array of measures
  m <- measures[pmatch(tolower(measure),tolower(measures))] 
  
  if(is.na(m)) stop("Invalid measure \"", measure, "\".")
  
  ### FIXME: deal with noise!
  ### Noise it its own group with index 0
  ### this works for external measures
  predict[is.na(predict)] <- 0L
  actual[is.na(actual)] <- 0L
  
  res <- switch(m,
    numMicroClusters	    = if(is(try(n <- nclusters(dsc, type="micro"), 
      silent=TRUE), "try-error")) NA_integer_ else n,
    numMacroClusters	    = if(is(try(n <- nclusters(dsc, type="macro"), 
      silent=TRUE), "try-error")) NA_integer_ else n,
    numClasses	    = numClasses(actual),
    
    SSQ  	    = ssq(points, actual, centers),
    silhouette = silhouette(points, actual, predict),
    
    precision	    = precision(actual, predict),
    recall	    = recall(actual, predict),
    F1		    = f1(actual, predict),
    purity	    = precision(actual, predict),
    fpr		    = recall(actual, predict),
    
    Euclidean	    = clue_agreement(predict, actual, "euclidean"),
    Manhattan	    = clue_agreement(predict, actual, "manhattan"),
    Rand	    = clue_agreement(predict, actual, "rand"),
    cRand	    = clue_agreement(predict, actual, "crand"),
    NMI		    = clue_agreement(predict, actual, "NMI"),
    KP		    = clue_agreement(predict, actual, "KP"),
    angle	    = clue_agreement(predict, actual, "angle"),
    diag	    = clue_agreement(predict, actual, "diag"),
    FM		    = clue_agreement(predict, actual, "FM"),
    Jaccard	    = clue_agreement(predict, actual, "jaccard"),
    #	purity	    = clue_agreement(predict, actual, "purity"),
    PS		    = clue_agreement(predict, actual, "PS"),
    
    classPurity	    = recall(actual, predict),
    
    # tNN specific
    tNNRelations = { if(!is(dsc, "DSC_tNN")) NA else length(dsc$RObj$relations) }, 
    tNNTotalMicroClusters = { if(!is(dsc, "DSC_tNN")) NA else nrow(dsc$RObj$centers) }
  )
  
  res
  
}

### helper
colMax <- function(x, which=FALSE) {
  if(!which) apply(x, 2, FUN=function(y) max(y))
  else {
    apply(x, 2, FUN=function(y) which.max(y))
  }
}

rowMax <- function(x, which=FALSE) {
  if(!which) apply(x, 1, FUN=function(y) max(y))
  else {
    apply(x, 1, FUN=function(y) which.max(y))
  }
}

f1 <- function(actual, predict) {
  precision <- precision(actual, predict)
  recall <- recall(actual, predict)
  (2*precision*recall)/(precision+recall)
}

# recall tp/(tp+fn)
recall <- function(actual, predict) {
  confusion <- table(actual, predict) 
  relevant <- rowSums(confusion)
  
  sum(colMax(confusion))/sum(relevant[colMax(confusion, which=TRUE)])
}

# precision tp/(tp+fp) == purity (sum_j \max_i n_{ij} / n)
precision <- function(actual, predict) {
  confusion <- table(actual, predict)    
  sum(colMax(confusion))/length(predict)
}

# as defined in Density-Based Clustering of Data Streams at 
# Multiple Resolutions by Wan et al
classPurity <- function(actual, predict) {
  confusion <- table(actual, predict)
  mean(rowMax(confusion)/rowSums(confusion))
}

numClusters <- function(centers) {
  nrow(centers)
}

numClasses <- function(actual) {
  length(unique(actual))
}

ssq <- function(points, actual, centers) {
  ### ssq does not use noise points
  points <- points[actual!=0,]
  sum(apply(dist(points,centers), 1L , min))
}

silhouette <- function(points, actual, predict) {
  ### silhouette does not use noise points
  noise <- actual==0 & predict==0
  mean(cluster::silhouette(predict[!noise], dist(points[!noise,]))[,"sil_width"])
}
clue_agreement <- function(predict, actual, measure) {
  predict <- clue::as.cl_hard_partition(predict)
  actual <- clue::as.cl_hard_partition(actual)
  as.numeric(clue::cl_agreement(clue::cl_ensemble(predict, actual), method=measure))
}


### this would need package Matrix
#get_confusionMatrix <- function(d,c,predict) {
#	#Get the actual class
#	actual <- attr(d, "assignment")
#	
#	actual[is.na(actual)]<- 0
#	
#	if(0 %in% actual)
#		actual <- actual + 1
#	
#	result <- cbind(actual,predict)
#	#Compute the sparse matrix
#	confusion <- sparseMatrix(i = result[,1],j = result[,2], x = 1)
#	confusion
#}

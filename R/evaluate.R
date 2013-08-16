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

.eval_methods  <- c(
    "numMicroClusters","numMacroClusters","numClasses",
    "precision", "recall", "F1",
    "purity", "fpr",
    "SSQ",
    "Euclidean", "Manhattan", "Rand", "cRand",
    "NMI", "KP", "angle", "diag", "FM", "Jaccard", "PS", 
    "classPurity", "tNNRelations", "tNNTotalMicroClusters")


evaluate <- function (dsc, dsd, method, n = 1000, 
                      type=c("auto", "micro", "macro"), assign="micro", ...) {
  
  methods <- .eval_methods
  
  # last two methods are tNN specific
  if(missing(method)) method <- head(methods, length(methods)-2L)
  if(is.null(method)) method <- head(methods, length(methods)-2L)
  else method <- methods[pmatch(tolower(method),tolower(methods))] 
  
  ### figure out type
  type <- get_type(dsc, type)
  
  ### FIXME: we might want to use only assigned micro-clusters here!
  c <- get_centers(dsc, type=type) 
  
  
  if(nrow(c)<1) {
    warning("No centers available!")
    return(0)
  }
  
  d <- get_points(dsd, n, assignment = TRUE)
  actual <- attr(d, "assignment")
  
  predict <- get_assignment(dsc,d, type=assign)
  
  ### translate micro to macro cluster ids if necessary
  if(type=="macro" && assign=="micro") predict <- microToMacro(dsc, predict)
  else if (type!=assign) stop("type and assign are not compatible!")
  
  ### make points assigned to unassigned micro-clusters noise (0)
  predict[is.na(predict)] <- 0L
  
  ### remove noise
  noise <- which(is.na(actual))
  if(length(noise)>0) {
    predict <- predict[-noise]
    actual <- actual[-noise]
    d <- d[-noise,]
  }
  
  e <- sapply(method, function(x) .evaluate(x, predict, actual, 
                                            d, c, dsc))
  
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
evaluate_cluster <- function(dsc, dsd, macro=NULL, method, 
                             n=1000, type=c("auto", "micro", "macro"), assign="micro", 
                             horizon=100, verbose=FALSE, ...) {
  
  evaluations <- data.frame()
  for(i in 1:(n/horizon)) {
    wrapper <- DSD_Wrapper(dsd, n=horizon, loop=FALSE)
    cluster(dsc, wrapper, horizon)
    
    reset_stream(wrapper)
    if(is.null(macro)) {
      e <- evaluate(dsc, wrapper, method, 
                    horizon, type, assign, ...)
    } else {
      suppressWarnings(recluster(macro, dsc)) ### we get reclustering warnings
      e <- evaluate(macro, wrapper, method,
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
.evaluate <- function(method, predict, actual, points, 
                      centers, dsc) {
  
  #make a vector of all of the methods and then do a lot of if statements
  methods <- .eval_methods
  
  #finds index of partial match in array of methods
  method <- methods[pmatch(tolower(method),tolower(methods))] 
  
  if(is.na(method)) stop("Invalid measure.")
  
  res <- switch(method,
                numMicroClusters	    = {n <- NA; 
                        try(n <- nclusters(dsc, type="micro"), silent=TRUE); 
                        n},
                numMacroClusters      = {n <- NA; 
                        try(n <- nclusters(dsc, type="macro"), silent=TRUE); 
                        n},
                numClasses	    = numClasses(actual),
                
                precision	    = precision(actual, predict),
                recall	    = recall(actual, predict),
                F1		    = f1(actual, predict),
                
                purity	    = precision(actual, predict),
                fpr		    = recall(actual, predict),
                
                SSQ		    = ssq(points,centers),
                
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

ssq <- function(points, centers) {
  dist <- dist(points,centers)
  mindistance <- apply(dist, 1, min)
  sum(mindistance)
}

clue_agreement <- function(predict, actual, method) {
  predict <- clue::as.cl_hard_partition(predict)
  actual <- clue::as.cl_hard_partition(actual)
  as.numeric(clue::cl_agreement(clue::cl_ensemble(predict, actual), method=method))
}

### silhouette <- function(d,c,assignment = NULL) {}

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

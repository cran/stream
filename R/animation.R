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



animate_cluster <- function(dsc, dsd, macro=NULL, n=1000,
  wait=.1, pointInterval=100, horizon=5*pointInterval, 
  evaluationMethod=NULL, evaluationType="micro", evaluationAssign="micro", 
  ...) {
  
  cluster.ani(dsc, dsd, macro, n, wait, 
    pointInterval, horizon, 
    evaluationMethod, evaluationType, evaluationAssign, 
    ...)
}

animate_data <- function(dsd, n=1000, 
  wait=.1, pointInterval=100, horizon=5*pointInterval, 
  ...) {
  
  cluster.ani(NULL, dsd, NULL, n, wait, pointInterval, 
    horizon, NULL,...)
}


cluster.ani <- function(dsc=NULL, dsd, macro=NULL, n=1000,
  wait=.1, pointInterval=100, horizon=5*pointInterval, 
  evaluationMethod=NULL, evaluationType="micro", evaluationAssign="micro", ...) {
  
  animation::ani.record(reset = TRUE)
  
  op <- par(no.readonly = TRUE)
  
  if(!is.null(evaluationMethod)) {
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights=c(3,1))
    evaluation <- data.frame(points=(1:(n/pointInterval))*pointInterval, eval=NA)
  } else
    layout(matrix(c(1,1), 2, 1, byrow = TRUE))
  
  
  ### do micro or macro 
  if(!is.null(macro)) cl <- macro
  else cl <- dsc
  
  points <- data.frame()
  assignment <- numeric()
  col <- gray.colors(horizon, start = 1, end = .7, gamma = 2.2)
  
  for (i in 1:n) {
    ### FIXME: do this in blocks!
    d <- get_points(dsd, assignment=TRUE)
    points <- rbind(points,d)
    assignment <- c(assignment,attr(d,"assignment"))
    
    if(nrow(points) > horizon) {
      points <- points[(nrow(points)-horizon +1):nrow(points),]
      assignment <- assignment[
        (length(assignment)-horizon +1):length(assignment)]
    }
    
    ## cluster?
    if(!is.null(dsc)) cluster(dsc, DSD_Wrapper(d[1,]),1)
    
    if(i %% pointInterval == 0) {
      ## recluster
      if(!is.null(dsc) && !is.null(macro)) 
        suppressWarnings(recluster(macro,dsc))
      
      ## plot points and clustering
      points_dsd <- DSD_Wrapper(points,assignment=assignment)
      
      ## eval part 1
      if(!is.null(evaluationMethod)) {
        reset_stream(points_dsd)
        
        evaluation[i/pointInterval,2] <- evaluate(cl,points_dsd,
          method=evaluationMethod, type=evaluationType, 
          assign=evaluationAssign, n=nrow(points))
        
        reset_stream(points_dsd)
      }
      
      par(mar=c(4.1,4.1,2.1,2.1))
      if(!is.null(dsc)) {
        plot(cl, points_dsd,
          n=nrow(points),
          col_points=col[horizon-nrow(points)+1: horizon],...)
      } else {
        plot(points_dsd,
          n=nrow(points),...)
      }
      
      ## eval part 2
      if(!is.null(evaluationMethod)) {
        par(mar=c(2.1,4.1,1.1,2.1))
        plot(evaluation, type="l", col="blue",
          ylim=c(0,1), xlim=c(1,n), ann=FALSE) 
        title(ylab=evaluationMethod)
      }
      
      animation::ani.record()
      
      if(wait>0) Sys.sleep(wait)
    }
    
  }
  
  par(op)
  
  if(!is.null(evaluationMethod)) {
    colnames(evaluation) <- c("points", evaluationMethod)
    evaluation
  }else invisible(NULL)
  
}
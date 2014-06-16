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
  wait=.1, horizon=100,
  evaluationMeasure=NULL, evaluationType="micro", evaluationAssign="micro", 
  evaluationAssignmentMethod="auto", ...) {
  
  cluster.ani(dsc, dsd, macro, n, wait, horizon, 
    evaluationMeasure, evaluationType, evaluationAssign, 
    evaluationAssignmentMethod, ...)
}

animate_data <- function(dsd, n=1000, 
  wait=.1, horizon=100, ...) {
  
  cluster.ani(NULL, dsd, NULL, n, wait, horizon, NULL,...)
}


cluster.ani <- function(dsc=NULL, dsd, macro=NULL, n=1000,
  wait=.1, horizon=100, 
  evaluationMeasure=NULL, evaluationType="micro", evaluationAssign="micro", 
  evaluationAssignmentMethod="auto", ...) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  animation::ani.record(reset = TRUE)
  
  rounds <- n %/% horizon 
  
  if(!is.null(evaluationMeasure)) {
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights=c(3,1.5))
    evaluation <- data.frame(points=seq(from=1, by=horizon, length.out=rounds), 
      measure=NA_real_)
  }
  
  for(i in 1:rounds) {
    d <- DSD_Wrapper(dsd, n=horizon, loop=FALSE)
    
    if(!is.null(dsc)) {
      cl <- cluster(dsc, d, horizon)
      
      if(!is.null(macro)) cl <- recluster(macro, dsc)
      
      if(!is.null(evaluationMeasure)) {
        reset_stream(d)
        evaluation[i,2] <- evaluate(cl, d,
          measure=evaluationMeasure, type=evaluationType, 
          assign=evaluationAssign, assignmentMethod=evaluationAssignmentMethod,
	  n=horizon)
      }
      
      reset_stream(d)
      if(!is.null(evaluationMeasure)) par(mar=c(4.1,4.1,2.1,2.1))
      plot(cl, d, n=horizon, ...)        
      
      if(!is.null(evaluationMeasure)){
        par(mar=c(2.1,4.1,1.1,2.1))
        plot(evaluation, type="l", col="blue",
          #ylim=c(0,1), 
          ann=FALSE) 
        title(ylab=evaluationMeasure)        
      }
          
    }else{
      ### plot just data
      plot(d, n=horizon, ...)
    }
    
    animation::ani.record()
    if(wait>0) Sys.sleep(wait)  
    
  }
  
  if(!is.null(evaluationMeasure)) {
    colnames(evaluation) <- c("points", evaluationMeasure)
    evaluation
  }else invisible(NULL)
}

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


DSD_Wrapper <- function(x, n, k=NA, loop=FALSE, assignment = NULL, 
  description=NULL) {
  
  if(is(x, "DSD")) {
    if(is.na(k) && !is.null(x$k)) k <- x$k
    x <- get_points(x, n, assignment = TRUE)
    x$assignment <- attr(x, "assignment")
    assignment <- ncol(x) 
  }else if(is.null(ncol)) stop("x needs to be a matrix-like object!") 
  
  if(length(assignment) == nrow(x)) {
    x <- cbind(x, assignment)
    assignment <- ncol(x)
  }
  
  if(is.null(assignment)) d <- ncol(x) else d <- ncol(x)-1L
  
  state <- new.env()
  assign("counter", 1L, envir = state)
  
  if(is.null(description)) description <- "Matrix Stream Wrapper"
  
  # creating the DSD object
  structure(list(
    description = description,
    strm = x,
    state = state,
    d = d,
    k = k,
    loop = loop,
    assignment = assignment
    ), class = c("DSD_Wrapper","DSD_R","DSD"))
}

get_points.DSD_Wrapper <- function(x, n=1, assignment = FALSE,...) {
  n <- as.integer(n)
  
  if(x$state$counter > nrow(x$strm)) {
    if(x$loop) x$state$counter <- 1L
    else stop("The stream is at its end!")
  }
  
  n_left <- nrow(x$strm) - x$state$counter + 1L
  
  if(n_left < n && !x$loop) stop("Not enought data points left in stream!")
  
  if(n_left >= n) {
    ### regular case
    d <- x$strm[x$state$counter:(x$state$counter + n -1L),,drop=FALSE]
    x$state$counter <- x$state$counter + n
  }else{
    ### we need to loop!
    
    
    # take what is left and reset counter
    d <- x$strm[x$state$counter:nrow(x$strm),,drop=FALSE] 
    togo <- n-n_left
    x$state$counter <- 1L
    
    while(togo > 0L) {
      n_left <- nrow(x$strm) - x$state$counter + 1L
      
      if(n_left < togo) {
        # take the whole stream
        d <- rbind(d, x$strm)
        togo <- togo - n_left
      }else{
        # take the rest
        d <- rbind(d, x$strm[1:(x$state$counter+togo-1),])
        x$state$counter <- x$state$counter + togo
        togo <- 0L
      }
    }
  }

  if(!is.null(x$assignment)) {
    a <- d[, x$assignment]
    d <- d[,-x$assignment]
  }else a <- rep.int(1L, times=nrow(d))
  
  d <- data.frame(d)
  
  if(assignment) attr(d, "assignment") <- a
  
  d
}

print.DSD_Wrapper <- function(x, ...) {
  NextMethod() # calling the super classes print()
  pos <- x$state$counter
  if (pos>nrow(x$strm)) 
    if (!x$loop) pos <- "'end'" else pos <- 1
  cat(paste('Contains', nrow(x$strm), 
    'data points - currently at position', pos, 
    '- loop is', x$loop, '\n'))
}

reset_stream.DSD_Wrapper <- function(dsd, pos=1) {
  dsd$state$counter <- pos
}

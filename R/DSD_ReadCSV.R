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


# accepts an open connection
DSD_ReadCSV <- function(file, sep=",", k=NA, d=NA,
  take=NULL, class=NULL, loop=FALSE) {
  
  # if the user passes a string, create a new connection and open it
  if (is(file,"character")) {
    file <- file(file)
    open(file)
  }
  
  # error out if no string or connection is passed
  else if (!is(file,"connection")) {
    stop("please pass a valid connection")
  }
  
  # open the connection if its closed
  else if (!isOpen(file)) {
    open(file)
  }
  
  
  
  # seekable? get bytes_per_point + d
  bytes_per_point <- NA
  if(isSeekable(file)) {
    tryCatch({
      dat <- suppressWarnings(read.table(file=file, 
        sep=sep, nrows=1, comment.char=""))
    }, error = function(ex) {})
    if(nrow(dat) >0) {
      bytes_per_point <- seek(file)
      seek(file, where = 0)
      d <- ncol(dat)
    }
  }else if(loop) stop("Loop only allowed for seekable connections!")
  
  # figure out d from take
  if(!is.null(take)) d <- length(take)
  
  # filename
  filename <- basename(summary(file)$description)
  
  # creating the DSD object
  l <- list(
    description = paste('File Data Stream (', filename, ')', sep=''),
    d = d,
    k = k,
    file = file,
    sep = sep,
    take = take,
    class = class,
    bytes_per_point = bytes_per_point,
    loop = loop)
  class(l) <- c("DSD_ReadCSV", "DSD_R", "DSD_data.frame", "DSD")
  
  l
}

## it is important that the connection is OPEN
get_points.DSD_ReadCSV <- function(x, n=1, 
  outofpoints=c("stop", "warn", "ignore"), 
  cluster = FALSE, class = FALSE, ...) {
  
  outofpoints <- match.arg(outofpoints)
  n <- as.integer(n)  
  
  d <- data.frame()
  if(!isSeekable(x$file)) pos <- NA else pos <- seek(x$file)
  tryCatch({
    d <- suppressWarnings(read.table(file=x$file, 
      sep=x$sep, nrows=n, comment.char="", ...))
  }, error = function(ex) {})
  
  if(nrow(d) < n) {
    if(!x$loop || !isSeekable(x$file)){
      if(outofpoints == "stop") {
        if(!is.na(pos)) seek(x$file, pos)
        stop("Not enough points in the stream!")
      }
      if(outofpoints == "warn") 
        warning("The stream is at its end! Returning available points!")
    } else { ### looping
      while(nrow(d) < n) {
        seek(x$file, where=0) # resetting the connection
        d2 <- suppressWarnings(read.table(file=x$file, 
          sep=x$sep, nrows=n-nrow(d), comment.char="", ...))
        if(nrow(d2) < 1) stop("Empty stream!")
        
        d <- rbind(d, d2)
      }
    }      
  }
  
  cl <- NULL
  if((class || cluster)&& nrow(d)>0) {
    if(is.null(x$class)) {
      warning("No class labels avaialble!")
    } else cl <- d[,x$class[1]]
  }
  
  if(!is.null(x$take) && nrow(d)>0) d <- d[,x$take, drop=FALSE]
  
  if(cluster) attr(d, "cluster") <- cl
  if(class) d <- cbind(d, class = cl)
  
  d
}

reset_stream.DSD_ReadCSV <- function(dsd, pos=1) {
  if(is.na(dsd$bytes_per_point)) stop("Underlying conneciton does not support seek!")
  invisible(seek(dsd$file, where=(pos-1) * dsd$bytes_per_point))
}

close_stream <- function(dsd) {
  if(!is(dsd, "DSD_ReadCSV")) 
    stop("'dsd' is not of class 'DSD_ReadCSV'")
  close(dsd$file)
}

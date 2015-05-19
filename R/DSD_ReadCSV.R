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
# ... goes to read.table
DSD_ReadCSV <- function(file, k=NA, d=NA,
  take=NULL, class=NULL, loop=FALSE, 
  sep=",", header=FALSE, skip=0, ...) {
  
  # if the user passes a string, create a new connection and open it
  if (is(file, "character")) file <- file(file)
  
  # error out if no string or connection is passed
  if (!is(file, "connection")) stop("Please pass a valid connection!")
  
  # open the connection if its closed
  if (!isOpen(file)) open(file)
  
  # seekable?
  if(loop && !isSeekable(file)) stop("Loop only allowed for seekable connections!")
  
  # skip
  if(skip>0) readLines(file, n=skip)
  
  # header?
  if(header) {
    header <- as.character(read.table(text=readLines(con=file, n=1), 
      sep=sep, as.is=TRUE, ...))
    if(!is.null(take)) header <- header[take]  
  }else header <- NULL
  
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
    header = header,
    read.table.args = list(...),
    class = class,
    loop = loop,
    skip = skip)
  class(l) <- c("DSD_ReadCSV", "DSD_R", "DSD_data.frame", "DSD")
  
  l
}

## it is important that the connection is OPEN
get_points.DSD_ReadCSV <- function(x, n=1, 
  outofpoints=c("stop", "warn", "ignore"), 
  cluster = FALSE, class = FALSE, ...) {
  .nodots(...)
  
  outofpoints <- match.arg(outofpoints)
  n <- as.integer(n)  
  
  d <- data.frame()
  if(!isSeekable(x$file)) pos <- NA else pos <- seek(x$file)
  ### only text connections can do read.table
  if(summary(x$file)$text == "text"){
    d <- do.call(read.table, c(list(file=x$file, sep=x$sep, nrows=n), 
      x$read.table.args))}
  else{
    ### need to get the data and wrap it into a textconnection (slow)
    d <- do.call(read.table, c(list(text=readLines(con=x$file, n=n), sep=x$sep, 
      nrows=n), 
      x$read.table.args))
  }
  
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
        d2 <- do.call(read.table, c(list(text=readLines(con=x$file, n=n-nrow(d)), 
          sep=x$sep), x$read.table.args))
        if(nrow(d2) < 1) stop("Empty stream!")
        
        d <- rbind(d, d2)
      }
    }      
  }
  
  cl <- NULL
  if(nrow(d)>0) {
    if(class || cluster) {
      if(is.null(x$class)) {
        warning("No class labels available!")
      } else cl <- d[,x$class[1]]
    }
    
    if(!is.null(x$take)) d <- d[,x$take, drop=FALSE]
    if(is.null(x$take) && !is.null(x$class)) d <- d[,-x$class, drop=FALSE]
  }  
  
  if(!is.null(x$header)) colnames(d) <- x$header
  
  if(cluster) attr(d, "cluster") <- cl
  if(class) d <- cbind(d, class = cl)
  d
}

reset_stream.DSD_ReadCSV <- function(dsd, pos=1) {
  pos <- as.integer(pos)
  
  if(!isSeekable(dsd$file)) stop("Underlying conneciton does not support seek!")
  seek(dsd$file, where=0)
  
  if(dsd$skip>0) readLines(dsd$file, n=dsd$skip)
  if(!is.null(dsd$header)) readLines(dsd$file, n=1)
  if(pos>1) readLines(dsd$file, n=pos-1L) 
  invisible(NULL)
}

close_stream <- function(dsd) {
  if(!is(dsd, "DSD_ReadCSV")) 
    stop("'dsd' is not of class 'DSD_ReadCSV'")
  close(dsd$file)
}

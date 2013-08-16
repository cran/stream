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
DSD_ReadStream <- function(file, sep=",", k=NA, d=NA,
	take=NULL, assignment=NULL, 
	loop=FALSE) {

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

    # figure out d
    if(is.na(d) && !is.null(take)) d <- length(take)


    # creating the DSD object
    l <- list(description = "File Data Stream",
	    d = d,
	    k = k,
	    file = file,
	    sep = sep,
	    take = take,
	    assignment = assignment,
	    loop = loop)
    class(l) <- c("DSD_ReadStream","DSD_R","DSD")
    
    l
}

## it is important that the connection is OPEN
get_points.DSD_ReadStream <- function(x, n=1, assignment=FALSE, ...) {

    togo <- n

    # comment.char="" is for performance reasons
    tryCatch({
		d <- suppressWarnings(read.table(file=x$file, 
				sep=x$sep, nrows=n, comment.char="", ...))
		togo <- n - nrow(d)
	    }, error = function(ex) {
	    })

    # this means no lines were read, we need to do a prep-read before looping
    if (x$loop && togo == n) {
	seek(x$file, where=0) # resetting the connection
	d <- suppressWarnings(read.table(file=x$file, 
			sep=x$sep, nrows=n, comment.char="", ...))
	togo <- n - nrow(d)
    }

    # we need to loop
    while (x$loop && togo > 0) {
	seek(x$file, where=0) # resetting the connection

	prev <- nrow(d)	
	d <- suppressWarnings(rbind(d, read.table(file=x$file, 
				sep=x$sep, nrows=togo, comment.char="", ...)))
	togo <- togo - (nrow(d)-prev)
    }

    # looping disabled, warn the user
    if (!x$loop && togo == n) {
	stop("looping disabled and the stream is empty")
    }

    else if (!x$loop && togo > 0) {
	warning("reached the end of the stream, returned as much as possible")
    }

    if(assignment) {
	if(is.null(x$assignment)) {
	    warning("No assignment avaialble!")
	    cl<-NULL
	}else cl <- d[,x$assignment[1]]
    }

    if(!is.null(x$take)) d <- d[,x$take, drop=FALSE]


    
    if(assignment) attr(d, "assignment") <-cl
    
    d
}

reset_stream.DSD_ReadStream <- function(dsd) {
    invisible(seek(dsd$file, where=0))
}

close_stream <- function(dsd) {
    if(!is(dsd, "DSD_ReadStream")) 
	stop("'dsd' is not of class 'DSD_ReadStream'")
    close(dsd$file)
}

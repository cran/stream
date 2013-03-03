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

get_points.DSD_MOA <- function(x, n=1, assignment=FALSE, ...) {
	
	if (n < 1)
		stop("n must be > 0")
	
	# pre-allocating the space for the matrix
	data <- matrix(NA, nrow=n, ncol=x$d)
	
	if(assignment) a <- numeric()

	# unpackaging the java instances
	for (i in 1:n) {
		instance <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
		row <- .jcall(instance, "[D", "toDoubleArray")
		class <- .jcall(instance, "D", "classValue")
		data[i,] <- row[1:x$d]
		
		if(assignment) {
			 a[i] <- class
		}
	}
	
	data <- data.frame(data)
	
	if(assignment) {
		a[which(a==-1)] <- NA
		attr(data,"assignment") <- a + 1
	}
	
	data
}

get_points.DSD_MOA <- function(x, n=1, assignment=FALSE, ...) {
	
	if (n < 1)
		stop("n must be > 0")
	
	# pre-allocating the space for the matrix
	data <- matrix(NA, nrow=n, ncol=x$d)
	
	if(assignment) a <- numeric()

	# unpackaging the java instances
	for (i in 1:n) {
		instance <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
		row <- .jcall(instance, "[D", "toDoubleArray")
		class <- .jcall(instance, "D", "classValue")
		data[i,] <- row[1:x$d]
		
		if(assignment) {
			 a[i] <- class
		}
	}
	
	data <- data.frame(data)
	
	if(assignment) {
		a[which(a==-1)] <- NA
		attr(data,"assignment") <- a + 1
	}
	
	data
}

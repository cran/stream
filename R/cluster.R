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


## wrapper for cluster functions

cluster <- function(dsc, dsd, n=1, verbose=FALSE, ...) { 
  if (n > 0) {
    
    # set new data flag
    if(is.environment(dsc$macro)) dsc$macro$newdata <- TRUE
    
    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    .cluster(dsc, dsd, n, verbose, ...)
  }
  
  # so cl <- cluster(cl, ...) also works
  invisible(dsc)
}

### Worker definition. Subclasses define a .cluster function
.cluster <- function(dsc, dsd, n, verbose=FALSE, ...) UseMethod(".cluster")





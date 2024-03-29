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

#' Save and Read DSC Objects
#'
#' Save and Read DSC objects safely (serializes the underlying data structure).
#' This also works for \pkg{streamMOA} DSC objects.
#'
#' @family DSC
#'
#' @name read_saveDSC
#' @param object a DSC object.
#' @param file filename.
#' @param ... further arguments.
#' @author Michael Hahsler
#' @seealso \code{\link{saveRDS}} and \code{\link{readRDS}}.
#' @examples
#'
#' stream <- DSD_Gaussians(k = 3, noise = 0.05)
#'
#' # create clusterer with r = 0.05
#' dbstream1 <- DSC_DBSTREAM(r = .05)
#' update(dbstream1, stream, 1000)
#' dbstream1
#'
#' saveDSC(dbstream1, file="dbstream.Rds")
#'
#' dbstream2 <- readDSC("dbstream.Rds")
#' dbstream2
#'
#' ## cleanup
#' unlink("dbstream.Rds")

#' @rdname read_saveDSC
#' @export
saveDSC <- function(object, file, ...) {
  ### for MOA based objects from streamMOA (rJava)
    if(!is.null(object$javaObj)) rJava::.jcache(object$javaObj)

    if(!is.null(object$micro$javaObj)) rJava::.jcache(object$micro$javaObj)
    if(!is.null(object$macro$javaObj)) rJava::.jcache(object$macro$javaObj)

  ### for RCpp
    try(object$RObj$cache(), silent = TRUE)
    try(object$micro$RObj$cache(), silent = TRUE)
    try(object$macro$RObj$cache(), silent = TRUE)

    saveRDS(object, file=file, ...)
}

#' @rdname read_saveDSC
#' @export
readDSC <- function(file) {
    d <- readRDS(file)

    ### RJava deserializes automatically

    ### for Rcpp
    try(d$RObj$uncache(), silent = TRUE)
    try(d$micro$RObj$uncache(), silent = TRUE)
    try(d$macro$RObj$uncache(), silent = TRUE)

    d
}


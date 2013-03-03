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


# denstream options:
# -h horizon "Range of the window."  1000
# -e epsilon 	0.01 (defines the epsilon neighborhood, range: 0 to 1)
# -p minPoints 	10 (min. num. points a cluster must have)
# -b beta	0.001 (range: 0 to 1)
# -m mu		1 (range: 0 to max(double))
# -i initPoints	10000 (number of points to use for initialization)

# Reclustering:  variant of DBSCAN



DSC_DenStream <- function(epsilon=0.1, beta=0.001, mu=1, 
	initPoints=100, minPoints=10, 
	horizon=1000, lambda=NULL) {
  
  if (horizon < 1)
    stop("invalid horizon, range: >= 1")
  
  if (epsilon <= 0)
    stop("invalid epsilon")

  if (minPoints < 0)
    stop("invalid minPoints, must be > 0")

  if (beta <= 0 || beta >= 1)
    stop("invalid beta, range: 0 to 1 exclusive")

  if (mu <= 0)
    stop("invalid mu, must be > 0")

  if (initPoints < 0)
    stop("invalid initPoints, must be > 0")

  paramList <- list(h = horizon,
	            e = epsilon,
                    p = minPoints,
                    b = beta,
                    m = mu,
                    i = initPoints)

  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)

  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/denstream/DenStream")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")

  if(!is.null(lambda))
  	.jfield(clusterer,"lambda")<-lambda

  # initializing the R object
  l <- list(description = "DenStream",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC_DenStream","DSC_Micro","DSC_MOA","DSC")
  l
}



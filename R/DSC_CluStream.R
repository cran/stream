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


# clustream options:
# IntOption("horizon", 'h', "Rang of the window.", 1000)
# IntOption("maxNumKernels", 'k', "Maximum number of micro kernels to use.", 100);
# IntOption("kernelRadiFactor", 't', "Multiplier for the kernel radius", 2);

# CluStream uses a modified k-means to recluster!
# Modifications:
#
#At the initialization stage, the seeds are no longer
#picked randomly, but are sampled with probability
#proportional to the number of points in a given micro-
#cluster. The corresponding seed is the centroid of that
#micro-cluster.
#
#At the partitioning stage, the distance of a seed
#from a given pseudo-point (or micro-cluster) is equal
#to the distance of the seed from the centroid of the
#corresponding micro-cluster.
#
#At the seed adjustment stage, the new seed for a
#given partition is defined as the weighted centroid of
#the micro-clusters in that partition.
#It is important to note that a give
#

DSC_CluStream <- function(
	horizon=1000, 
	k=100,
	t=2
	) {
  
  if (horizon < 0)
    stop("invalid horizon, must be > 0")

  if (k < 0)
    stop("invalid k, must be > 0")

  paramList <- list(h = as.integer(horizon),
                    k = as.integer(k),
		    t = t)

  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)

  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/clustream/Clustream")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")

  # initializing the R object
  l <- list(description = "CluStream",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC_CluStream","DSC_Micro","DSC_MOA","DSC")
  
  l
}

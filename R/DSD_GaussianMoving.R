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


DSD_GaussianMoving <- function(t = 10,n = 20) {

    state <- new.env()
    assign("counter", 1L, envir = state)

	k <- 3
	

##simulate data
mu <- cbind(
   x = c(0,0,1),
   y = c(0,0,1)
)

#sd_rho <- cbind(
#       x = c(0.2, 0.15, 0.05),
#       y = c(0.1, 0.04, 0.03),
#       rho = c(0, 0.7, 0.3)
#)

sd_rho <- cbind(
       x = c(0.08, 0.08, 0.08),
       y = c(0.08, 0.08, 0.08),
       rho = c(0, 0, 0)
)

Sigma <- lapply(1:nrow(sd_rho), FUN = function(i) rbind(
         c(sd_rho[i,"x"]^2, sd_rho[i, "rho"]*sd_rho[i,"x"]*sd_rho[i,"y"]),
         c(sd_rho[i,"rho"]*sd_rho[i,"x"]*sd_rho[i,"y"],sd_rho[i,"y"]^2))) 

sequence <- c(1,2,3)

EMMsim_sequence <- rep(sequence, n)

library("MASS")

sequ <- c()
ds <- data.frame()

# make a for loop
for (jj in 1:t){
    mu[2,"x"] = mu[2,"x"] + 1/t
    mu[2,"y"] = mu[2,"y"] + 1/t 
    EMMsim <- t(sapply(EMMsim_sequence, FUN = function(i)
		    mvrnorm(1, mu=mu[i,], Sigma=Sigma[[i]])))


    sequ <- append(sequ, EMMsim_sequence)
    ds <- rbind(ds, EMMsim)

}



    # creating the DSD object
    l <- list(description = "Gaussian Moving Stream",
	    strm = ds,
	    state = state,
	    d = ncol(ds),
	    k = k,
	    loop = FALSE,
	    assignment = sequ)
    class(l) <- c("DSD_GaussianMoving","DSD_Wrapper","DSD_R","DSD")
    l
}

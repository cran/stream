#' @title `r packageDescription("stream")$Package`: `r packageDescription("stream")$Title`
#'
#' @description A framework for data stream modeling and associated data mining tasks such as clustering and classification.
#'
#' @author Michael Hahsler
#' @docType package
#' @name stream-package
#'
#' @import Rcpp
#' @import proxy
#' @import magrittr
#' @importFrom methods is new
#' @importFrom dbscan dbscan
#' @importFrom utils head read.table write.table
#' @importFrom stats predict runif rnorm prcomp mahalanobis terms
#' @importFrom graphics plot par layout title pairs points lines image
#' @importFrom grDevices gray gray.colors rgb col2rgb
#' @importFrom mlbench mlbench.2dnormals mlbench.cassini mlbench.circle  mlbench.cuboids mlbench.friedman1  mlbench.friedman2  mlbench.friedman3 mlbench.hypercube  mlbench.peak  mlbench.ringnorm  mlbench.shapes mlbench.simplex mlbench.smiley  mlbench.spirals mlbench.threenorm  mlbench.twonorm  mlbench.waveform mlbench.xor
#'
#' @useDynLib stream, .registration=TRUE
NULL

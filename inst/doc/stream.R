### R code from vignette source 'stream.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: stream.Rnw:135-136
###################################################
options(width = 75, digits = 3, prompt = 'R> ', scipen = 3)


###################################################
### code chunk number 2: stream.Rnw:647-648
###################################################
set.seed(1000)


###################################################
### code chunk number 3: stream.Rnw:651-653
###################################################
library("stream")
stream <- DSD_Gaussians(k = 3, d = 2)


###################################################
### code chunk number 4: stream.Rnw:658-660
###################################################
dstream <- DSC_DStream(gridsize = .1, Cm = 1.2)
update(dstream, stream, n = 500)


###################################################
### code chunk number 5: initial_example
###################################################
km <- DSC_Kmeans(k = 3)
recluster(km, dstream)
plot(km, stream, type = "both")


###################################################
### code chunk number 6: stream.Rnw:875-876
###################################################
set.seed(1000) 


###################################################
### code chunk number 7: stream.Rnw:878-882
###################################################
library("stream")

stream <- DSD_Gaussians(k = 3, d = 3, noise = .05, p = c(.5, .3, .1)) 
stream


###################################################
### code chunk number 8: stream.Rnw:903-905
###################################################
p <- get_points(stream, n = 5) 
p 


###################################################
### code chunk number 9: stream.Rnw:916-918
###################################################
p <- get_points(stream, n = 100, class = TRUE) 
head(p, n = 10)


###################################################
### code chunk number 10: static
###################################################
plot(stream, n = 500)


###################################################
### code chunk number 11: static_pc
###################################################
plot(stream, n = 500, method = "pc")


###################################################
### code chunk number 12: stream.Rnw:968-969
###################################################
set.seed(1000) 


###################################################
### code chunk number 13: moa1
###################################################
stream <- DSD_Benchmark(1)
stream


###################################################
### code chunk number 14: stream.Rnw:981-985 (eval = FALSE)
###################################################
## for(i in 1:4) {
##   plot(stream, 250, xlim = c(0, 1), ylim = c(0, 1))
##   tmp <- get_points(stream, n = 1400)
## }


###################################################
### code chunk number 15: moa1
###################################################
plot(stream, 250, xlim = c(0, 1), ylim = c(0, 1))
arrows(.15, .85, .85, .15, col = rgb(.8, .8, .8, .6), lwd = 10)
arrows(.15, .15, .85, .85, col = rgb(.8, .8, .8, .6), lwd = 10)
tmp <- get_points(stream, n = 1400)


###################################################
### code chunk number 16: moa2
###################################################
plot(stream, 250, xlim = c(0, 1), ylim = c(0, 1))
arrows(.15, .85, .85, .15, col = rgb(.8, .8, .8, .6), lwd = 10)
arrows(.15, .15, .85, .85, col = rgb(.8, .8, .8, .6), lwd = 10)
tmp <- get_points(stream, n=1400)


###################################################
### code chunk number 17: moa3
###################################################
plot(stream, 250, xlim = c(0, 1), ylim = c(0, 1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)
tmp <- get_points(stream, n=1400)


###################################################
### code chunk number 18: moa4
###################################################
plot(stream, 250, xlim=c(0,1), ylim=c(0,1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)


###################################################
### code chunk number 19: stream.Rnw:1037-1040 (eval = FALSE)
###################################################
## reset_stream(stream)
## animate_data(stream, n = 10000, horizon = 100, 
##   xlim = c(0, 1), ylim = c(0, 1))


###################################################
### code chunk number 20: stream.Rnw:1046-1049 (eval = FALSE)
###################################################
## library("animation")
## animation::ani.options(interval = .1)
## ani.replay()


###################################################
### code chunk number 21: stream.Rnw:1056-1058 (eval = FALSE)
###################################################
## saveHTML(ani.replay())
## saveGIF(ani.replay())


###################################################
### code chunk number 22: stream.Rnw:1082-1084
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 23: stream.Rnw:1086-1087
###################################################
stream <- DSD_Gaussians(k = 3, d = 5) 


###################################################
### code chunk number 24: stream.Rnw:1092-1093 (eval = FALSE)
###################################################
## write_stream(stream, "data.csv", n = 100, sep = ",") 


###################################################
### code chunk number 25: stream.Rnw:1121-1125
###################################################
file <- system.file("examples", "kddcup10000.data.gz", package = "stream")
stream_file <- DSD_ReadCSV(gzfile(file),
  take = c(1, 5, 6, 8:11, 13:20, 23:41), class = 42, k = 7)
stream_file


###################################################
### code chunk number 26: stream.Rnw:1137-1138
###################################################
get_points(stream_file, n = 5)


###################################################
### code chunk number 27: stream.Rnw:1145-1147
###################################################
stream_scaled <- DSD_ScaleStream(stream_file, center = TRUE, scale = TRUE)
get_points(stream_scaled, n = 5)


###################################################
### code chunk number 28: stream.Rnw:1177-1179
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 29: stream.Rnw:1181-1183
###################################################
data("EuStockMarkets", package = "datasets")
head(EuStockMarkets)


###################################################
### code chunk number 30: stream.Rnw:1190-1192
###################################################
replayer <- DSD_Memory(EuStockMarkets, k = NA) 
replayer 


###################################################
### code chunk number 31: stream.Rnw:1198-1200
###################################################
get_points(replayer, n = 5)
replayer


###################################################
### code chunk number 32: stream.Rnw:1208-1209 (eval = FALSE)
###################################################
## get_points(replayer, n = 2000)


###################################################
### code chunk number 33: stream.Rnw:1211-1213
###################################################
err <- try(get_points(replayer, n = 2000))
cat(err)


###################################################
### code chunk number 34: stream.Rnw:1226-1228
###################################################
reset_stream(replayer, pos = 100)
replayer


###################################################
### code chunk number 35: stream.Rnw:1527-1528
###################################################
set.seed(1000) 


###################################################
### code chunk number 36: stream.Rnw:1531-1533
###################################################
library("stream") 
stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)


###################################################
### code chunk number 37: stream.Rnw:1541-1543
###################################################
dstream <- DSC_DStream(gridsize = .1, Cm = 1.2) 
dstream


###################################################
### code chunk number 38: stream.Rnw:1551-1553
###################################################
update(dstream, stream, n = 500) 
dstream


###################################################
### code chunk number 39: stream.Rnw:1562-1563
###################################################
head(get_centers(dstream))


###################################################
### code chunk number 40: cluster
###################################################
plot(dstream, stream)


###################################################
### code chunk number 41: cluster-grid
###################################################
plot(dstream, stream, grid = TRUE)


###################################################
### code chunk number 42: stream.Rnw:1846-1850
###################################################
library("stream") 
stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)
dstream <- DSC_DStream(gridsize = .1) 
update(dstream, stream, n = 500) 


###################################################
### code chunk number 43: stream.Rnw:1858-1859
###################################################
evaluate(dstream, stream, n = 100)


###################################################
### code chunk number 44: stream.Rnw:1870-1871
###################################################
evaluate(dstream, stream, measure = c("purity", "crand"), n = 500)


###################################################
### code chunk number 45: stream.Rnw:1909-1910
###################################################
set.seed(1000)


###################################################
### code chunk number 46: stream.Rnw:1912-1917
###################################################
stream <- DSD_Benchmark(1)
dstream <- DSC_DStream(gridsize = .05, lambda = .01)
ev <- evaluate_cluster(dstream, stream, 
  measure = c("numMicroClusters", "purity"), n = 5000, horizon = 100)
head(ev)


###################################################
### code chunk number 47: evaluation
###################################################
plot(ev[ , "points"], ev[ , "purity"], type = "l", 
  ylim = c(0, 1), ylab = "Avg. Purity", xlab = "Points")


###################################################
### code chunk number 48: stream.Rnw:1944-1948 (eval = FALSE)
###################################################
## reset_stream(stream)
## dstream <- DSC_DStream(gridsize = .05, lambda = .01)
## r <- animate_cluster(dstream, stream, n = 5000, horizon = 100, 
##   evaluationMeasure = "purity", xlim = c(0, 1), ylim = c(0, 1))


###################################################
### code chunk number 49: stream.Rnw:1978-1980
###################################################
library("stream")
set.seed(1000) 


###################################################
### code chunk number 50: stream.Rnw:1983-1988
###################################################
stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)
dstream <- DSC_DStream(gridsize = .05, Cm = 1.5)

update(dstream, stream, n = 1000)
dstream


###################################################
### code chunk number 51: recluster
###################################################
plot(dstream, stream, type = "both")


###################################################
### code chunk number 52: recluster2
###################################################
km <- DSC_Kmeans(k = 3, weighted = TRUE)
recluster(km, dstream)
km
plot(km, stream, type = "both") 


###################################################
### code chunk number 53: stream.Rnw:2048-2049
###################################################
evaluate(km, stream, measure = c("purity", "crand", "SSQ"), n = 1000)


###################################################
### code chunk number 54: stream.Rnw:2054-2056
###################################################
evaluate(km, stream, c(measure = "purity", "crand", "SSQ"), n = 1000, 
  assign = "macro")


###################################################
### code chunk number 55: stream.Rnw:2078-2081
###################################################
points <- get_points(stream, n = 100)
assignment <- get_assignment(dstream, points, type = "macro")
assignment


###################################################
### code chunk number 56: silhouette
###################################################
assignment[is.na(assignment)] <- 0L
library("cluster")
plot(silhouette(assignment, dist = dist(points)))


###################################################
### code chunk number 57: stream.Rnw:2174-2189 (eval = FALSE)
###################################################
## DSD_UniformNoise <- function(d = 2, range = NULL) {
##   if(is.null(range)) range <- matrix(c(0, 1), ncol = 2, nrow = d, 
##     byrow = TRUE)
##   structure(list(description = "Uniform Noise Data Stream", d = d, 
##     k = NA_integer_, range = range),
##         class = c("DSD_UniformNoise", "DSD_R", "DSD"))
##   }
##   
## get_points.DSD_UniformNoise <- function(x, n = 1, 
##   assignment = FALSE, ...) {
##     data <- as.data.frame(t(replicate(n, 
##       runif(x$d, min = x$range[ , 1], max = x$range[ , 2]))))
##     if(assignment) attr(data, "assignment") <- rep(NA_integer_, n)
##     data
## }


###################################################
### code chunk number 58: stream.Rnw:2282-2283
###################################################
set.seed(1000) 


###################################################
### code chunk number 59: data_bng
###################################################
library("stream") 
stream <- DSD_Memory(DSD_BarsAndGaussians(noise = .05), n = 1500)
stream
plot(stream)


###################################################
### code chunk number 60: stream.Rnw:2308-2316
###################################################
algorithms <- list(
  'Sample' = DSC_TwoStage(micro = DSC_Sample(k = 100), 
    macro = DSC_Kmeans(k = 4)), 
  'Window' = DSC_TwoStage(micro = DSC_Window(horizon = 100), 
    macro = DSC_Kmeans(k = 4)), 
  'D-Stream' = DSC_DStream(gridsize = .7, Cm = 1.5),
  'tNN' = DSC_tNN(r = .45)
)


###################################################
### code chunk number 61: stream.Rnw:2326-2330
###################################################
for(a in algorithms) {
  reset_stream(stream) 
  update(a, stream, n = 1000)
}


###################################################
### code chunk number 62: stream.Rnw:2335-2336
###################################################
sapply(algorithms, nclusters, type = "micro")


###################################################
### code chunk number 63: microclusters
###################################################
op <- par(no.readonly = TRUE)
layout(mat = matrix(1:length(algorithms), ncol = 2))
for(a in algorithms) {
  reset_stream(stream) 
  plot(a, stream, main = a$description, type = "micro")
}
par(op)


###################################################
### code chunk number 64: microclusters_assignment
###################################################
op <- par(no.readonly = TRUE)
layout(mat = matrix(1:length(algorithms), ncol = 2))
for(a in algorithms) {
  reset_stream(stream) 
  plot(a, stream, main = a$description, 
    assignment = TRUE, weight = FALSE, type = "micro")
}
par(op)


###################################################
### code chunk number 65: stream.Rnw:2414-2421
###################################################
sapply(algorithms, FUN=function(a) {
  reset_stream(stream, pos = 1001) 
  evaluate(a, stream, 
    measure = c("numMicroClusters", "purity"),
    type = "micro",
    n = 500)
})


###################################################
### code chunk number 66: macroclusters
###################################################
op <- par(no.readonly = TRUE)
layout(mat=matrix(1:length(algorithms), ncol = 2))
for(a in algorithms) {
  reset_stream(stream) 
  plot(a, stream, main = a$description, type = "both")
}
par(op)


###################################################
### code chunk number 67: stream.Rnw:2463-2469
###################################################
sapply(algorithms, FUN = function(a) {
  reset_stream(stream, pos = 1001) 
  evaluate(a, stream, measure = c("numMacroClusters", "purity", 
      "SSQ", "cRand", "silhouette"), 
    n = 500, assign = "micro", type = "macro")
})


###################################################
### code chunk number 68: stream.Rnw:2489-2490
###################################################
set.seed(0)


###################################################
### code chunk number 69: stream.Rnw:2493-2494
###################################################
stream <- DSD_Memory(DSD_Benchmark(1), n = 5000)


###################################################
### code chunk number 70: stream.Rnw:2504-2512
###################################################
algorithms <- list(
  'Sample' = DSC_TwoStage(micro = DSC_Sample(k = 100, biased = TRUE), 
    macro = DSC_Kmeans(k = 2)), 
  'Window' = DSC_TwoStage(micro = DSC_Window(horizon = 100, lambda = .01), 
    macro = DSC_Kmeans(k = 2)), 
  'D-Stream' = DSC_DStream(gridsize = .05, lambda = .01),
  'tNN' = DSC_tNN(r = .02, lambda = .01)
)


###################################################
### code chunk number 71: stream.Rnw:2520-2526
###################################################
evaluation <- lapply(algorithms, FUN = function(a) {
  reset_stream(stream) 
  evaluate_cluster(a, stream, 
    type = "macro", assign = "micro", measure = "crand", 
    n = 5000, horizon = 250)
})


###################################################
### code chunk number 72: dynamic
###################################################
Position <- evaluation[[1]][ , "points"]
cRand <- sapply(evaluation, FUN = function(x) x[ , "cRand"])
head(cRand)
matplot(Position, cRand, type = "l", lwd = 2)
legend("bottomleft", legend = names(evaluation), 
  col = 1:6, lty = 1:6, bty = "n", lwd = 2)


###################################################
### code chunk number 73: dynamic_box
###################################################
boxplot(cRand, las = 2)


###################################################
### code chunk number 74: stream.Rnw:2596-2604 (eval = FALSE)
###################################################
## library("stream")
## con <- gzcon(
##   url(paste("http://archive.ics.uci.edu/ml/machine-learning-databases/",
##     "kddcup99-mld/kddcup.data.gz", sep="")))
## 
## stream <- DSD_ReadCSV(con, take=c(1, 5, 6, 8:11, 13:20, 23:41), 
##     class=42, k=7)
## stream2 <- DSD_ScaleStream(stream, n=1000)


###################################################
### code chunk number 75: stream.Rnw:2610-2612 (eval = FALSE)
###################################################
## dstream <- DSC_DStream(gridsize = .5, gaptime = 10000L, lambda=.01)
## update(dstream, stream2, n=4000000, verbose=TRUE)



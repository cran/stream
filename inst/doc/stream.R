### R code from vignette source 'stream.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: stream.Rnw:134-135
###################################################
options(width = 75, digits = 3, prompt = 'R> ', scipen = 3)


###################################################
### code chunk number 2: stream.Rnw:644-645
###################################################
set.seed(1000)


###################################################
### code chunk number 3: stream.Rnw:648-650
###################################################
library("stream")
stream <- DSD_Gaussians(k = 3, d = 2)


###################################################
### code chunk number 4: stream.Rnw:655-657
###################################################
dstream <- DSC_DStream(gridsize = .1)
update(dstream, stream, n = 500)


###################################################
### code chunk number 5: initial_example
###################################################
km <- DSC_Kmeans(k = 3)
recluster(km, dstream)
plot(km, stream, type="both")


###################################################
### code chunk number 6: stream.Rnw:870-871
###################################################
set.seed(1000) 


###################################################
### code chunk number 7: stream.Rnw:873-877
###################################################
library("stream")

stream <- DSD_Gaussians(k = 3, d = 3, noise = .05, p = c(.5, .3, .1)) 
stream


###################################################
### code chunk number 8: stream.Rnw:898-900
###################################################
p <- get_points(stream, n = 5) 
p 


###################################################
### code chunk number 9: stream.Rnw:911-913
###################################################
p <- get_points(stream, n = 100, class = TRUE) 
head(p)


###################################################
### code chunk number 10: static
###################################################
plot(stream, n = 500)


###################################################
### code chunk number 11: static_pc
###################################################
plot(stream, n = 500, method = "pc")


###################################################
### code chunk number 12: stream.Rnw:963-964
###################################################
set.seed(1000) 


###################################################
### code chunk number 13: moa1
###################################################
stream <- DSD_Benchmark(1)
stream


###################################################
### code chunk number 14: stream.Rnw:976-980 (eval = FALSE)
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
### code chunk number 19: stream.Rnw:1032-1035 (eval = FALSE)
###################################################
## reset_stream(stream)
## animate_data(stream, n = 10000, horizon = 100, 
##   xlim = c(0, 1), ylim = c(0, 1))


###################################################
### code chunk number 20: stream.Rnw:1041-1044 (eval = FALSE)
###################################################
## library("animation")
## animation::ani.options(interval = .1)
## ani.replay()


###################################################
### code chunk number 21: stream.Rnw:1051-1053 (eval = FALSE)
###################################################
## saveHTML(ani.replay())
## saveGIF(ani.replay())


###################################################
### code chunk number 22: stream.Rnw:1077-1079
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 23: stream.Rnw:1081-1082
###################################################
stream <- DSD_Gaussians(k = 3, d = 5) 


###################################################
### code chunk number 24: stream.Rnw:1087-1088 (eval = FALSE)
###################################################
## write_stream(stream, "data.csv", n = 100, sep = ",") 


###################################################
### code chunk number 25: stream.Rnw:1116-1120
###################################################
file <- system.file("examples", "kddcup10000.data.gz", package = "stream")
stream_file <- DSD_ReadCSV(gzfile(file),
  take = c(1, 5, 6, 8:11, 13:20, 23:41), class = 42, k = 7)
stream_file


###################################################
### code chunk number 26: stream.Rnw:1133-1134
###################################################
get_points(stream_file, n = 5)


###################################################
### code chunk number 27: stream.Rnw:1141-1143
###################################################
stream_scaled <- DSD_ScaleStream(stream_file, center = TRUE, scale = TRUE)
get_points(stream_scaled, n = 5)


###################################################
### code chunk number 28: stream.Rnw:1173-1175
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 29: stream.Rnw:1177-1179
###################################################
data("EuStockMarkets")
head(EuStockMarkets)


###################################################
### code chunk number 30: stream.Rnw:1186-1188
###################################################
replayer <- DSD_Memory(EuStockMarkets, k = NA) 
replayer 


###################################################
### code chunk number 31: stream.Rnw:1194-1196
###################################################
get_points(replayer, n = 5)
replayer


###################################################
### code chunk number 32: stream.Rnw:1204-1205 (eval = FALSE)
###################################################
## get_points(replayer, n = 2000)


###################################################
### code chunk number 33: stream.Rnw:1207-1209
###################################################
err <- try(get_points(replayer, n = 2000))
cat(err)


###################################################
### code chunk number 34: stream.Rnw:1220-1222
###################################################
reset_stream(replayer, pos = 100)
replayer


###################################################
### code chunk number 35: stream.Rnw:1518-1519
###################################################
set.seed(1000) 


###################################################
### code chunk number 36: stream.Rnw:1522-1524
###################################################
library("stream") 
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)


###################################################
### code chunk number 37: stream.Rnw:1532-1534
###################################################
dstream <- DSC_DStream(gridsize = 0.1) 
dstream


###################################################
### code chunk number 38: stream.Rnw:1542-1544
###################################################
update(dstream, stream, 500) 
dstream


###################################################
### code chunk number 39: stream.Rnw:1553-1554
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
### code chunk number 42: stream.Rnw:1819-1823
###################################################
library("stream") 
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
dstream <- DSC_DStream(gridsize = 0.1) 
update(dstream, stream, 500) 


###################################################
### code chunk number 43: stream.Rnw:1831-1832
###################################################
evaluate(dstream, stream, n = 100)


###################################################
### code chunk number 44: stream.Rnw:1842-1843
###################################################
evaluate(dstream, stream, measure = c("purity", "crand"), n = 500)


###################################################
### code chunk number 45: stream.Rnw:1881-1882
###################################################
set.seed(1000)


###################################################
### code chunk number 46: stream.Rnw:1884-1889
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
### code chunk number 48: stream.Rnw:1916-1920 (eval = FALSE)
###################################################
## reset_stream(stream)
## dstream <- DSC_DStream(gridsize = .05, lambda = .01)
## r <- animate_cluster(dstream, stream, n = 5000, horizon = 100, 
##   evaluationMeasure = "purity", xlim = c(0, 1), ylim = c(0, 1))


###################################################
### code chunk number 49: stream.Rnw:1950-1952
###################################################
library("stream")
set.seed(1000) 


###################################################
### code chunk number 50: stream.Rnw:1955-1960
###################################################
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
dstream <- DSC_DStream(gridsize = .05)

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
### code chunk number 53: stream.Rnw:2020-2021
###################################################
evaluate(km, stream, measure = c("purity", "crand", "SSQ"), n = 1000)


###################################################
### code chunk number 54: stream.Rnw:2026-2028
###################################################
evaluate(km, stream, c(measure = "purity", "crand", "SSQ"), n = 1000, 
  assign = "macro")


###################################################
### code chunk number 55: stream.Rnw:2050-2053
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
### code chunk number 57: stream.Rnw:2145-2160 (eval = FALSE)
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
### code chunk number 58: stream.Rnw:2252-2254
###################################################
set.seed(1000) 
library("stream") 


###################################################
### code chunk number 59: data_bng
###################################################
stream <- DSD_Memory(DSD_BarsAndGaussians(noise = 0.05), n = 1500)
stream
plot(stream)


###################################################
### code chunk number 60: stream.Rnw:2278-2286
###################################################
algorithms <- list(
  'Sample' = DSC_TwoStage(micro = DSC_Sample(k = 100), 
    macro = DSC_Kmeans(k = 4)), 
  'Window' = DSC_TwoStage(micro = DSC_Window(horizon = 100), 
    macro = DSC_Kmeans(k = 4)), 
  'D-Stream' = DSC_DStream(gridsize = .7),
  'tNN' = DSC_tNN(r = .45)
)


###################################################
### code chunk number 61: stream.Rnw:2296-2300
###################################################
for(a in algorithms) {
  reset_stream(stream) 
  update(a, stream, n = 1000)
}


###################################################
### code chunk number 62: stream.Rnw:2305-2306
###################################################
sapply(algorithms, nclusters, type="micro")


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
### code chunk number 65: stream.Rnw:2384-2391
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
### code chunk number 67: stream.Rnw:2433-2439
###################################################
sapply(algorithms, FUN = function(a) {
  reset_stream(stream, pos = 1001) 
  evaluate(a, stream, measure = c("numMacroClusters", "purity", 
      "SSQ", "cRand", "silhouette"), 
    n = 500, assign = "micro", type = "macro")
})


###################################################
### code chunk number 68: stream.Rnw:2459-2460
###################################################
set.seed(0)


###################################################
### code chunk number 69: stream.Rnw:2463-2464
###################################################
stream <- DSD_Memory(DSD_Benchmark(1), n = 5000)


###################################################
### code chunk number 70: stream.Rnw:2474-2482
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
### code chunk number 71: stream.Rnw:2490-2496
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



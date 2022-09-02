### R code from vignette source 'stream.Rnw'

###################################################
### code chunk number 1: options
###################################################
options(width = 75, digits = 3, prompt = 'R> ', scipen = 3)


###################################################
### code chunk number 2: init
###################################################
library("stream")
set.seed(1000)
stream_orig <- DSD_Gaussians(k = 3, d = 2)


###################################################
### code chunk number 3: scale
###################################################
stream <- stream_orig %>% DSF_Scale()


###################################################
### code chunk number 4: simple_DStream
###################################################
dstream <- DSC_DStream(gridsize = .5, Cm = 1.2)
update(dstream, stream, n = 500)


###################################################
### code chunk number 5: simple_kmeans_reclustering
###################################################
km <- DSC_Kmeans(k = 3)
recluster(km, dstream)
plot(km, stream, type = "both")


###################################################
### code chunk number 6: Create_DSD
###################################################
library("stream")
set.seed(1000)

stream <- DSD_Gaussians(k = 3, d = 3, noise = .05, p = c(.5, .3, .1))
stream


###################################################
### code chunk number 7: get_points
###################################################
p <- get_points(stream, n = 10)
p


###################################################
### code chunk number 8: static
###################################################
plot(stream, n = 500)


###################################################
### code chunk number 9: static_pc
###################################################
plot(stream, n = 500, method = "pc")


###################################################
### code chunk number 10: moa1
###################################################
set.seed(1000)
stream <- DSD_Benchmark(1)
stream


###################################################
### code chunk number 11: stream.Rnw:1011-1015 (eval = FALSE)
###################################################
## for(i in 1:4) {
##   plot(stream, 250, xlim = c(0, 1), ylim = c(0, 1))
##   tmp <- get_points(stream, n = 1400)
## }


###################################################
### code chunk number 12: moa1
###################################################
plot(stream, 250, xlim = c(0, 1), ylim = c(0, 1))
arrows(.15, .85, .85, .15, col = rgb(.8, .8, .8, .6), lwd = 10)
arrows(.15, .15, .85, .85, col = rgb(.8, .8, .8, .6), lwd = 10)
tmp <- get_points(stream, n = 1400)


###################################################
### code chunk number 13: moa2
###################################################
plot(stream, 250, xlim = c(0, 1), ylim = c(0, 1))
arrows(.15, .85, .85, .15, col = rgb(.8, .8, .8, .6), lwd = 10)
arrows(.15, .15, .85, .85, col = rgb(.8, .8, .8, .6), lwd = 10)
tmp <- get_points(stream, n=1400)


###################################################
### code chunk number 14: moa3
###################################################
plot(stream, 250, xlim = c(0, 1), ylim = c(0, 1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)
tmp <- get_points(stream, n=1400)


###################################################
### code chunk number 15: moa4
###################################################
plot(stream, 250, xlim=c(0,1), ylim=c(0,1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)


###################################################
### code chunk number 16: stream.Rnw:1068-1071 (eval = FALSE)
###################################################
## reset_stream(stream)
## animate_data(stream, n = 10000, horizon = 100,
##   xlim = c(0, 1), ylim = c(0, 1))


###################################################
### code chunk number 17: stream.Rnw:1077-1080 (eval = FALSE)
###################################################
## library("animation")
## animation::ani.options(interval = .1)
## ani.replay()


###################################################
### code chunk number 18: stream.Rnw:1087-1089 (eval = FALSE)
###################################################
## saveHTML(ani.replay())
## saveGIF(ani.replay())


###################################################
### code chunk number 19: stream.Rnw:1107-1113
###################################################
library("stream")
set.seed(1000)
stream1 <- DSD_Gaussians(k = 3, d = 2, variance_limit = c(0, 0.01),
                         space_limit = c(0, 5))
stream2 <- DSD_Gaussians(k = 3, d = 2, variance_limit = c(.05, .1),
                         space_limit = c(0, 5))


###################################################
### code chunk number 20: dsd-lim1
###################################################
plot(stream1, 1000)


###################################################
### code chunk number 21: dsd-lim2
###################################################
plot(stream2, 1000)


###################################################
### code chunk number 22: stream.Rnw:1142-1148
###################################################
library("stream")
set.seed(1000)
stream1 <- DSD_Gaussians(k = 5, d = 2, variance_limit = c(0.01, 0.03),
                         space_limit = c(0, 7),
                         separation_type = "Mahalanobis",
                         separation = 3)


###################################################
### code chunk number 23: stream.Rnw:1150-1155
###################################################
set.seed(1000)
stream2 <- DSD_Gaussians(k = 5, d = 2, variance_limit = c(0.01, 0.03),
                         space_limit = c(0, 7),
                         separation_type = "Mahalanobis",
                         separation = 10)


###################################################
### code chunk number 24: dsd-ms1
###################################################
plot(stream1, 1000)


###################################################
### code chunk number 25: dsd-ms2
###################################################
plot(stream2, 1000)


###################################################
### code chunk number 26: stream.Rnw:1190-1193
###################################################
library("stream")
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 5)


###################################################
### code chunk number 27: stream.Rnw:1198-1199 (eval = FALSE)
###################################################
## write_stream(stream, "data.csv", n = 100, sep = ",")


###################################################
### code chunk number 28: stream.Rnw:1233-1237
###################################################
file <- system.file("examples", "kddcup10000.data.gz", package = "stream")
stream_file <- DSD_ReadCSV(gzfile(file),
  take = c(1, 5, 6, 8:11, 13:20, 23:41, .class = 42), k = 7)
stream_file


###################################################
### code chunk number 29: stream.Rnw:1250-1251
###################################################
get_points(stream_file, n = 5)


###################################################
### code chunk number 30: stream.Rnw:1258-1260
###################################################
stream_scaled <- DSD_ScaleStream(stream_file, center = TRUE, scale = TRUE)
get_points(stream_scaled, n = 5)


###################################################
### code chunk number 31: stream.Rnw:1291-1293
###################################################
data("EuStockMarkets", package = "datasets")
head(EuStockMarkets)


###################################################
### code chunk number 32: stream.Rnw:1300-1302
###################################################
replayer <- DSD_Memory(EuStockMarkets, k = NA)
replayer


###################################################
### code chunk number 33: stream.Rnw:1308-1310
###################################################
get_points(replayer, n = 5)
replayer


###################################################
### code chunk number 34: stream.Rnw:1317-1319
###################################################
points <- get_points(replayer, n = 2000)
dim(points)


###################################################
### code chunk number 35: stream.Rnw:1334-1336
###################################################
reset_stream(replayer, pos = 100)
replayer


###################################################
### code chunk number 36: stream.Rnw:1691-1694
###################################################
library("stream")
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)


###################################################
### code chunk number 37: stream.Rnw:1702-1704
###################################################
dstream <- DSC_DStream(gridsize = .1, Cm = 1.2)
dstream


###################################################
### code chunk number 38: stream.Rnw:1712-1714
###################################################
update(dstream, stream, n = 500)
dstream


###################################################
### code chunk number 39: stream.Rnw:1723-1724
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
### code chunk number 42: stream.Rnw:2074-2078
###################################################
library("stream")
stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)
dstream <- DSC_DStream(gridsize = .1)
update(dstream, stream, n = 2000)


###################################################
### code chunk number 43: stream.Rnw:2086-2087
###################################################
evaluate_static(dstream, stream, n = 100)


###################################################
### code chunk number 44: stream.Rnw:2098-2099
###################################################
evaluate_static(dstream, stream, measure = c("purity", "crand"), n = 500)


###################################################
### code chunk number 45: stream.Rnw:2140-2146
###################################################
set.seed(1000)
stream <- DSD_Benchmark(1)
dstream <- DSC_DStream(gridsize = .05, lambda = .01)
ev <- evaluate_stream(dstream, stream,
  measure = c("numMicroClusters", "purity"), n = 5000, horizon = 100)
head(ev)


###################################################
### code chunk number 46: evaluation
###################################################
plot(ev[ , "points"], ev[ , "purity"], type = "l",
  ylab = "Avg. Purity", xlab = "Points")


###################################################
### code chunk number 47: stream.Rnw:2179-2184 (eval = FALSE)
###################################################
## set.seed(1000)
## stream <- DSD_Benchmark(1)
## dstream <- DSC_DStream(gridsize = .05, lambda = .01)
## r <- animate_cluster(dstream, stream, horizon = 100, n = 5000,
##      measure = "purity", plot.args = list(xlim = c(0, 1), ylim = c(0, 1)))


###################################################
### code chunk number 48: stream.Rnw:2214-2221
###################################################
library("stream")
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)
dstream <- DSC_DStream(gridsize = .05, Cm = 1.5)

update(dstream, stream, n = 1000)
dstream


###################################################
### code chunk number 49: recluster
###################################################
plot(dstream, stream, type = "both")


###################################################
### code chunk number 50: recluster2
###################################################
km <- DSC_Kmeans(k = 3, weighted = TRUE)
recluster(km, dstream)
km
plot(km, stream, type = "both")


###################################################
### code chunk number 51: stream.Rnw:2282-2283
###################################################
evaluate_static(km, stream, measure = c("purity", "crand", "SSQ"), n = 1000)


###################################################
### code chunk number 52: stream.Rnw:2288-2290
###################################################
evaluate_static(km, stream, c(measure = "purity", "crand", "SSQ"), n = 1000,
  assign = "macro")


###################################################
### code chunk number 53: stream.Rnw:2313-2316
###################################################
points <- get_points(stream, n = 100)
assignment <- get_assignment(dstream, points, type = "macro")
assignment


###################################################
### code chunk number 54: silhouette
###################################################
assignment[is.na(assignment)] <- 0L
library("cluster")
plot(silhouette(assignment, dist = dist(points)))


###################################################
### code chunk number 55: data_bng
###################################################
set.seed(1000)
library("stream")
stream <- DSD_BarsAndGaussians(noise = .05) %>% DSD_Memory(n = 1500)
stream
plot(stream)


###################################################
### code chunk number 56: stream.Rnw:2539-2547
###################################################
algorithms <- list(
  'Sample' = DSC_TwoStage(micro = DSC_Sample(k = 100),
    macro = DSC_Kmeans(k = 4)),
  'Window' = DSC_TwoStage(micro = DSC_Window(horizon = 100),
    macro = DSC_Kmeans(k = 4)),
  'D-Stream' = DSC_DStream(gridsize = .7, Cm = 1.5),
  'DBSTREAM' = DSC_DBSTREAM(r = .45)
)


###################################################
### code chunk number 57: stream.Rnw:2557-2561
###################################################
for(a in algorithms) {
  reset_stream(stream)
  update(a, stream, n = 1000)
}


###################################################
### code chunk number 58: stream.Rnw:2566-2567
###################################################
sapply(algorithms, nclusters, type = "micro")


###################################################
### code chunk number 59: microclusters
###################################################
op <- par(no.readonly = TRUE)
layout(mat = matrix(1:length(algorithms), ncol = 2))
for (a in algorithms) {
  reset_stream(stream)
  plot(a, stream, main = description(a), type = "micro")
}
par(op)


###################################################
### code chunk number 60: microclusters_assignment
###################################################
op <- par(no.readonly = TRUE)
layout(mat = matrix(1:length(algorithms), ncol = 2))
for (a in algorithms) {
  reset_stream(stream)
  plot(
    a,
    stream,
    main = description(a),
    assignment = TRUE,
    weight = FALSE,
    type = "micro"
  )
}
par(op)


###################################################
### code chunk number 61: stream.Rnw:2651-2664
###################################################
sapply(
  algorithms,
  FUN = function(a) {
    reset_stream(stream, pos = 1001)
    evaluate_static(
      a,
      stream,
      measure = c("numMicroClusters", "purity"),
      type = "micro",
      n = 500
    )
  }
)


###################################################
### code chunk number 62: macroclusters
###################################################
op <- par(no.readonly = TRUE)
layout(mat = matrix(1:length(algorithms), ncol = 2))
for (a in algorithms) {
  reset_stream(stream)
  plot(a, stream, main = description(a))
}
par(op)


###################################################
### code chunk number 63: stream.Rnw:2708-2714
###################################################
sapply(algorithms, FUN = function(a) {
  reset_stream(stream, pos = 1001)
  evaluate_static(a, stream, measure = c("numMacroClusters", "purity",
      "SSQ", "cRand", "silhouette"),
    n = 500, assign = "micro", type = "macro")
})


###################################################
### code chunk number 64: stream.Rnw:2736-2738
###################################################
set.seed(0)
stream <- DSD_Memory(DSD_Benchmark(1), n = 5000)


###################################################
### code chunk number 65: stream.Rnw:2748-2756
###################################################
algorithms <- list(
  'Sample + k-means' = DSC_TwoStage(micro = DSC_Sample(k = 100, biased = TRUE),
    macro = DSC_Kmeans(k = 2)),
  'Window + k-means' = DSC_TwoStage(micro = DSC_Window(horizon = 100, lambda = .01),
    macro = DSC_Kmeans(k = 2)),
  'D-Stream' = DSC_DStream(gridsize = .1, lambda = .01),
  'DBSTREAM' = DSC_DBSTREAM(r = .05, lambda = .01)
)


###################################################
### code chunk number 66: stream.Rnw:2767-2772
###################################################
evaluation <- lapply(algorithms, FUN = function(a) {
  reset_stream(stream)
  evaluate_stream(a, stream, horizon = 100, n = 5000, measure = "cRand",
    type = "macro", assign = "micro")
})


###################################################
### code chunk number 67: stream.Rnw:2788-2790
###################################################
cRand <- sapply(evaluation, FUN = function(x) x[ , "cRand"])
head(cRand)


###################################################
### code chunk number 68: dynamic
###################################################
pos <- evaluation[[1]][ , "points"]
matplot(pos, cRand, type = "l", lwd = 1)
legend("bottomleft",  legend = names(evaluation),
  col = 1:6, lty = 1:6, lwd = 1)


###################################################
### code chunk number 69: dynamic_box
###################################################
boxplot(cRand, las = 2, cex.axis = .8)


###################################################
### code chunk number 70: stream.Rnw:2849-2857 (eval = FALSE)
###################################################
## library("stream")
## con <- gzcon(
##   url(paste0("http://archive.ics.uci.edu/ml/machine-learning-databases/",
##     "kddcup99-mld/kddcup.data.gz")))
## 
## stream <- DSD_ReadCSV(con, take=c(1, 5, 6, 8:11, 13:20, 23:42),
##     class = 42, k = 7)
## stream2 <- DSD_ScaleStream(stream, n = 1000)


###################################################
### code chunk number 71: stream.Rnw:2863-2865 (eval = FALSE)
###################################################
## dstream <- DSC_DStream(gridsize = .5, gaptime = 10000L, lambda = .01)
## update(dstream, stream2, n = 4000000, verbose = TRUE)



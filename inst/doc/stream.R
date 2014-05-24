### R code from vignette source 'stream.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: stream.Rnw:951-952
###################################################
set.seed(1000) 


###################################################
### code chunk number 2: stream.Rnw:954-958
###################################################
library("stream")

dsd <- DSD_Gaussians(k=3, d=3, noise=0.05) 
dsd 


###################################################
### code chunk number 3: stream.Rnw:977-979
###################################################
p <- get_points(dsd, n=5) 
p 


###################################################
### code chunk number 4: stream.Rnw:990-992
###################################################
p <- get_points(dsd, n=100, assignment=TRUE) 
attr(p, "assignment") 


###################################################
### code chunk number 5: static
###################################################
plot(dsd, n=500)


###################################################
### code chunk number 6: static_pc
###################################################
plot(dsd, n=500, method="pc")


###################################################
### code chunk number 7: stream.Rnw:1036-1037
###################################################
set.seed(1000) 


###################################################
### code chunk number 8: moa1
###################################################
dsd <- DSD_Benchmark(1)
dsd


###################################################
### code chunk number 9: stream.Rnw:1050-1057 (eval = FALSE)
###################################################
## plot(dsd, 200, xlim=c(0,1), ylim=c(0,1))
## tmp <- get_points(dsd, n=1300)
## plot(dsd, 200, xlim=c(0,1), ylim=c(0,1))
## tmp <- get_points(dsd, n=1300)
## plot(dsd, 200, xlim=c(0,1), ylim=c(0,1))
## tmp <- get_points(dsd, n=1300)
## plot(dsd, 200, xlim=c(0,1), ylim=c(0,1))


###################################################
### code chunk number 10: moa1
###################################################
plot(dsd, 200, xlim=c(0,1), ylim=c(0,1))
tmp <- get_points(dsd, n=1300)


###################################################
### code chunk number 11: moa2
###################################################
plot(dsd, 200, xlim=c(0,1), ylim=c(0,1))
tmp <- get_points(dsd, n=1300)


###################################################
### code chunk number 12: moa3
###################################################
plot(dsd, 200, xlim=c(0,1), ylim=c(0,1))
tmp <- get_points(dsd, n=1300)


###################################################
### code chunk number 13: moa4
###################################################
plot(dsd, 200, xlim=c(0,1), ylim=c(0,1))


###################################################
### code chunk number 14: stream.Rnw:1099-1101 (eval = FALSE)
###################################################
## reset_stream(dsd)
## animate_data(dsd, n=10000, pointInterval=100, xlim=c(0,1), ylim=c(0,1))


###################################################
### code chunk number 15: stream.Rnw:1109-1114 (eval = FALSE)
###################################################
## library(animation)
## animation::ani.options(interval=.1)
## ani.replay()
## saveHTML(ani.replay())
## saveGIF(ani.replay())


###################################################
### code chunk number 16: stream.Rnw:1135-1137
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 17: stream.Rnw:1139-1141
###################################################
dsd <- DSD_Gaussians(k=3, d=5) 
dsd


###################################################
### code chunk number 18: stream.Rnw:1146-1147 (eval = FALSE)
###################################################
## write_stream(dsd, "data.csv", n=100, sep=",") 


###################################################
### code chunk number 19: stream.Rnw:1175-1179
###################################################
file <- system.file("examples", "kddcup10000.data.gz", package="stream")
dsd_file <- DSD_ReadStream(gzfile(file),take=c(1, 5, 6, 8:11, 13:20, 23:41), 
assignment=42, k=7)
dsd_file


###################################################
### code chunk number 20: stream.Rnw:1191-1192
###################################################
get_points(dsd_file,5)


###################################################
### code chunk number 21: stream.Rnw:1221-1223
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 22: stream.Rnw:1225-1228
###################################################
dsd <- DSD_Gaussians(k=3, d=2) 
p <- get_points(dsd, 100)
head(p)


###################################################
### code chunk number 23: stream.Rnw:1234-1236
###################################################
replayer <- DSD_Wrapper(p, k=3) 
replayer 


###################################################
### code chunk number 24: stream.Rnw:1242-1244
###################################################
get_points(replayer, n=5)
replayer


###################################################
### code chunk number 25: stream.Rnw:1252-1253 (eval = FALSE)
###################################################
## get_points(replayer, n = 1000)


###################################################
### code chunk number 26: stream.Rnw:1255-1257
###################################################
err <- try(get_points(replayer, n = 1000))
cat(err)


###################################################
### code chunk number 27: stream.Rnw:1267-1269
###################################################
reset_stream(replayer)
replayer


###################################################
### code chunk number 28: stream.Rnw:1278-1280
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 29: stream.Rnw:1282-1284
###################################################
dsd <- DSD_Gaussians(k=3, d=2, noise=0.05)
dsd


###################################################
### code chunk number 30: stream.Rnw:1291-1293
###################################################
dstream <- DSC_DStream(gridsize=0.1) 
dstream


###################################################
### code chunk number 31: stream.Rnw:1301-1303
###################################################
cluster(dstream, dsd, 500) 
dstream


###################################################
### code chunk number 32: stream.Rnw:1312-1313
###################################################
head(get_centers(dstream))


###################################################
### code chunk number 33: cluster
###################################################
plot(dstream, dsd)


###################################################
### code chunk number 34: stream.Rnw:1343-1344
###################################################
evaluate(dstream, dsd, n = 500)


###################################################
### code chunk number 35: stream.Rnw:1351-1352
###################################################
evaluate(dstream, dsd, method = c("purity", "crand"), n = 500)


###################################################
### code chunk number 36: stream.Rnw:1370-1371
###################################################
set.seed(1000)


###################################################
### code chunk number 37: stream.Rnw:1373-1378
###################################################
dsd <- DSD_Benchmark(1)
micro <- DSC_DStream(gridsize=.05, lambda=.01)
ev <- evaluate_cluster(micro, dsd, method=c("numMicroClusters","purity"), 
  n=5000, horizon=100)
head(ev)


###################################################
### code chunk number 38: evaluation
###################################################
plot(ev[,"points"], ev[,"purity"], type="l", 
  ylim=c(0,1), ylab="Avg. Purity", xlab="Points")


###################################################
### code chunk number 39: stream.Rnw:1404-1409 (eval = FALSE)
###################################################
## reset_stream(dsd)
## micro <- DSC_DStream(gridsize=.05, lambda=.01)
## r <- animate_cluster(micro, dsd, evaluationMethod="purity", n=5000, 
##     horizon=100, pointInterval=100,
##     xlim=c(0,1), ylim=c(0,1))


###################################################
### code chunk number 40: stream.Rnw:1423-1427 (eval = FALSE)
###################################################
## library(animation)
## animation::ani.options(interval=.1)
## ani.replay()
## saveHTML(ani.replay())


###################################################
### code chunk number 41: stream.Rnw:1436-1438
###################################################
library("stream")
set.seed(1000) 


###################################################
### code chunk number 42: stream.Rnw:1441-1446
###################################################
dsd <- DSD_Gaussians(k=3, d=2, noise=0.05)
dstream <- DSC_DStream(gridsize=.1)

cluster(dstream, dsd, 1000)
dstream


###################################################
### code chunk number 43: recluster
###################################################
km <- DSC_Kmeans(k=3, weighted=TRUE)
recluster(km, dstream)
km
plot(km, dsd) 


###################################################
### code chunk number 44: recluster2
###################################################
plot(km, dsd, type="both") 


###################################################
### code chunk number 45: stream.Rnw:1493-1494
###################################################
evaluate(km, dsd, method=c("purity", "crand", "SSQ"), n=500)


###################################################
### code chunk number 46: stream.Rnw:1499-1500
###################################################
evaluate(km, dsd, c(method="purity", "crand", "SSQ"), n=500, assign="macro")


###################################################
### code chunk number 47: stream.Rnw:1754-1763 (eval = FALSE)
###################################################
## DSD_UniformNoise <- function(d=2)  
##     structure(list(description = "Uniform Noise Data Stream", d = d),
##               class=c("DSD_UniformNoise","DSD_R","DSD"))
## 
## get_points.DSD_UniformNoise <- function(x, n=1, assignment = FALSE, ...) {
##     data <- as.data.frame(t(replicate(n, runif(x$d))))
##     if(assignment) attr(data, "assignment") <- rep(NA, n)
##     data
## }



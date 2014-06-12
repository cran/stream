### R code from vignette source 'stream.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: stream.Rnw:112-113
###################################################
options(width = 75, digits = 3)


###################################################
### code chunk number 2: stream.Rnw:1183-1184
###################################################
set.seed(1000) 


###################################################
### code chunk number 3: stream.Rnw:1186-1190
###################################################
library("stream")

dsd <- DSD_Gaussians(k=3, d=3, noise=0.05) 
dsd 


###################################################
### code chunk number 4: stream.Rnw:1210-1212
###################################################
p <- get_points(dsd, n=5) 
p 


###################################################
### code chunk number 5: stream.Rnw:1223-1225
###################################################
p <- get_points(dsd, n=100, assignment=TRUE) 
attr(p, "assignment") 


###################################################
### code chunk number 6: static
###################################################
plot(dsd, n=500)


###################################################
### code chunk number 7: static_pc
###################################################
plot(dsd, n=500, method="pc")


###################################################
### code chunk number 8: stream.Rnw:1273-1274
###################################################
set.seed(1000) 


###################################################
### code chunk number 9: moa1
###################################################
dsd <- DSD_Benchmark(1)
dsd


###################################################
### code chunk number 10: stream.Rnw:1286-1290 (eval = FALSE)
###################################################
## for(i in 1:4) {
##   plot(dsd, 250, xlim=c(0,1), ylim=c(0,1))
##   tmp <- get_points(dsd, n=1400)
## }


###################################################
### code chunk number 11: moa1
###################################################
plot(dsd, 250, xlim=c(0,1), ylim=c(0,1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)
tmp <- get_points(dsd, n=1400)


###################################################
### code chunk number 12: moa2
###################################################
plot(dsd, 250, xlim=c(0,1), ylim=c(0,1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)
tmp <- get_points(dsd, n=1400)


###################################################
### code chunk number 13: moa3
###################################################
plot(dsd, 250, xlim=c(0,1), ylim=c(0,1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)
tmp <- get_points(dsd, n=1400)


###################################################
### code chunk number 14: moa4
###################################################
plot(dsd, 250, xlim=c(0,1), ylim=c(0,1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)


###################################################
### code chunk number 15: stream.Rnw:1342-1344 (eval = FALSE)
###################################################
## reset_stream(dsd)
## animate_data(dsd, n=10000, horizon=100, xlim=c(0,1), ylim=c(0,1))


###################################################
### code chunk number 16: stream.Rnw:1351-1356 (eval = FALSE)
###################################################
## library(animation)
## animation::ani.options(interval=.1)
## ani.replay()
## saveHTML(ani.replay())
## saveGIF(ani.replay())


###################################################
### code chunk number 17: stream.Rnw:1380-1382
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 18: stream.Rnw:1384-1385
###################################################
dsd <- DSD_Gaussians(k=3, d=5) 


###################################################
### code chunk number 19: stream.Rnw:1390-1391 (eval = FALSE)
###################################################
## write_stream(dsd, "data.csv", n=100, sep=",") 


###################################################
### code chunk number 20: stream.Rnw:1419-1423
###################################################
file <- system.file("examples", "kddcup10000.data.gz", package="stream")
dsd_file <- DSD_ReadStream(gzfile(file),take=c(1, 5, 6, 8:11, 13:20, 23:41), 
assignment=42, k=7)
dsd_file


###################################################
### code chunk number 21: stream.Rnw:1436-1437
###################################################
get_points(dsd_file, n=5)


###################################################
### code chunk number 22: stream.Rnw:1444-1446
###################################################
dsd_scaled <- DSD_ScaleStream(dsd_file, center=TRUE, scale=TRUE)
get_points(dsd_scaled, n=5)


###################################################
### code chunk number 23: stream.Rnw:1475-1477
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 24: stream.Rnw:1479-1481
###################################################
data(EuStockMarkets)
head(EuStockMarkets)


###################################################
### code chunk number 25: stream.Rnw:1488-1490
###################################################
replayer <- DSD_Wrapper(EuStockMarkets, k=NA) 
replayer 


###################################################
### code chunk number 26: stream.Rnw:1496-1498
###################################################
get_points(replayer, n=5)
replayer


###################################################
### code chunk number 27: stream.Rnw:1506-1507 (eval = FALSE)
###################################################
## get_points(replayer, n = 2000)


###################################################
### code chunk number 28: stream.Rnw:1509-1511
###################################################
err <- try(get_points(replayer, n = 2000))
cat(err)


###################################################
### code chunk number 29: stream.Rnw:1521-1523
###################################################
reset_stream(replayer, pos=100)
replayer


###################################################
### code chunk number 30: stream.Rnw:1539-1541
###################################################
library("stream") 
set.seed(1000) 


###################################################
### code chunk number 31: stream.Rnw:1543-1544
###################################################
dsd <- DSD_Gaussians(k=3, d=2, noise=0.05)


###################################################
### code chunk number 32: stream.Rnw:1551-1553
###################################################
dstream <- DSC_DStream(gridsize=0.1) 
dstream


###################################################
### code chunk number 33: stream.Rnw:1561-1563
###################################################
cluster(dstream, dsd, 500) 
dstream


###################################################
### code chunk number 34: stream.Rnw:1572-1573
###################################################
get_centers(dstream)


###################################################
### code chunk number 35: cluster
###################################################
plot(dstream, dsd)


###################################################
### code chunk number 36: cluster-grid
###################################################
plot(dstream, dsd, grid=TRUE)


###################################################
### code chunk number 37: stream.Rnw:1622-1623
###################################################
evaluate(dstream, dsd, n = 500)


###################################################
### code chunk number 38: stream.Rnw:1633-1634
###################################################
evaluate(dstream, dsd, measure = c("purity", "crand"), n = 500)


###################################################
### code chunk number 39: stream.Rnw:1665-1666
###################################################
set.seed(1000)


###################################################
### code chunk number 40: stream.Rnw:1668-1673
###################################################
dsd <- DSD_Benchmark(1)
micro <- DSC_DStream(gridsize=.05, lambda=.01)
ev <- evaluate_cluster(micro, dsd, measure=c("numMicroClusters","purity"), 
  n=5000, horizon=100)
head(ev)


###################################################
### code chunk number 41: evaluation
###################################################
plot(ev[,"points"], ev[,"purity"], type="l", 
  ylim=c(0,1), ylab="Avg. Purity", xlab="Points")


###################################################
### code chunk number 42: stream.Rnw:1700-1704 (eval = FALSE)
###################################################
## reset_stream(dsd)
## micro <- DSC_DStream(gridsize=.05, lambda=.01)
## r <- animate_cluster(micro, dsd, n=5000, 
##     horizon=100, evaluationMeasure="purity", xlim=c(0,1), ylim=c(0,1))


###################################################
### code chunk number 43: stream.Rnw:1733-1735
###################################################
library("stream")
set.seed(1000) 


###################################################
### code chunk number 44: stream.Rnw:1738-1743
###################################################
dsd <- DSD_Gaussians(k=3, d=2, noise=0.05)
dstream <- DSC_DStream(gridsize=.05)

cluster(dstream, dsd, 1000)
dstream


###################################################
### code chunk number 45: recluster
###################################################
plot(dstream, dsd, type="both")


###################################################
### code chunk number 46: recluster2
###################################################
km <- DSC_Kmeans(k=3, weighted=TRUE)
recluster(km, dstream)
km
plot(km, dsd, type="both") 


###################################################
### code chunk number 47: stream.Rnw:1803-1804
###################################################
evaluate(km, dsd, measure=c("purity", "crand", "SSQ"), n=1000)


###################################################
### code chunk number 48: stream.Rnw:1809-1810
###################################################
evaluate(km, dsd, c(measure="purity", "crand", "SSQ"), n=1000, assign="macro")


###################################################
### code chunk number 49: stream.Rnw:2078-2091 (eval = FALSE)
###################################################
## DSD_UniformNoise <- function(d=2, range=NULL) {
##   if(is.null(range)) range <- matrix(c(0,1), ncol=2, nrow=d, byrow=TRUE)
##   structure(list(description = "Uniform Noise Data Stream", d = d, 
##     k=NA_integer_, range=range),
##         class=c("DSD_UniformNoise","DSD_R","DSD"))
##   }
##   
## get_points.DSD_UniformNoise <- function(x, n=1, assignment = FALSE, ...) {
##     data <- as.data.frame(t(replicate(n, 
##       runif(x$d, min=x$range[,1], max=x$range[,2]))))
##     if(assignment) attr(data, "assignment") <- rep(NA_integer_, n)
##     data
## }



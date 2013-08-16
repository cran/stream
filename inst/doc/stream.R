### R code from vignette source 'stream.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: stream.Rnw:938-943
###################################################
library("stream") 
set.seed(1000) 

dsd <- DSD_GaussianStatic(k=3, d=2, noise=0.05) 
dsd 


###################################################
### code chunk number 2: stream.Rnw:962-964
###################################################
p <- get_points(dsd, n=5) 
p 


###################################################
### code chunk number 3: stream.Rnw:975-977
###################################################
p <- get_points(dsd, n=100, assignment=TRUE) 
attr(p, "assignment") 


###################################################
### code chunk number 4: static
###################################################
plot(dsd, n=500)


###################################################
### code chunk number 5: moa1
###################################################
set.seed(1000) 
dsd <- DSD_RandomRBFGeneratorEvents(k=3, d=2) 
dsd


###################################################
### code chunk number 6: stream.Rnw:1016-1020 (eval = FALSE)
###################################################
## plot(dsd, 500)
## plot(dsd, 500)
## plot(dsd, 500)
## plot(dsd, 500)


###################################################
### code chunk number 7: moa1
###################################################
plot(get_points(dsd, 500)) 


###################################################
### code chunk number 8: moa2
###################################################
plot(get_points(dsd, 500)) 


###################################################
### code chunk number 9: moa3
###################################################
plot(get_points(dsd, 500)) 


###################################################
### code chunk number 10: moa4
###################################################
plot(get_points(dsd, 500)) 


###################################################
### code chunk number 11: stream.Rnw:1059-1060 (eval = FALSE)
###################################################
## data_animation(dsd, 5000)


###################################################
### code chunk number 12: stream.Rnw:1078-1082
###################################################
library("stream") 
set.seed(1000) 
dsd <- DSD_GaussianStatic(k=3, d=5) 
dsd


###################################################
### code chunk number 13: stream.Rnw:1087-1088
###################################################
write_stream(dsd, "data.csv", n=100, sep=",") 


###################################################
### code chunk number 14: stream.Rnw:1112-1116
###################################################
file <- system.file("examples", "kddcup10000.data.gz", package="stream")
dsd_file <- DSD_ReadStream(gzfile(file),take=c(1, 5, 6, 8:11, 13:20, 23:41), 
assignment=42, k=7)
dsd_file


###################################################
### code chunk number 15: stream.Rnw:1122-1123
###################################################
get_points(dsd_file,5)


###################################################
### code chunk number 16: stream.Rnw:1151-1156
###################################################
library("stream") 
set.seed(1000) 
dsd <- DSD_GaussianStatic(k=3, d=2) 
p <- get_points(dsd, 100)
head(p)


###################################################
### code chunk number 17: stream.Rnw:1162-1164
###################################################
replayer <- DSD_Wrapper(p) 
replayer 


###################################################
### code chunk number 18: stream.Rnw:1170-1172
###################################################
get_points(replayer, n=5)
replayer


###################################################
### code chunk number 19: stream.Rnw:1179-1180 (eval = FALSE)
###################################################
## get_points(replayer,n = 1000)


###################################################
### code chunk number 20: stream.Rnw:1190-1192
###################################################
reset_stream(replayer)
replayer


###################################################
### code chunk number 21: stream.Rnw:1201-1205
###################################################
library("stream") 
set.seed(1000) 
dsd <- DSD_GaussianStatic(k=3, d=2, noise=0.05)
dsd


###################################################
### code chunk number 22: stream.Rnw:1212-1214
###################################################
birch <- DSC_BIRCH(radius=0.01) 
birch


###################################################
### code chunk number 23: stream.Rnw:1222-1224
###################################################
cluster(birch, dsd, 500) 
birch


###################################################
### code chunk number 24: stream.Rnw:1230-1231
###################################################
head(get_centers(birch))


###################################################
### code chunk number 25: cluster
###################################################
plot(birch, dsd)


###################################################
### code chunk number 26: stream.Rnw:1258-1265
###################################################
library("stream")
dsd <- DSD_GaussianStatic(k=3, d=2, mu=rbind(c(1.5,1.3),c(1,1),c(1.2,1)))

tnn <- DSC_tNN(, r=.1, macro=FALSE)
cluster(tnn, dsd, 500)

evaluate(tnn, dsd, n = 500)


###################################################
### code chunk number 27: stream.Rnw:1269-1270
###################################################
evaluate(tnn, dsd, method = c("purity", "crand"), n = 500)


###################################################
### code chunk number 28: stream.Rnw:1285-1289
###################################################
dsd <- DSD_GaussianMoving()
micro <- DSC_DenStream(initPoints=100)
evaluate_cluster(micro, dsd, method=c("purity","crand"), n=600, horizon= 100, 
                 assign="micro")


###################################################
### code chunk number 29: stream.Rnw:1292-1297
###################################################
reset_stream(dsd)
micro <- DSC_DenStream(initPoints=100)
macro <- DSC_Kmeans(k=3)
evaluate_cluster(micro, dsd, macro, method=c("purity","crand"), n=600, 
                 horizon=100, assign="micro")


###################################################
### code chunk number 30: stream.Rnw:1307-1314
###################################################
library("stream")

dsd <- DSD_GaussianStatic(k=3, d=2, mu=rbind(c(1.5,1.3),c(1,1),c(1.2,1)))

tnn <- DSC_tNN(r=.04, macro=FALSE)
cluster(tnn, dsd, 1000)
tnn


###################################################
### code chunk number 31: stream.Rnw:1323-1326
###################################################
km <- DSC_Kmeans(k=3)
recluster(km, tnn)
km


###################################################
### code chunk number 32: stream.Rnw:1331-1332
###################################################
evaluate(km, dsd,"purity")


###################################################
### code chunk number 33: recluster
###################################################
plot(km, dsd) 


###################################################
### code chunk number 34: stream.Rnw:1365-1370
###################################################
library("stream") 
set.seed(1000) 
d <- get_points(DSD_GaussianStatic(k=3, d=2, noise=0.01), n=1000,
assignment=TRUE) 
head(d)


###################################################
### code chunk number 35: stream.Rnw:1373-1375
###################################################
dsd <- DSD_Wrapper(d, k=3) 
dsd


###################################################
### code chunk number 36: stream.Rnw:1383-1385
###################################################
denstream <- DSC_DenStream() 
clustream <- DSC_CluStream() 


###################################################
### code chunk number 37: stream.Rnw:1388-1391
###################################################
cluster(denstream, dsd, 1000) 
reset_stream(dsd) 
cluster(clustream, dsd, 1000)


###################################################
### code chunk number 38: stream.Rnw:1394-1396
###################################################
denstream
clustream


###################################################
### code chunk number 39: denstream
###################################################
reset_stream(dsd) 
plot(denstream,dsd) 


###################################################
### code chunk number 40: clustream
###################################################
reset_stream(dsd) 
plot(clustream,dsd) 


###################################################
### code chunk number 41: stream.Rnw:1431-1435
###################################################
km_denstream <- DSC_Kmeans(k=3)
recluster(km_denstream, denstream)
km_clustream <- DSC_Kmeans(k=3)
recluster(km_clustream, clustream)


###################################################
### code chunk number 42: denstreamkmeans
###################################################
reset_stream(dsd) 
plot(km_denstream,dsd) 


###################################################
### code chunk number 43: clustreamkmeans
###################################################
reset_stream(dsd) 
plot(km_clustream, dsd) 


###################################################
### code chunk number 44: stream.Rnw:1468-1473
###################################################
sapply(list(DenStream=km_denstream, CluStream=km_clustream), 
FUN=function(x){
    reset_stream(dsd)
    evaluate(x, dsd, c("purity","rand", "crand"))
})



library(haven)
data.2.pca <- read.table("D:/data.2.pca.txt", quote="\"",comment.char="",header=TRUE)
dim(data.2.pca)
str(data.2.pca)
head(data.2.pca)

data.1<-data.2.pca[,1:2]
colMeans(data.1)
var(data.1)
cor(data.1)

library(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("bar.emf")
# produce the desired graph(s)
plot(data.1,asp=1)
dev.off() #turn off device and finalize file
# }

PCA.data.1 <- prcomp(data.1)
objects(data.1)

PCA.data.1$scale
PCA.data.1$rotation
PCA.data.1$sdev

PC.skor.data.1 <- predict(PCA.data.1)
colMeans(PC.skor.data.1)
var(PC.skor.data.1)
cor(PC.skor.data.1)

screeplot(PCA.data.1)
screeplot(PCA.data.1, type = "lines")

emf("bar.emf")
# produce the desired graph(s)
plot(PC.skor.data.1,asp=1,xlim=c(-20,20),ylim=c(-20,20))
dev.off() #turn off device and finalize file

emf("bar.emf")
# produce the desired graph(s)
screeplot(PCA.data.1, type = "lines")
dev.off() #turn off device and finalize file

proporsi.keragaman.PC.2 <- PCA.data.1$sdev[1]^2/sum(PCA.data.1$sdev^2)




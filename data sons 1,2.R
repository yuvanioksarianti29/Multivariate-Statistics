library(haven)
T3_8_SONS <- read.table("D:/T3_8_SONS.DAT", quote="\"",comment.char="")
Data.Sons <- T3_8_SONS[,1:2]

colnames(Data.Sons) <- c("panjang.kepala","lebar.kepala")
dim(Data.Sons)
str(Data.Sons)
head(Data.Sons)

colMeans(Data.Sons)
var(Data.Sons)
cor(Data.Sons)

library(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("bar.emf")
# produce the desired graph(s)
plot(Data.Sons,asp=1)
dev.off() #turn off device and finalize file
# }

PCA.Data.Sons <- prcomp(Data.Sons)
objects(PCA.Data.Sons)

PCA.Data.Sons$scale
PCA.Data.Sons$rotation
PCA.Data.Sons$sdev

PC.skor.Data.Sons <- predict(PCA.Data.Sons)
colMeans(PC.skor.Data.Sons)
var(PC.skor.Data.Sons)
cor(PC.skor.Data.Sons)

screeplot(PCA.Data.Sons)
screeplot(PCA.Data.Sons, type = "lines")

emf("bar.emf")
# produce the desired graph(s)
plot(PC.skor.Data.Sons,asp=1,xlim=c(-20,20),ylim=c(-20,20))
dev.off() #turn off device and finalize file

emf("bar.emf")
# produce the desired graph(s)
screeplot(PCA.Data.Sons, type = "lines")
dev.off() #turn off device and finalize file

proporsi.keragaman.PC.1 <- PCA.Data.Sons$sdev[1]^2/sum(PCA.Data.Sons$sdev^2)
proporsi.keragaman.PC.2 <- PCA.Data.Sons$rotation[1]^2/sum(PCA.Data.Sons$rotation^2)
proporsi.keragaman.PC.3 <- PCA.Data.Sons$scale[1]^2/sum(PCA.Data.Sons$scale^2)

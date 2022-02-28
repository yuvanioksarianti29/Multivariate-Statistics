data.2.pca <- read.csv ("D:/data.2.pca.txt", sep="")
PCA.1 <- prcomp(data.2.pca)
data.2.pca
colMeans(data.2.pca)
cor(data.2.pca)
var(data.2.pca)
objects (PCA.1)
dim(data.2.pca)
str(data.2.pca)
head(data.2.pca)
screeplot(PCA.1, type="lines")


PCA.1$center
PCA.1$rotation
PCA.1$scale
PCA.1$sdev
PCA.1$x
proporsi.keragaman.PC.1 <- data.2.pca$sdev[1]^2/sum(data.2.pca$sdev^2)

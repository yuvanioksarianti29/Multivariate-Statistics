library(CCA)
library(CCP)
require(ggplot2)
require(GGally)
mm <- read.csv("D:/sales.txt", sep="")
colnames(mm) <- c("SalesGrowth", "SalesProfitability", "NewAccountSales", "Creativity", "MechanicalReasoning", 
                  "AbstractReasoning", "Mathematics")
summary(mm)
save(mm,file="D:/data.korelasi.kanonik.Sales.Rdata")
load("D:/data.korelasi.kanonik.Sales.Rdata")

SalesPerformance <- mm[, 1:3]
Intelligent <- mm[, 4:7]
ggpairs(SalesPerformance)
ggpairs(Intelligent)

result.cca <- cc(SalesPerformance, Intelligent)
cc1 <- result.cca
# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(SalesPerformance)[1]
p <- length(SalesPerformance)
q <- length(Intelligent)

# Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")
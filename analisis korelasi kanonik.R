
require(ggplot2)
require(GGally)
require(CCA)
mm <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
summary(mm)

#Canonical correlation analysis#
xtabs(~Sex, data = mm)

psych <- mm[, 1:3]
acad <- mm[, 4:8]

ggpairs(psych)
ggpairs(acad)

#Selanjutnya, kita akan melihat korelasi di dalam dan di 
#antara dua set variabel menggunakan fungsi matcor dari paket CCA

# correlations
matcor(psych, acad)

##R Canonical Correlation Analysis##

cc1 <- cc(psych, acad)

#display the canonical correlations
cc1$cor
# raw canonical coefficients
cc1[3:4]

# compute canonical loadings
cc2 <- comput(psych, acad, cc1)

# display canonical loadings
cc2[3:6]

library(CCP)
# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(psych)[1]
p <- length(psych)
q <- length(acad)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
p.asym(rho, n, p, q, tstat = "Roy")

# standardized psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(psych))))
s1 %*% cc1$xcoef

# standardized acad canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(acad))))
s2 %*% cc1$ycoef
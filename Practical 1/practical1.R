# ----Exploratory Analysis----
# 1. Load the data set (which can be found in Moodle) into R.----
windFarm <- read.csv('C:\\Users\\sumen\\Desktop\\MT5757\\Practical 1\\data.csv')

windFarm$density <- windFarm$Nhat/windFarm$are

attach(windFarm)

head(windFarm)

str(windFarm)

# 2. Plot histograms and boxplots for the estimated abundance per unit area pre and post impact.----
hist(windFarm$density, col=2, breaks = rep(50))
hist(windFarm$density)
require(lattice)
histogram( ~density|as.factor(impact), xlab = 'Density values by impact category',
           main = 'Distribution of density by impact category')

boxplot(density)
boxplot(density~impact)

# 3. Calculate confidence intervals ----
# a
mu1 <- mean(density[impact == 0])
mu2 <- mean(density[impact == 1])
n1 <- length(density[impact == 0])
n2 <- length(density[impact == 1])
t <- qt(0.025,n-1)
s1 <- sd(density[impact == 0])
s2 <- sd(density[impact == 1])
# s2 <- sqrt(var(density[impact == 1]))
ciNormal1 <- c((mu1+t*s1/sqrt(n1)),(mu1-t*s1/sqrt(n1)))
ciNormal2 <- c((mu2+t*s2/sqrt(n2)),(mu2-t*s2/sqrt(n2)))
ciNormal1
ciNormal2
# b
z <- qnorm(0.025)
ciPois1 <- c((mu1+z*sqrt(mu1/n1)),(mu1-z*sqrt(mu1/n1)))
ciPois2 <- c((mu2+z*sqrt(mu2/n2)),(mu2-z*sqrt(mu2/n2)))
ciPois1
ciPois2
# c
results<- matrix(0, nrow=1000, ncol=1) 
for(j in 1:1000){
  rowsToUse<-sort(sample(which(impact==0),length(which(impact==0)), replace=T))
  results[j,]<- mean(Nhat[rowsToUse]/area[rowsToUse])
}
cisBoot1<-quantile(results, probs=c(0.025,0.975))
for(j in 1:1000){
  rowsToUse<-sort(sample(which(impact==1),length(which(impact==1)), replace=T))
  results[j,]<- mean(Nhat[rowsToUse]/area[rowsToUse])
}
cisBoot2<-quantile(results, probs=c(0.025,0.975))
cisBoot1
cisBoot2

ci <- matrix(0, nrow = 100, ncol=2)
for(i in 1:100){
  for(j in 1:1000){
    rowsToUse<-sort(sample(which(impact==0),length(which(impact==0)), replace=T))
    results[j,]<- mean(Nhat[rowsToUse]/area[rowsToUse])
  }
  ci[i]<-quantile(results, probs=c(0.025,0.975))
}

# 4. Plot the data in x/y space ----
par(mfrow=c(1,1))
plot(x.pos, y.pos, xlab="X-coordinate", ylab="Y-coordinate", main="Transect lines", pch=20, col="lightgrey")
points(x.pos, y.pos, pch=20, col="blue", cex=log(Nhat+1))

par(mfrow=c(1,2))
plot(x.pos[impact==0], y.pos[impact==0], xlab="X-coordinate", ylab="Y-coordinate", main="Pre impact", pch=20, col="lightgrey")
points(x.pos[impact==0], y.pos[impact==0], pch=1, col="blue", cex=log(Nhat[impact==0]+1))
plot(x.pos[impact==1], y.pos[impact==1], xlab="X-coordinate", ylab="Y-coordinate", main="Post impact", pch=20, col="lightgrey")
points(x.pos[impact==1], y.pos[impact==1], pch=1, col="blue", cex=log(Nhat[impact==0]+1))


# ----Model Fitting----
# 1. the impact covariate alone ----
require(car)

glmFit1<- glm(Nhat ~ impact, data=windFarm, offset=log(area), family=poisson)
Anova(glmFit1)

glmFitOD1<- glm(Nhat ~ impact, data=windFarm, offset=log(area), family=quasipoisson)
Anova(glmFitOD1, test="F")

summary(glmFit1)
summary(glmFitOD1)

# 2. impact, Depth, x.pos and y.pos covariates ----
glmFit2<- glm(Nhat ~ impact + Depth + x.pos + y.pos, data=windFarm, offset=log(area), family=poisson)
Anova(glmFit2)

glmFitOD2<- glm(Nhat ~ impact + Depth + x.pos + y.pos, data=windFarm, offset=log(area), family=quasipoisson)
Anova(glmFitOD2, test="F")

summary(glmFit2)
summary(glmFitOD2)

# 3. impact, Depth, x.pos, y.pos and interaction terms between impact and each of x.pos and y.pos ----
glmFit3<- glm(Nhat ~ impact + Depth + x.pos + y.pos + impact*x.pos + impact*y.pos, data=windFarm, offset=log(area), family=poisson)
Anova(glmFit3)
summary(glmFit3)

glmFitOD3<- glm(Nhat ~ impact + Depth + x.pos + y.pos + impact*x.pos + impact*y.pos, data=windFarm, offset=log(area), family=quasipoisson)
Anova(glmFitOD3, test="F")
summary(glmFitOD3)

# ----Model Selection----
# 1.dredge----
require(MuMIn)
options(na.action='na.fail')
dredge <- dredge(glmFit3, rank = "QAIC", chat = summary(glmFitOD3)$dispersion)

# 2.stepwise ----
step.pois3 <- step(glmFit3, direction = "both")



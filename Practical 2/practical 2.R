windFarm <- read.csv('C:\\Users\\sumen\\Desktop\\MT5757\\Practical 1\\data.csv')

windFarm$density <- windFarm$Nhat/windFarm$are

attach(windFarm)

head(windFarm)

str(windFarm)


# ----Model Fitting----
# 1. the impact covariate alone ----
require(car)

glmFit1<- glm(Nhat ~ impact, data=windFarm, offset=log(area), family=poisson)
glmFitOD1<- glm(Nhat ~ impact, data=windFarm, offset=log(area), family=quasipoisson)

# 2. impact, Depth, x.pos and y.pos covariates ----
glmFit2<- glm(Nhat ~ impact + Depth + x.pos + y.pos, data=windFarm, offset=log(area), family=poisson)
glmFitOD2<- glm(Nhat ~ impact + Depth + x.pos + y.pos, data=windFarm, offset=log(area), family=quasipoisson)

# 3. impact, Depth, x.pos, y.pos and interaction terms between impact and each of x.pos and y.pos ----
glmFit3<- glm(Nhat ~ impact + Depth + x.pos + y.pos + impact*x.pos + impact*y.pos, data=windFarm, offset=log(area), family=poisson)
glmFitOD3<- glm(Nhat ~ impact + Depth + x.pos + y.pos + impact*x.pos + impact*y.pos, data=windFarm, offset=log(area), family=quasipoisson)

# ----Collinearity----

# 1.pairwise scatter plots----
pairs(cbind(x.pos, y.pos,Depth,impact))

require(car)
vif(glmFitOD2)
vif(glmFitOD3)

xmatrix<- model.matrix(glmFitOD3)
head(xmatrix)

# 2.Ridge regression----
# 2.1. Fit a ridge regression----
require(glmnet)
xmatrix<- model.matrix(glmFitOD3)
ridge<- glmnet(xmatrix, windFarm$Nhat, family="poisson", offset=log(area), alpha=0)
cvridge<- cv.glmnet(xmatrix, windFarm$Nhat, family="poisson", offset=log(area), alpha=0, nfolds=10)

# 2.2. Examine these results----
par(mfrow=c(1,2)) 
plot(ridge, xvar="lambda") 
abline(v=log(cvridge$lambda.min)) 
plot(cvridge) 
abline(v=log(cvridge$lambda.min))

# 2.3. Examine which ¦Ë values were trialled and which value was chosen----
log(cvridge$lambda)
log(cvridge$lambda.min)

# 2.4. For clarity, ¡°zoom-in¡± on the y-axis for the plot----
plot(ridge, xvar="lambda", ylim=c(-0.00001, 0.00001))

# 2.5. Investigate the change in the coefficients for the ridge regression compared to the glmFitOD3 model----
par(mfrow=c(1,1)) 
cis<- confint(glmFitOD3) 
plot(1:7, coef(glmFitOD3), ylim=c(range(cis)), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
ridgecoefs<- coef(ridge)[-2, which(ridge$lambda==cvridge$lambda.min)] 
points(1:7, ridgecoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 2.6. Zoom-in for the depth covariate----
plot(1:7, coef(glmFitOD3), ylim=c(range(cis[-c(1,3),])), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
points(1:7, ridgecoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 2.7. Zoom-in for the x.pos, y.pos and their interactions----
plot(1:7, coef(glmFitOD3), ylim=c(range(cis[-c(1,3,5),])), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
points(1:7, ridgecoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 2.8. Are the ridge regression-based coefficients inside the 95% confidence intervals for the glmFitOD3 model?----
ifelse(cis[,1]<ridgecoefs & ridgecoefs < cis[,2],1,0)

# ----LASSO regression----
# 1.Fit a lasso regression for the glmFitOD3 model---
require(glmnet) 
xmatrix<- model.matrix(glmFitOD3) 
lasso<- glmnet(xmatrix, windFarm$Nhat, family="poisson",offset=log(area), alpha=1)
cvlasso<- cv.glmnet(xmatrix, windFarm$Nhat, family="poisson", offset=log(area), alpha=1, nfolds=10)

# 2. Examine the results----
par(mfrow=c(1,2)) 
plot(lasso, xvar="lambda") 
abline(v=log(cvlasso$lambda.min)) 
plot(cvlasso) 
abline(v=log(cvlasso$lambda.min))

# 3. Zoom in for better clarity----
par(mfrow=c(1,1)) 
plot(lasso, xvar="lambda", ylim=c(-0.00001, 0.00001))

# 4. Investigate the change in the coefficients for the lasso regression compared to the glmFitOD3 model coefficients----
par(mfrow=c(1,1)) 
plot(1:7, coef(glmFitOD3), ylim=c(range(cis)), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
lassocoefs<- coef(lasso)[-2, which(lasso$lambda==cvlasso$lambda.min)] 
points(1:7, lassocoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 5. Zoom-in for the depth covariate----
plot(1:7, coef(glmFitOD3), ylim=c(range(cis[-c(1,3),])), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
points(1:7, lassocoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 6. Zoom-in for x, y and their interactions----
plot(1:7, coef(glmFitOD3), ylim=c(range(cis[-c(1,3,5),])), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
points(1:7, lassocoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 7.Examine if the lasso-based coefficients are inside the 95% confidence intervals for the glmFitOD3 model. lassocoefs----
ifelse(cis[,1]<lassocoefs & lassocoefs < cis[,2],1,0)


# ----Elastic net regression----

# 1. Fit an elastic net regression for the glmFitOD3 model----
require(glmnet) 
xmatrix<- model.matrix(glmFitOD3) 
enet<- glmnet(xmatrix, windFarm$Nhat, family="poisson", offset=log(area), alpha=0.5) 
cvenet<- cv.glmnet(xmatrix, windFarm$Nhat, family="poisson", offset=log(area), nfolds=10, alpha=0.5)

# 2. Examine the results----
par(mfrow=c(1,2)) 
plot(enet, xvar="lambda") 
abline(v=log(cvenet$lambda.min)) 
plot(cvenet) 
abline(v=log(cvenet$lambda.min))

# 3. Zoom in for better clarity----
par(mfrow=c(1,1)) 
plot(enet, xvar="lambda", ylim=c(-0.00001, 0.00001))

# 4. Investigate the change in the coefficients for the elastic net regression compared to the glmFitOD3 model:----
par(mfrow=c(1,1)) 
plot(1:7, coef(glmFitOD3), ylim=c(range(cis)), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
enetcoefs<- coef(enet)[-2, which(enet$lambda==cvenet$lambda.min)] 
points(1:7, enetcoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 5. Zoom in for the depth covariate----
plot(1:7, coef(glmFitOD3), ylim=c(range(cis[-c(1,3),])), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
points(1:7, enetcoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 6. Zoom in for the x, y and their interactions----
plot(1:7, coef(glmFitOD3), ylim=c(range(cis[-c(1,3,5),])), xaxt="n", xlab="Coefficients", pch=20) 
segments(1:7,cis[,1], 1:7, cis[,2] ) 
points(1:7, enetcoefs, col=2, pch=20) 
axis(1, at=1:7, names(coef(glmFitOD3)))

# 7. Examine if the elastic net-based coefficients are inside the 95% confidence intervals for the glmFitOD3 model.
ifelse(cis[,1]<enetcoefs & enetcoefs < cis[,2],1,0)

# 8. Compare the coefficients for the ridge regression, LASSO and elastic net----
lmcoefs<- coef(glmFitOD3) 
for(i in 1:length(lmcoefs)){
  plot(1:4, c(lmcoefs[i], ridgecoefs[i], lassocoefs[i], enetcoefs[i]), ylim=range(c(lmcoefs[i], ridgecoefs[i], lassocoefs[i], enetcoefs[i], cis[i,])),
       pch=20, col=c(1:4),xlab="Modelling method", xaxt="n", ylab="Coefficients", main=names(coef(glmFitOD3))[i])
  axis(1, at=1:4, c("LM", "Ridge", "LASSO", "Enet")) 
  segments(1,cis[i,1],1 , cis[i,2]) 
  abline(h=c(cis[i,1], cis[i,2]))}


# ----compare CV----
min(cvridge$cvm)
min(cvlasso$cvm)
min(cvenet$cvm)

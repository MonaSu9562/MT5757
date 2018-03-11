dat<- read.csv("D:\\study\\MT5757\\Practical 4\\simulatedData.csv", header=T)

library("MRSea")

sink("D:\\study\\MT5757\\Practical 4\\Output.txt")
# ----Fit a polynomial model to each simulated set and evaluate the RSS and ASE----

RSS_poly<- c() 
ASE_poly<- c()

# plot the data and underlying function ----
plot(dat$x, dat$response, pch=16, col="grey") 
lines(dat$x,dat$mu, lwd=2)

# Fit a polynomial model----
for(i in 1:100){
    #print(i) 
    #subset the data so it only uses one at a time:
    datasub<- dat[dat$sim==i,]
    polymod<- lm ( response ~ poly(x,6), data=datasub)
    lines(datasub$x,fitted(polymod), col=2)
    RSS_poly[i]<- sum(residuals(polymod)**2)
    ASE_poly[i]<- mean((datasub$mu-fitted(polymod))**2)
}

# find the mean of the RSS and ASE scores----
mean(RSS_poly) 
mean(ASE_poly)

# ----Fit a penalised regression spline GAM----
RSS_gam<- c() 
ASE_gam<- c()

# Plot the data and underlying function ----
plot(dat$x, dat$response, pch=16, col="grey") 
lines(dat$x,dat$mu, lwd=2) 

# Fit a PRS GAM to each of the 100 simulated sets----
require(mgcv)
for(i in 1:100){ 
    #print(i) 
    #subset the data so it only uses one at a time: 
    datasub<- dat[dat$sim==i,] 
    fitgam<- gam(response ~ s(x), data=datasub) 
    lines(datasub$x,fitted(fitgam), col=2) 
    RSS_gam[i]<- sum(residuals(fitgam)**2) 
    ASE_gam[i]<- mean((datasub$mu-fitted(fitgam))**2)
}
mean(RSS_gam) 
mean(ASE_gam)

# ----Fit a spatially adaptive spline based model using SALSA----
require(MRSea) 
ASE_SALSA<- c() 
knots<- c() 
RSS_SALSA<- c()

plot(dat$x, dat$response, pch=16, col="grey") 
lines(dat$x,dat$mu, lwd=2)

for(i in 1:100){ 
  #print(i)
  datasub<- dat[dat$sim==i,]
  initialModel<- glm(response ~ 1, data=datasub) 
  salsa1dlist<-list(fitnessMeasure = 'BIC', minKnots_1d=c(2), maxKnots_1d = c(40), startKnots_1d = c(10), degree=c(2), maxIterations = 10, gaps=c(0))
  # run SALSA
  salsa1dOutput<-runSALSA1D(initialModel, salsa1dlist, varlist=c('x'),factorlist=NULL, datasub, splineParams=NULL, suppress.printout=TRUE, datain=datasub)
  ASE_SALSA[i]<- mean((datasub$mu-fitted(salsa1dOutput$bestModel))**2) 
  knots[i]<-length(salsa1dOutput$splineParams[[2]]$knots) 
  RSS_SALSA[i]<- sum(residuals(salsa1dOutput$bestModel)**2) 
  lines(datasub$x,fitted(salsa1dOutput$bestModel), lwd=2, col=2)
}
mean(RSS_SALSA) 
mean(ASE_SALSA)

# ----Examine the distribution of the number of knots obtained using SALSA----
barplot(table(knots))

# ----For the SALSA based models, examine the relationship between model complexity (using knots) and both RSS and ASE ----
par(mfrow=c(1,2)) 
plot(knots, ASE_SALSA) 
lines(lowess(x=knots, y=ASE_SALSA)) 
plot(knots, RSS_SALSA) 
lines(lowess(x=knots, y=RSS_SALSA))

summary(lm(ASE_SALSA ~knots)) 
summary(lm(RSS_SALSA ~knots))

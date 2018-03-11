# ----Exploratory Analysis----
# 1. Load the data set (which can be found in Moodle) into R.----
windFarm <- read.csv("/cs/home/ms383/MT5757/Practical 1/data.csv")

windFarm$density <- windFarm$Nhat/windFarm$are

attach(windFarm)

head(windFarm)

str(windFarm)

# ----PRACTICAL FIVE ----

# 3. Fit a penalized spline based GAM ----
require(mgcv)
pen_reg <- gam(Nhat ~ s(x.pos) + s(y.pos) + s(Depth) + impact, data = windFarm, family = quasipoisson, offset = log(area))
summary(pen_reg)

# 4. view the partial plots ----
# y are all on the same scale
par(mfrow=c(2,2))
plot(pen_reg, shade=T)
# y-axes are all potentially on different scales:
par(mfrow=c(2,2))
plot(pen_reg, shade=T, scale=0)

# 5. Visually compare the depth relationship estimated using the gam function and a regression spline equivalent with just one knot----
require(splines)
par(mfrow=c(1,2))
test<-update(pen_reg, .~.-s(Depth)+bs(Depth, knots=c(20)))
termplot(test, se=T)

# 6. Carry out model selection on the terms in the working GAM using the dredge function----
require(MuMIn)
pen_regPois<-update(pen_reg, family="poisson")

options(na.action = "na.fail")
dredge(pen_regPois, chat=pen_reg$scale, rank="QAIC")

# 7. Make some predictions for pre and post impact----
preddata<-read.csv("/cs/home/ms383/MT5757/Practical 5/predictionData.csv", header=T)
preddata$Depth<-abs(preddata$Depth)

preds<- predict(pen_reg, newdata=preddata, se=T)

require(fields)
par(mfrow=c(1,2))
quilt.plot(preddata$x.pos[preddata$impact==0],
           preddata$y.pos[preddata$impact==0],
           preds$fit[preddata$impact==0],
           main="Fitted values on the link scale, pre-impact")
quilt.plot(preddata$x.pos[preddata$impact==1],
           preddata$y.pos[preddata$impact==1],
           preds$fit[preddata$impact==1],
           main="Fitted values on the link scale, post-impact")
par(mfrow=c(1,2))
quilt.plot(preddata$x.pos[preddata$impact==0],
           preddata$y.pos[preddata$impact==0],
           exp(preds$fit[preddata$impact==0]),
           main="Fitted values on the response scale, pre-impact")
quilt.plot(preddata$x.pos[preddata$impact==1],
           preddata$y.pos[preddata$impact==1],
           exp(preds$fit[preddata$impact==1]),
           main="Fitted values on the response scale, post-impact")

# 8. Fitting an interaction based GAM with multiple covariates----
pen_regInt<- gam(Nhat ~ s(x.pos, by=impact)+s(y.pos)+s(Depth)+impact,
                 data=windFarm, family=quasipoisson, offset=log(area))
summary(pen_regInt)
preds<- predict(pen_regInt, newdata=preddata, se=T)

# 9. Plot these predictions----
par(mfrow=c(1,2))
quilt.plot(preddata$x.pos[preddata$impact==0],
           preddata$y.pos[preddata$impact==0],
           preds$fit[preddata$impact==0],
           main="Fitted values on the link scale, pre-impact")
quilt.plot(preddata$x.pos[preddata$impact==1],
           preddata$y.pos[preddata$impact==1],
           preds$fit[preddata$impact==1],
           main="Fitted values on the link scale, post-impact")
par(mfrow=c(1,2))
quilt.plot(preddata$x.pos[preddata$impact==0],
           preddata$y.pos[preddata$impact==0],
           exp(preds$fit[preddata$impact==0]),
           main="Fitted values on the response scale, pre-impact")
quilt.plot(preddata$x.pos[preddata$impact==1],
           preddata$y.pos[preddata$impact==1],
           exp(preds$fit[preddata$impact==1]),
           main="Fitted values on the response scale, post-impact")

# 10. runs test ----
require(lawstat)
runs.test(residuals(pen_regInt, type = "pearson"))

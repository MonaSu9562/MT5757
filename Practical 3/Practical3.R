dat <- read.csv('D:\\study\\MT5757\\Practical 1\\data.csv')

dat$density <- dat$Nhat/dat$are

attach(dat)


# ----Model Fitting----
# 1. the impact covariate alone ----
require(car)

glmFit1<- glm(Nhat ~ impact, data=dat, offset=log(area), family=poisson)
glmFitOD1<- glm(Nhat ~ impact, data=dat, offset=log(area), family=quasipoisson)

# 2. impact, Depth, x.pos and y.pos covariates ----
glmFit2<- glm(Nhat ~ impact + Depth + x.pos + y.pos, data=dat, offset=log(area), family=poisson)
glmFitOD2<- glm(Nhat ~ impact + Depth + x.pos + y.pos, data=dat, offset=log(area), family=quasipoisson)

# 3. impact, Depth, x.pos, y.pos and interaction terms between impact and each of x.pos and y.pos ----
glmFit3<- glm(Nhat ~ impact + Depth + x.pos + y.pos + impact*x.pos + impact*y.pos, data=dat, offset=log(area), family=poisson)
glmFitOD3<- glm(Nhat ~ impact + Depth + x.pos + y.pos + impact*x.pos + impact*y.pos, data=dat, offset=log(area), family=quasipoisson)


# ----Detecting residual autocorrelation----

# Generate 50 values at random from a Normal distribution
testVals<- rnorm(50)

# Assign each value a positive or negative value, and plot these in sequence
sign(testVals) 
plot(sign(testVals), type="l")

# plot residuals for the working model in order: 
plot(sign(residuals(glmFitOD3, type="pearson")[1:800]), type="l")

# runs test of pr in observation order
require(lawstat) 
runs.test(residuals(glmFitOD3, type="pearson"))


# ----Diagnosing nonlinearities on the link scale----

# Examine the Pearson¡¯s residuals in relation to Depth
require(car) 
residualPlots(glmFitOD3, quadratic=T, type = "pearson", ylim=c(-20,20),terms = ~ Depth, fitted=FALSE)

# Re-order the residuals in order of Depth value
dat$Depth[order(dat$Depth)][1:100] 
par(mfrow=c(1,2))
plot(sign(residuals(glmFitOD3, type="pearson")[order(dat$Depth)])[1:100], type="l", main="100 Residuals in Depth order", ylab="Pearsons Residuals")
plot(sign(rnorm(100)), type="l", main="Random values")

# runs-test personresidual ordered by depth
require(lawstat) 
runs.test(residuals(glmFitOD3, type="pearson")[order(dat$Depth)])

# Examine the Pearson¡¯s residuals in relation to x.pos
residualPlots(glmFitOD3, quadratic=T, type = "pearson", ylim=c(-20,20),terms = ~ x.pos, fitted=FALSE)

# runs-test pearsons residual ordered by x.pos
runs.test(residuals(glmFitOD3, type="pearson")[order(dat$x.pos)])

# Examine the Pearson¡¯s residuals in relation to y.pos
residualPlots(glmFitOD3, quadratic=T, type = "pearson", ylim=c(-20,20),terms = ~ y.pos, fitted=FALSE)

# runs test for the working model using the Pearsons residuals ordered by y.pos value
runs.test(residuals(glmFitOD3, type="pearson")[order(dat$y.pos)])

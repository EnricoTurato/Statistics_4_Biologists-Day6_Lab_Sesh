dat = read.csv("Eulaema.csv")
head(dat)
names(dat)
View(dat)
str(dat)

y = dat$Eulaema_nigrita
method = as.factor(dat$method)

x1 = dat$MAT
x2 = dat$MAP
x3 = dat$Tseason
x4 = dat$Pseason
x5 = dat$forest.
x6 = dat$lu_het
x7 = dat$effort
x8 = dat$altitude
x9 = method

oldpar = par(no.readonly = TRUE)
par(bg = "ivory")
boxplot(y~method, data = dat, xlab="Method", ylim = c(0, 800),
        ylab="CountBee",boxwex=0.35, main = "Boxplot of BeeCount data vs methods", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(y~method,
           data = dat,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(1,2,3,4,5,6,7,9),
           vertical = TRUE, 
           add = TRUE)
par(oldpar)

m = glm(y ~ x2 , family="poisson", data = dat)
summary(m)

'm1 = glm(y ~ x1 , family="poisson", data = dat)
summary(m1)

m2 = glm(y~x2 , family="poisson", data = dat)
summary(m2)

m3 = glm(y ~ x3 , family="poisson", data = dat)
summary(m3)

m4 = glm(y~ x4 , family="poisson", data = dat)
summary(m4)

m5 = glm(y~ x5 , family="poisson", data = dat)
summary(m5)

m6 = glm(y~ x6 , family="poisson", data = dat)
summary(m6)

m7 = glm(y~ x7 , family="poisson", data = dat)
summary(m7)

m8 = glm(y~ x8 , family="poisson", data = dat)
summary(m8)


AIC(m1, m2, m3, m4, m5, m6, m7, m8) #m7 fits best

m = glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, family="poisson", data = dat)
summary(m)'

library(ggplot2)
par(mfrow=c(2,2))
# Display residual-fitted plot
plot(m, which = 1)  
# Normal QQ plot
plot(m, which = 2) 
# Display scale-location plot
plot(m, which = 3)  
# Cook's distance
plot(m, which = 4) 
# Leverage plot
plot(m, which = 5)

plot(x2, y, las=1, col="skyblue3", pch=16)
xx = seq(min(x2), max(x2), 0.01)
y_hat = predict(m, newdata=list(x2=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
#lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
#lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)

polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE)

library(MuMIn)
r.squaredGLMM(m)
1-(m$deviance/m$null.deviance)
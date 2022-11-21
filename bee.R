library(rcompanion)
dat = read.csv("Eulaema.csv")
head(dat)
names(dat)
#View(dat)
str(dat)

#subdat = dat[dat$pop=="CC",]

#hist(dat$Eulaema_nigrita, col="black", border="green", main = "MAT: Normal Distribution overlay on Histogram", linecol="red", lwd=3 )

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

  
  
  
plot(dat)
par(mfrow=c(3,3))
plot(x1, y, las=1, ylab="x", xlab="y")
plot(x2, y, las=1, ylab="x", xlab="y")
plot(x3, y, las=1, ylab="x", xlab="y")
plot(x4, y, las=1, ylab="x", xlab="y")
plot(x5, y, las=1, ylab="x", xlab="y")
plot(x6, y, las=1, ylab="x", xlab="y")
plot(x7, y, las=1, ylab="x", xlab="y")
plot(x8, y, las=1, ylab="x", xlab="y")
plot(x9, y, las=1, ylab="x", xlab="y")

par(mfrow=c(3,3))

hist(x1, prob = FALSE, col="black", border="green",
                    main = "MAT: Normal Distribution overlay on Histogram",
                    lwd=3 )
hist(x2, prob = FALSE, col="black", border="green",
                    main = "MAP: Normal Distribution overlay on Histogram",
                     lwd=3 )
hist(x3, prob = FALSE, col="black", border="green",
                    main = "Tseason: Normal Distribution overlay on Histogram",
                     lwd=3 )

hist(x4, prob = FALSE, col="black", border="green",
                    main = "Pseason: Normal Distribution overlay on Histogram",
                     lwd=3 )
hist(x5, prob = FALSE, col="black", border="green",
                    main = "forest.: Normal Distribution overlay on Histogram",
                    lwd=3 )
hist(x6, prob = FALSE, col="black", border="green",
                    main = "lu.het: Normal Distribution overlay on Histogram",
                     lwd=3 )

hist(x7, prob = FALSE, col="black", border="green",
                    main = "effort: Normal Distribution overlay on Histogram",
                     lwd=3 )
hist(x8, prob = FALSE, col="black", border="green",
                    main = "altitude: Normal Distribution overlay on Histogram",
                     lwd=3 )
hist(x9, prob = FALSE, col="black", border="green",
                    main = "method: Normal Distribution overlay on Histogram",
                     lwd=3 )



'plotNormalHistogram(x1, prob = FALSE, col="black", border="green",
                    main = "MAT: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )
plotNormalHistogram(x2, prob = FALSE, col="black", border="green",
                    main = "MAP: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )
plotNormalHistogram(x3, prob = FALSE, col="black", border="green",
                    main = "Tseason: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )

plotNormalHistogram(x4, prob = FALSE, col="black", border="green",
                    main = "Pseason: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )
plotNormalHistogram(x5, prob = FALSE, col="black", border="green",
                    main = "forest.: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )
plotNormalHistogram(x6, prob = FALSE, col="black", border="green",
                    main = "lu.het: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )

plotNormalHistogram(x7, prob = FALSE, col="black", border="green",
                    main = "effort: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )
plotNormalHistogram(x8, prob = FALSE, col="black", border="green",
                    main = "altitude: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )
plotNormalHistogram(x9, prob = FALSE, col="black", border="green",
                    main = "method: Normal Distribution overlay on Histogram",
                    linecol="red", lwd=3 )'

oldpar = par(no.readonly = TRUE)
par(oldpar)
par(bg = "ivory")
boxplot(y~method, data = dat, xlab="Method", ylim = c(0, 800),
        ylab="CountBee",boxwex=0.35, main = "Boxplot of Bee data for method", lwd = 2, border= "slategrey", # colour of the box borders
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

m = glm(y~x, family="poisson")
summary(m)

#ggplot(data = datset, aes(x = , y= , col= categorical variable)) + 
#geom_point() + geom_smooth(method = "lm", se = FALSE)

library(ggplot2)
qplot(x, y, ylab = "y", main = "Ideal regression setup") + stat_smooth(method = "glm")
# Run regression 
reg = glm(y~x, family="poisson")
par(mfrow=c(2,2))
# Display residual-fitted plot
plot(reg, which = 1)  
# Normal QQ plot
plot(reg, which = 2) 
# Display scale-location plot
plot(reg, which = 3)  
# Cook's distance
plot(reg, which = 4) 
# Leverage plot
plot(reg, which = 5) 


plot(x, y, las=1, col="skyblue3", pch=16)
xx = seq(min(x), max(x), 0.01)
y_hat = predict(m, newdata=list(x=xx), type="response", se.fit=T)
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

#library(MASS)
#m = glm.nb(y~x)
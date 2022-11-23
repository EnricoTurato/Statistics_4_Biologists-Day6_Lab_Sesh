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

# rescaling variables

x2_scaled = scale(x2, center = TRUE, scale = TRUE)
x5_scaled = scale(x5, center = TRUE, scale = TRUE)

oldpar = par(no.readonly = TRUE)
par(bg = "ivory")
boxplot(y~method, data = dat, xlab="Method", ylim = c(0, 1300),
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

'mean(x2)
sd(x2)
mean(x2_scaled)
sd(x2_scaled)
mean(x5)
sd(x5)
mean(x5_scaled)
sd(x5_scaled)

m = glm(y ~ x2 +x5 , family="poisson", data = dat)
summary(m)'

mm = lm (y ~ x9)
summary(mm)


m_scaled = glm(y ~ x2_scaled +x5_scaled , family="poisson", data = dat)
summary(m_scaled)

library(MuMIn)
r.squaredGLMM(m_scaled)
1-(m_scaled$deviance/m_scaled$null.deviance)


library(MASS)
'm_nb = glm.nb(y~x2 + x5)
summary(m_nb)
'
m_nb_scaled = glm.nb(y~x2_scaled + x5_scaled)
summary(m_nb_scaled)


dat_low_forest_coverage = dat[dat$forest.<=0.33,]
dat_medium_forest_coverage = dat[dat$forest.>0.33 & dat$forest.<=0.66,] 
dat_high_forest_coverage = dat[dat$forest.>0.66,]
View(dat_low_forest_coverage)
View(dat_medium_forest_coverage)
View(dat_high_forest_coverage)

'plot(x2_scaled, y, las=1, col="darkgrey", pch=16)
xx = seq(min(x2_scaled), max(x2_scaled), 0.01)
y_hat = predict(m_nb_scaled, newdata=list(x2_scaled=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)

polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE)'


###################################################################################################
###################################################################################################
###################################################################################################
#
#
#
# after solution were given during lecture
#
#
#
###################################################################################################
###################################################################################################
###################################################################################################
library(MASS)
library(MuMIn)
dat = read.csv("Eulaema.csv")
head(dat)


m = glm.nb(Eulaema_nigrita ~ MAP + forest., data = dat)
summary(m)


1- m$deviance/m$null.deviance


plot(dat$forest., dat$Eulaema_nigrita, col="grey", las=1,
     xlab="Forest cover",
     ylab="El. nigrita abundance")

newforest = seq(min(dat$forest.), max(dat$forest.), length.out=200)

newMAP = rep(mean(dat$MAP), length(newforest))

y_hat = predict(m, newdata=list(MAP=newMAP,
                                forest.=newforest),
                type="response")

lines(newforest, y_hat,lwd=2)

newMAP2 = rep(mean(dat$MAP)+sd(dat$MAP), length(newforest))

y_hat2 = predict(m, newdata=list(MAP=newMAP2,
                                 forest.=newforest),
                 type="response")

newMAP3 = rep(mean(dat$MAP)-sd(dat$MAP), length(newforest))

y_hat3 = predict(m, newdata=list(MAP=newMAP3,
                                 forest.=newforest),
                 type="response")

lines(newforest, y_hat2, lwd=2, col=2)

lines(newforest, y_hat3, lwd=2, col=3)

legend("topleft", lty=1, lwd=2, col=1:3, bty="n",
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))

#########################################################################
#########################################################################
#
# trying to do a similar plot but for forest
#
#########################################################################
#########################################################################


dat_low_forest_coverage = dat[dat$forest.<=0.33,]
dat_medium_forest_coverage = dat[dat$forest.>0.33 & dat$forest.<=0.66,] 
dat_high_forest_coverage = dat[dat$forest.>0.66,]

plot(dat$MAP, dat$Eulaema_nigrita, col="grey", las=1,
     xlab="MAP",
     ylab="El. nigrita abundance")

dat_low_forest_coverage = dat[dat$forest.<=0.33,]
length(dat_low_forest_coverage$forest.)
dat_medium_forest_coverage = dat[dat$forest.>0.33 & dat$forest.<=0.66,] 
length(dat_medium_forest_coverage$forest.)
dat_high_forest_coverage = dat[dat$forest.>0.66,]
length(dat_high_forest_coverage$forest.)


newMAP_low = seq(min(dat_low_forest_coverage$MAP), max(dat_low_forest_coverage$MAP), length.out=200)

newforest_low = rep(mean(dat_low_forest_coverage$forest.), length(newMAP_low))

newMAP_medium = seq(min(dat_medium_forest_coverage$MAP), max(dat_medium_forest_coverage$MAP), length.out=200)

newforest_medium = rep(mean(dat_medium_forest_coverage$forest.), length(newMAP_medium))

newMAP_high = seq(min(dat_high_forest_coverage$MAP), max(dat_high_forest_coverage$MAP), length.out=200)

newforest_high = rep(mean(dat_high_forest_coverage$forest.), length(newMAP_high))

y_hat = predict(m, newdata=list(MAP=newMAP_low,
                                forest.=newforest_low),
                type="response")

lines(newMAP_low, y_hat,lwd=2)



y_hat2 = predict(m, newdata=list(MAP=newMAP_medium,
                                 forest.=newforest_medium),
                 type="response")



y_hat3 = predict(m, newdata=list(MAP=newMAP_high,
                                 forest.=newforest_high),
                 type="response")

lines(newMAP_medium, y_hat2, lwd=2, col=2)

lines(newMAP_high, y_hat3, lwd=2, col=3)

legend("topleft", lty=1, lwd=2, col=1:3, bty="n",
       legend=c("forest_coverage = low",
                "forest_coverage = medium",
                "forest_coverage = high"))
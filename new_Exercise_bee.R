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
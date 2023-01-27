# a
dat <- read.csv('iris.csv')
dat[dat == -99.0] = NA

# b
mdat <- sapply(dat[,1:4], mean, na.rm = T)
vdat <- sapply(dat[1:4], var, na.rm = T)

# c 
dat <- cbind(dat, Bsepal = as.integer(dat$Sepal.Width > 3.1))

# d
rowToBeAdded <- data.frame(6.1,3.2,4.1,1.3,'versicolor', 1)
names(rowToBeAdded) <- names(dat)
dat <- rbind(dat, rowToBeAdded)

# e
datse <- dat[dat$Species == 'setosa',]
datve <- dat[dat$Species == 'versicolor',]
datvi <- dat[dat$Species == 'virginica',]

# f
mdatse <- sapply(datse[, 1:4], mean, na.rm = T)
vdatse <- sapply(datse[, 1:4], var, na.rm = T)

mdatve <- sapply(datve[, 1:4], mean, na.rm = T)
vdatve <- sapply(datve[, 1:4], var, na.rm = T)

mdatvi <- sapply(datvi[, 1:4], mean, na.rm = T)
vdatvi <- sapply(datvi[, 1:4], var, na.rm = T)



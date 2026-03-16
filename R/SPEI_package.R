#install.packages(c("SPEI", "ggplot2", "plyr"))
require(SPEI)


precipdata <- read.csv(file = 'C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/precip_chirps.csv')
fullprecip <- read.csv(file = 'D:/PhD/Code/precip_pixels_Nanfilled.csv')
t2mdata <- read.csv(file = 'C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/t2m_pixels.csv')
tmindata <- read.csv(file = 'C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/tmin_pixels.csv')
tmaxdata <- read.csv(file = 'C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/tmax_pixels.csv')

t2mdata <- head(t2mdata, -8)

dfpet <- data.frame(Dates = precipdata$Dates)
dfspei <- data.frame(Dates = precipdata$Dates)
dfpdsi <- data.frame(Dates = precipdata$Dates)
dfspi <- data.frame(Dates = precipdata$Dates)

#calculate 3-month SPI with Gamma fitting function -----------------------------
SPI3 = spi(precipdata$X2, scale = 3, distribution = 'Gamma')
#plot(SPI3)
plot(spi(ts(precipdata$X4822,freq=12,start=c(1983,1)),scale = 3))

show(SPI3$fitted)

for (i in 2:ncol(precipdata))
{
  spi3 <- spi(precipdata[ , i], scale = 3, distribution = 'Gamma')
  dfspi <- cbind(dfspi, as.data.frame(spi3$fitted))
}

#calculate SPEI with Gamma fitting function ------------------------------------

#calculate PET using hargreaves method
for (i in 2:ncol(precipdata))
{
  PET <- hargreaves(Tmin = tmindata[ , i], Tmax = tmaxdata[ , i], lat = i)
  dfpet <- cbind(dfpet, PET)
}

#calculate PET using Thornthwaite method
for (i in 2:ncol(precipdata))
{
  PET <- thornthwaite(Tave = t2mdata[ , i], lat = i)
  dfpet <- cbind(dfpet, PET)
}

for (i in 2:ncol(precipdata))
{
  wbal <- precipdata[ , i] - dfpet[ , i]
  spei3 <- spei(wbal, 3)
  dfspei <- cbind(dfspei, as.data.frame(spei3$fitted))
}

write.csv(dfspei, "C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/outputchirps/SPEI.csv")

#calculate PDSI and sc-PDSI with complete rainfall TS (no NANs)-----------------
for (i in 2:ncol(precipdata))
{
  pdsi(precipdata[ , i], dfpet[ , i], AWC=100, start=1983, end=2022)
  dfpdsi <- cbind(dfpdsi, as.data.frame(pdsi$X))
}

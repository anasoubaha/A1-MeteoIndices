##install.packages("writexl")
rm(list = ls())

library(writexl)
library(xts)
source('./src/run_theory.R')
`%>%` = magrittr::`%>%`

sebou <- read.csv(file = 'C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/outputchirps/DI_sebou.csv')
tensift <- read.csv(file = 'C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/outputchirps/DI_tensift.csv')
souss <- read.csv(file = 'C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/outputchirps/DI_souss.csv')
ma <- read.csv(file = 'C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/outputchirps/DI_ma.csv')
#### using example from SCI package ####

## create artificial data, resembling precipitation
set.seed(101)
n.years <- 60
date <- rep(1:n.years, each = 12) + 1950 + rep((0:11)/12, times = n.years)
PRECIP <- (0.25*sin( 2 * pi * date) + 0.3)*rgamma(n.years*12, shape = 3, scale = 1)
PRECIP[PRECIP < 0.1] <- 0

## apply SCI transformation (computing SPI)
spi.para <- SCI::fitSCI(PRECIP, first.mon = 1, time.scale = 6, distr = "gamma", p0 = TRUE)
spi <- SCI::transformSCI(PRECIP, first.mon = 1, obj = spi.para)

## creting XTS time serie
spi_dates <- seq(as.Date("1951-01-01"), length.out = length(date), by = "month")
spi <- xts::xts(spi, spi_dates)

## plot time series
xts::plot.xts(spi)
lattice::xyplot(spi)


dates <- seq(as.Date("1983-03-01"), length.out = length(ma$Dates), by = "month")
rai <- xts::xts(ma$RAI, dates)
spi <- xts::xts(ma$SPI, dates)
spei <- xts::xts(ma$SPEI, dates)
scpdsi <- xts::xts(ma$scPDSI, dates)
pzi <- xts::xts(ma$PZI, dates)


## drough features
dght_features1 <- run_theory(time_serie = rai[0:length(rai)], threshold = -1)
dght_features2 <- run_theory(time_serie = spi[0:length(rai)], threshold = -0.5)
dght_features3 <- run_theory(time_serie = spei[0:length(rai)], threshold = -0.5)
dght_features4 <- run_theory(time_serie = scpdsi[0:length(rai)], threshold = -1.5)
dght_features5 <- run_theory(time_serie = pzi[0:length(rai)], threshold = -1.5)

df1 <- data.frame(Duration = dght_features1$Duration)
df2 <- data.frame(Severity = dght_features1$Severity)
df3 <-data.frame(Intensity = dght_features1$Intesity)
df4 <-data.frame(Date_Ini_Ev=dght_features1$Date_Ini_Ev)
df5 <-data.frame(Date_Fin_Ev=dght_features1$Date_Fin_Ev)

df <- cbind(df1, df2, df3, df4, df5)

write_xlsx(df, "C:/Users/Oubaha.ANAS/Workspace/Code/Article1-MeteoIndices/outputchirps/ma_rai.xlsx")

dght_features$Duration
dght_features$Severity
dght_features$Intesity
dght_features$Date_Ini_Ev
dght_features$Date_Fin_Ev
dght_features$Interarrival

## plot

library(ggplot2)

lapply(dght_features %>% names(), 
       function(x){
         data.frame(value = dght_features[[x]], 
                    feature = x) 
         }) %>% 
  .[c(1,2,3, 6)] %>% # deleting Dates output to plot
  do.call(rbind, .) %>%
  
  ggplot() + 
  facet_wrap(~feature, "free", nrow = 2, ncol = 2) + 
  geom_histogram(aes(x = value)) + 
  xlab("")

# imports
#install.packages(c("PCICt"))

source("D:/PhD/Code/R/SPI_grid.R")

datadir <- "D:/PhD/Datasets/PERSIANN-CDR/"

# descriptions for saving output files with
datadescr <- "CDR"

# Put the path to your precipitation files here;
# must be a netCDF file with monthly data
inputfile <- "CDRm.nc"

# Name of precipitation variable in the file
varname <- "precip"

n <- 3

# Region Name (for file naming)

Reg <- "CDR"


file <- paste0(datadir, inputfile)
varname <- varname
descr <- datadescr
ppt <- ncgrid_read(file)

ppt %<>% compute_SPIgrid()
ppt %>% output_SPI(Reg = paste0(descr, "_", Reg), n = n)
output_SPI(ppt, Reg = descr, n = n)

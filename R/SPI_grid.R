# This script contains functions to be applied to a spatio-temporal
# (monthly temporal resolution and timestep) netcdf precipitation grid file,
# to add columns with SPI values over various timescales and plot snaptshots
# of the output

#imports

library(SCI)
library(metR)
library(data.table)
library(ncdf4)          # for writing output to file
library(magrittr)       # for %<>% 
library(tidyverse)
library(docstring)

figdir <- "D:/PhD/Code/figures/"
datadir <- "D:/PhD/Datasets/PERSIANN-CDR/"
outdir <- ""

ncgrid_read <- function(path,
                       varname = 'precip',
                       drop_empty = FALSE)
{
    #' Read in gridded data from a NetCDF file as a data.table.
    #'
    #' @param path The full path to the location on disk of the nc file to read.
    #' @param varname The name of the variable to read in.
    #' @param drop_empty Should NA values of this variable be dropped?
    #'
    #' @return A data.table    
    if (drop_empty)  {
        return(ReadNetCDF(path) %>% na.omit(cols = varname))
    }
    else {
        return(ReadNetCDF(path))
    }
}


compute_SPIgrid <- function(dtable,
                            n = 3,
                            distr_to_fit = "gamma",
                            zero_corr = TRUE,
                            varname = "precip",
                            lonname = "lon",
                            latname = "lat",
                            timename = "time")
{
    #' Standard Precipitation Index (SPI) at a prescribed time scale of a
    #' gridded monthly rainfall data.table 
    #' 
    #' From a data.table with gridded monthly spatio-temporal rainfall data at
    #' selected latitude, longitude and time points, computes the SPI values 
    #' at each point in space for all times at the requested time-scale 
    #'
    #' @param dtable A data.table obtained from gridded rainfall data
    #' @param n integer. The time scale at which to compute SPI values, in months.
    #'  Default to 12.
    #' @param distr_to_fit The distribution to use for the SPI fits. Defaults to
    #' "gamma"
    #' @param zero_corr bool. Should the probability of zero-rainfall be fit 
    #' independently? Defaults to TRUE
    #' @param varname A character string, containing the name of the variable in 
    #' which monthly precipitation is stored
    #' @param lonname,latname,timename The names of the columns in `dtable` that 
    #' contain the longitude, latitude and time dimensions of the data  
    #'
    #' @return data.table, with time, longitude and latitude columns renamed to
    #' "time", "lon" and "lat" and a column named by paste0("SPI", n) added with
    #' computed SPI values at the required timescale   
    
    #' @examples see runSPI.R
    
    # for ease of use, relabel columns with standard names
    
    #setnames(dtable,
             #c(timename, lonname, latname),
             #c("time", "lon", "lat"))
    
    Nmon <- length(unique(dtable$time))  # months of the data file considered
    
    # .default because of unexplained error without it
    lats <- unique.default(dtable$lat)
    lons <- unique.default(dtable$lon)
    
    dtable[, "SPI3" := 0]  # initialise to value to replace NAs
    mon0 <- as.integer(format(dtable$time[1], "%m"))  # first month
    
    for (i in lons)
    {
        for (j in lats)
        {
            Var <- dtable[lat == j & lon == i, get(varname)]
            if (sum(is.na(Var)) < 5 &
                length(Var) > 0)
                #more than 5 missing data pnts excl; excludes ocean pnts
            {
                Var.SPIfit <- fitSCI(
                    Var,
                    first.mon = mon0,
                    time.scale = n,
                    distr = distr_to_fit,
                    p0 = zero_corr,
                    scaling = "sd"
                )
                dtable$SPI3[dtable$lat == j & dtable$lon == i] <- transformSCI(
                       Var,
                       first.mon = mon0,
                       obj = Var.SPIfit
                       )
            }
        }
    }
    
    return(dtable)
}




output_SPI <- function(dtable, Reg = "CDR", n = 3)
{
    #' Writes SPI data from a data.table out to netcdf and RDS files
    #'
    #' Asssumes that `dtable` was output by `compute_SPIgrid` and hence has
    #' "time", "lat" and "lon" as its dimension columns
    #'
    #' @param dtable A data.table output by `compute_SPIgrid`, with a column for 
    #' SPI at the required timescale (`n`) 
    #' @param Reg A character string describing the region and data used, to be
    #' used to name the output files. Default="SWSA_GPCC_v2020"
    #' @param n An integer time-scale in months for which the SPI data should be
    #' written out. Defaults to 12 months.
    #'
    #' @return NULL
    #' 
    Nmon <- length(unique(dtable$time)) # months of the data file considered
    
    lats <- unique.default(dtable$lat) 
    lons <- unique.default(dtable$lon)
    
    # co-ordinates of first grid point to get reference time series
    lon0 <- dtable[1, lon]
    lat0 <- dtable[1, lat]
    t0 <- dtable[1, time]
    
    assign(paste0(Reg, "_SPI", n), dtable)  # Assign a name to the array to save
    saveRDS(dtable, file = paste0(Reg, "_SPI", n, ".rds"))  # save as rds file
    
    ## Write to nc-file
    
    # times for first grid point as seconds since start point converted to days:
    
    timevec <- dtable[lat == lat0 & lon == lon0, time]
    # as.numeric to convert from time difference
    daysvec <- as.numeric(timevec - t0) / 86400
    
    # Define time dimension; 
    # have units start from first entry;
    # units in days for CF-compliance and Python xarray compatibility
    t <- ncdim_def("time", paste0("days since ", t0), daysvec, unlim = TRUE) 
    
    # For lat and long, preserve the existing grid
    x <- ncdim_def("lon", "degrees_east", lons)
    y <- ncdim_def("lat", "degrees_north", lats)
    
    nt <- ncdim_def("timescale", "months", n)
    
    SPI <- ncvar_def(
        "SPI",
        units = "1",
        dim = list(nt, x, y, t),
        missval = NA,
        longname = paste0(n , "-month Standardised Precipitation Index")
    ) # define precipitation variable, dependent on time only
    
    ncSPI <- nc_create(
        paste0(datadir, Reg, "_SPI", n, ".nc"),
        vars = SPI,
        force_v4 = TRUE,
        verbose = FALSE
    ) #Create nc file
    
    for (l in 1:length(timevec))
        # cycling through times directly converts to numeric
    {
        tl <- timevec[l]
        ncvar_put(
            ncSPI,
            varid = "SPI",
            start = c(1, 1, 1, l),
            count = c(-1,-1,-1, 1),
            vals = dtable[time == tl, get(paste0('SPI', n))],
            verbose = FALSE
        ) # Populate the variable with data; count -1 implies use all data
    }
    
    nc_close(ncSPI) #Close the nc file after using
}


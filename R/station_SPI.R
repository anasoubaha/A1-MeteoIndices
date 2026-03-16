library(SCI)
library(docstring)

assign_SPI <- function(df,
                      tscale = 12,
                      stationvar = "station",
                      rainvar = "rainfall")
{
  #' Computes the SPI (Standardised Precipitation Index) and adds it to a 
  #' data.frame. 
  #' 
  #' The SPI column is named (paste0("SPI", n)), where `n`
  #'  is the timescale in months at which SPI values are computed 
  #'
  #' @param df A data.frame containing monthly rainfall data with a unique
  #' rainfall value for each month at each unique instance of `stationvar`
  #' @param tscale The (integer) time scale in months at which the SPI
  #' values are to calculated
  #' @param stationvar A character string containing the variable name used
  #' to distinguish locations from one another
  #' @param rainvar A character string containing the variable name in which
  #' monthly rainfall data are stored
  #'
  #' @return A data.frame with the SPI data added in a new column
  
  for (S in unique(pull(df, stationvar)))
  {
    rain <- pull(df[df[stationvar] == S, ], rainvar)
    mon1 <- pull(df[df[stationvar] == S, ], "month")[1]
    SPIfit <- fitSCI(
      rain,
      first.mon = mon1,
      time.scale = tscale,
      distr = "gamma",
      p0 = TRUE,
      scaling = "sd"
    )
    SPI <- transformSCI(rain, first.mon = mon1, obj = SPIfit)
    df[df[stationvar] == S, paste0("SPI", tscale)] <- SPI
  }
  return(df)
}


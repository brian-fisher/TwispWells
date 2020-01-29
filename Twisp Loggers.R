library(data.table)
library(lubridate)
library(tidyverse)
library(magrittr)
library(modelr)


# Settings ----------------------------------------------------------------

folder <- "C:/Users/Brian/Documents/MSRF/Twisp Floodplain/Monitoring/well data/"


# File List ---------------------------------------------------------------
logger.folder <- paste0(folder,"to_process")

logger.files <- list.files(logger.folder )
barometer.files <- list.files(paste0(folder,"barometer"))

# Functions ---------------------------------------------------------------


Convert_psi <- function(PSI, temperature) {
      # converts PSI to feet of water based on temperature
      # This matches the conversion used in Hobo loggers barometric conversion utility to 
      # within .001 ft of water
      
      return((PSI )*(2.307 - .00008457 * temperature + .00001546* temperature^2 ))
}

read_logger <- function(name = "low07", 
                        file = logger.files[1], 
                        logger.type = "heron", 
                        file.location = logger.folder
                        ) {
      ###   Reads in logger data in Heron and Hobo formats.  Returns common format
      ###
      ###   type should be either "heron" or "hobo"
      ###   alternate testing settings:
      ###         logger.type = "hobo"
      ###         file = logger.files[8]
      ###         name = "low11"
      if(!(logger.type %in% c("heron", "hobo"))) {
            stop("Incorrect logger type, please enter either 'heron' or 'hobo'")
      }
      
      source.file = file.path( file.location, file )
      
      if(logger.type == "heron"){
            skip = 12
            x <- fread(source.file, skip = skip)
            colnames(x) <- c("Date", "Time", "rawPressure(ft.H2O)", "CompPressure(ft.H2O)", "Depth to Water (ft)", "Temp°C", "flag")
            x %<>%
                  mutate(DateTime = ymd_hms(paste(Date, Time)),
                         Logger = name) %>%
                  select(DateTime, `rawPressure(ft.H2O)`, `Temp°C`, Logger)
      }
      if(logger.type == "hobo"){
         skip = 1
         x <- fread(source.file, skip = skip)
         colnames(x) <- c("num" , "DateTime",  "PSI", "Temp", "SensorDepth")
         
         if(max(x$Temp) > 30) {  
               # Converts to Celsius if data appears to be in Farenheit
               # Assumes temperature is in Farenheit if max is over 30 deg
               x %<>% mutate( Temp = (Temp - 32)/1.8 )
            }
         
         x %<>% 
               rename( "Temp°C" = Temp) %>%
               mutate(Logger = name,
                      `rawPressure(ft.H2O)` = Convert_psi(PSI, `Temp°C`),
                      DateTime = mdy_hms(DateTime)) %>%
               select(DateTime, `rawPressure(ft.H2O)`, `Temp°C`, Logger)

      }
      assign(name, x, envir = globalenv())
      invisible()
}

check_for_air_data <- function(datafile) {
      # datafile = low11
      p <- datafile %>%
            ggplot(aes( x = DateTime, y = `rawPressure(ft.H2O)`))+
            geom_line() +
            labs( title = substitute(datafile)
                  )

      datafile %<>%
            mutate(change = `rawPressure(ft.H2O)`- lag(`rawPressure(ft.H2O)`))
      print(max(datafile$change, na.rm = TRUE))
      return(p)
}

which.interval <- function(x, y) {
      z <- which(x %within% y)
      z <- ifelse(isTRUE(z > 0), z, NA)
      z
}


# Process files ------------------------------------------------------------------

loggers.df <- tibble(file = logger.files) %>%
      mutate(logger = map_chr(str_split(file, pattern = "-"),3),
             extension = str_sub(file, start = -4),
             logger.type = case_when(
                   extension == ".txt" ~ "heron",
                   extension == ".csv" ~ "hobo",
                   TRUE ~ "other"
                  )
             ) %>%
      arrange(logger) %>%
      select(-extension)
      
mapply(read_logger, file = loggers.df$file, name = loggers.df$logger, logger.type = loggers.df$logger.type)

lapply(lapply(loggers.df$logger, get), check_for_air_data)

logger.data <- bind_rows(lapply(loggers.df$logger, get))%>%
      mutate( Date = as_date(DateTime) )



# Summary table -----------------------------------------------------------


logger.data$Period <- sapply(logger.data$DateTime, 
                             function(x) which.interval(x, Periods$Interval) )
logger_compare <- logger.data %>%
      select(-`Depth to Water (ft)`, -`rawPressure(ft.H2O)`) %>%
      group_by(Period, logger) %>%
      summarize( Temp = mean(`Temp°C`),
                 Pressure = mean(`CompPressure(ft.H2O)`))
logger_compare %>%
      select(-Temp) %>%
      spread(key = logger, value = Pressure)


# Plots -------------------------------------------------------------------


logger.data %>%
      ggplot(aes(x= DateTime, y = `CompPressure(ft.H2O)`, color = logger)) +
      geom_line()
      
logger.data %>%
      ggplot(aes(x= DateTime, y = `rawPressure(ft.H2O)`, color = logger)) +
      geom_line()

logger.data %>%
      ggplot(aes(x= DateTime, y = `Depth to Water (ft)`, color = logger)) +
      geom_line()

logger.data %>%
      ggplot(aes(x= DateTime, y = `Temp°C`, color = logger)) +
      geom_line()




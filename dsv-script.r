# Process CR1000 data logger outputs and generate DSV and PDays
# Developed by Ben Bradford, UW Madison, 2020 (bbradford@wisc.edu)

# Must call this script with the station name supplied as an argument



# Setup -------------------------------------------------------------------

# set local variables
setwd("C:/dsv") # set working directory


# debugging
logtext <- function(msg, logfile = "log.txt") {
  cat(format(Sys.time(), "[%Y-%m-%d %X]\t"), msg, "\n", file = logfile, append = T)
}


# read and log script arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  message("Station name must be supplied as script argument.")
  logtext("ERROR: Script called without station name.")
  Sys.sleep(3)
  stop(call. = F)
} else {
  message("Script called with args: ", args)
  logtext(paste("Script called with args:", args))
}


# load required libraries
if(!require(dplyr)){install.packages("dplyr")}
if(!require(readr)){install.packages("readr")}
if(!require(googlesheets4)){install.packages("googlesheets4")}


# Define functions --------------------------------------------------------

## load and parse data files from loggers
loadDat <- function(file, loc) {
  
  # parse TOA5 file
  headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
  df = read.csv(file, skip = 4, header = F)
  colnames(df) = headers
  
  # add some columns
  df %>%
    mutate(Location = loc) %>%
    mutate(Date = as.Date(TIMESTAMP)) %>%
    mutate(Year = as.numeric(format(Date, "%Y"))) %>%
    mutate(HiRH = case_when(AvgHrRH >= 95 ~ 1, T ~ 0))
}


## generate dsv from leaf wetness hours and avg temp during high RH hours
dsv <- function(tavgC, lw) {
  
  # return 0 if arguments are NA
  if (is.na(tavgC) | is.na(lw)) {
    return(0)
  }
  
  # return dsvs based on temp and leaf wetness
  if (tavgC > 13 & tavgC < 18) {
    return(case_when(
      lw <=  6 ~ 0,
      lw <= 15 ~ 1,
      lw <= 20 ~ 2,
      T ~ 3))
  }
  if (tavgC >= 18 & tavgC < 21) {
    return(case_when(
      lw <=  3 ~ 0,
      lw <=  8 ~ 1,
      lw <= 15 ~ 2,
      lw <= 22 ~ 3,
      T ~ 4))
  }
  if (tavgC >= 21 & tavgC < 26) {
    return(case_when(
      lw <=  2 ~ 0,
      lw <=  5 ~ 1,
      lw <= 12 ~ 2,
      lw <= 20 ~ 3,
      T ~ 4))
  }
  if (tavgC >= 26 & tavgC < 29) {
    return(case_when(
      lw <=  3 ~ 0,
      lw <=  8 ~ 1,
      lw <= 15 ~ 2,
      lw <= 22 ~ 3,
      T ~ 4))
  }
  return(0)
}


## function generates Farenheit p-days from daily min/max temps
pday <- function(tminF, tmaxF) {

  # check for NA arguments
  if (is.na(tminF) | is.na(tmaxF)) {return(0)}
  
  # calculate temperature quartiles
  t = data.frame(
    t1 = tminF,
    t2 = ((2 * tminF) + tmaxF) / 3,
    t3 = (tminF + (2 * tmaxF)) / 3,
    t4 = tmaxF
  )
  
  # calculate unmodified pdays from temperatures
  tm =
    mutate_all(t,
      function(t) {
        case_when(t < 45 ~ 0,
          between(t, 45, 70) ~ 10 * (1 - (((t - 70) ^ 2) / 625)),
          between(t, 70, 85) ~ 10 * (1 - (((t - 70) ^ 2) / 256)),
          T ~ 0)
      })
  
  # weight and sum pdays
  Fpday = with(tm, (1 / 24) * ((5 * t1) + (8 * t2) + (8 * t3) + (3 * t4)))
  
  # return integer pdays
  return(round(Fpday, 2))
}


## convert hourly readings to daily summaries
makeDaily <- function(df) {

  # average temps from high RH hours
  HiRH = df %>%
    filter(HiRH == 1) %>%
    group_by(Date) %>%
    summarise(TavgC.HiRH = mean(Tair_C_Avg), .groups = "drop")
  
  # main daily summary
  daily = df %>%
    group_by(Year, Location, Date) %>%
    mutate(
      TminF = (Tair_C_Min * 9 / 5) + 32,
      TmaxF = (Tair_C_Max * 9 / 5) + 32,
      TavgF = (Tair_C_Avg * 9 / 5) + 32
    ) %>%
    summarise(
      TminC = min(Tair_C_Min),
      TmaxC = max(Tair_C_Max),
      TavgC = mean(Tair_C_Avg),
      TminF = min(TminF),
      TmaxF = max(TmaxF),
      TavgF = mean(TavgF),
      MinRH = min(AvgHrRH),
      MaxRH = max(AvgHrRH),
      MeanRH = mean(AvgHrRH),
      HrsHiRH = sum(HiRH),
      PrecipIn = sum(Rain_in_Tot),
      .groups = "drop"
    ) %>%
    group_by(Year) %>%
    mutate(PrecipCumIn = cumsum(PrecipIn))
  
  joined = daily %>%
    left_join(HiRH) %>%
    group_by(Year) %>%
    mutate(DSV = mapply(dsv, .$TavgC.HiRH, .$HrsHiRH)) %>%
    mutate(DSVcum = cumsum(DSV)) %>%
    mutate(Pday = mapply(pday, .$TminF, .$TmaxF)) %>%
    mutate(Pdaycum = cumsum(Pday)) %>%
    ungroup() %>%
    mutate_if(is.numeric, round, 2) %>%
    select(-"TavgC.HiRH")
  
  # replace NA with zero
  joined[is.na(joined)] = 0
  
  return(joined)
}



# Process station data ----------------------------------------------------

gs <- "1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w" # target google sheet
dateCutoff <- "2020-05-01" # filter values before this date
year = "2020"


runDat <- function(arg, name, remote) {
  local = paste0(arg, ".dat")
  file.copy(remote, local, overwrite = T)
  hourly = loadDat(local, name) %>% filter(Date >= dateCutoff)
  daily = makeDaily(hourly)
  write_csv(hourly, paste0(arg, "-hourly-", year, ".csv"))
  write_csv(daily, paste0(arg, "-", year, ".csv"))
  write_sheet(daily, gs, arg)
  logtext(paste("Script completed for station", arg))
}


if (args[1] == "han") {
  runDat("han", "Hancock", "C:/Campbellsci/LoggerNet/Hancock_Hr1.dat")
} else if (args[1] == "gma") {
  runDat("gma", "Grand Marsh", "C:/Campbellsci/LoggerNet/GrandMarsh_Hr1.dat")
} else if (args[1] == "plo") {
  runDat("plo", "Plover", "C:/Campbellsci/LoggerNet/Plover_Hr1.dat")
} else if (args[1] == "ant") {
  runDat("ant", "Antigo", "C:/Campbellsci/LoggerNet/Antigo_Hr1.dat")
} else {
  message("'", args[1], "' is not a valid station name.")
  logtext("ERROR: Invalid station name.")
  Sys.sleep(3)
}


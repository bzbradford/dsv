# Process CR1000 data logger outputs and generate DSV and PDays
# Developed by Ben Bradford, UW Madison, 2020 (bbradford@wisc.edu)

# Must call this script with the station name supplied as an argument



# Setup -------------------------------------------------------------------

if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(googlesheets4)) install.packages("googlesheets4")

setwd("C:/dsv")



# Define functions --------------------------------------------------------

# convert temps
c_to_f <- function(temp) {
  temp * 9 / 5.0 + 32
}


# debugging
logtext <- function(msg, logfile = "log.txt") {
  cat(format(Sys.time(), "\n[%Y-%m-%d %X] "), msg, "\n", file = logfile, append = T)
}


# load and parse data files from loggers
loadDat <- function(file, loc) {
  
  # parse TOA5 file
  headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
  df = read.csv(file, skip = 4, header = F)
  colnames(df) = headers
  
  # remove dubious values
  df[df < -25] <- NA
  
  # add some columns
  df %>%
    as_tibble() %>%
    select(-contains("soil")) %>%
    mutate(
      Location = loc,
      DateTime = parse_datetime(TIMESTAMP) - 60 * 60, # timestamp is end of hour, shift to start of hour
      Date = as.Date(DateTime),
      Year = as.numeric(format(Date, "%Y")),
      .after = RECORD) %>%
    mutate(HiRH = case_when(AvgHrRH >= 95 ~ 1, T ~ 0)) %>%
    mutate(across(contains("_C_"), c_to_f, .names = "{.col}_F"), .before = "Rain_in_Tot") %>%
    rename_with(~ gsub("_C", "", .x), contains("_F")) %>%
    rename_with(~ paste0(gsub("_C", "", .x), "_C"), .cols = contains("_C_")) %>%
    select(-"TIMESTAMP") %>%
    mutate(RECORD = row_number() - 1)
}


# generate dsv from leaf wetness hours and avg temp during high RH hours
dsv <- function(tavgC, lw) {
  
  # return 0 if arguments are NA
  if (is.na(tavgC) | is.na(lw)) return(0)
  
  # return dsvs based on temp and leaf wetness
  if (tavgC > 13 & tavgC < 18) {
    if (lw <= 6) return(0)
    if (lw <= 15) return(1)
    if (lw <= 20) return(2)
    return(3)
  }
  
  if (tavgC >= 18 & tavgC < 21) {
    if (lw <= 3) return(0)
    if (lw <= 8) return(1)
    if (lw <= 15) return(2)
    if (lw <= 22) return(3)
    return(4)
  }
  
  if (tavgC >= 21 & tavgC < 26) {
    if (lw <= 2) return(0)
    if (lw <= 5) return(1)
    if (lw <= 12) return(2)
    if (lw <= 20) return(3)
    return(4)
  }
  
  if (tavgC >= 26) {
    if (lw <= 3) return(0)
    if (lw <= 8) return(1)
    if (lw <= 15) return(2)
    if (lw <= 22) return(3)
    return(4)
  }
  
  return(0)
}


# function generates Farenheit p-days from daily min/max temps
pday <- function(tminF, tmaxF) {

  # check for NA arguments
  if (is.na(tminF) | is.na(tmaxF)) return(0)
  
  # calculate temperature quartiles
  t <- tibble(
    t1 = tminF,
    t2 = ((2 * tminF) + tmaxF) / 3,
    t3 = (tminF + (2 * tmaxF)) / 3,
    t4 = tmaxF
  )
  
  # calculate unmodified pdays from temperatures
  tm <- t %>%
    mutate_all(function(t) {
      case_when(t < 45 ~ 0,
        between(t, 45, 70) ~ 10 * (1 - (((t - 70) ^ 2) / 625)),
        between(t, 70, 85) ~ 10 * (1 - (((t - 70) ^ 2) / 256)),
        T ~ 0)
    })
  
  # weight and sum pdays
  Fpday <- with(tm, (1 / 24) * ((5 * t1) + (8 * t2) + (8 * t3) + (3 * t4)))
  
  # return integer pdays
  return(round(Fpday, 2))
}


# convert hourly readings to daily summaries
makeDaily <- function(df) {

  # average temps from high RH hours
  HiRH <- df %>%
    filter(HiRH == 1) %>%
    group_by(Date) %>%
    summarise(TavgC.HiRH = mean(Tair_Avg_C), .groups = "drop")
  
  # main daily summary
  daily <- df %>%
    group_by(Year, Location, Date, DayOfYear) %>%
    summarise(
      TminC = min(Tair_Min_C, na.rm = T),
      TmaxC = max(Tair_Max_C, na.rm = T),
      TavgC = mean(Tair_Avg_C, na.rm = T),
      TminF = min(Tair_Min_F, na.rm = T),
      TmaxF = max(Tair_Max_F, na.rm = T),
      TavgF = mean(Tair_Avg_F, na.rm = T),
      MinRH = min(AvgHrRH, na.rm = T),
      MaxRH = max(AvgHrRH, na.rm = T),
      MeanRH = mean(AvgHrRH, na.rm = T),
      HrsHiRH = sum(HiRH, na.rm = T),
      PrecipIn = sum(Rain_in_Tot, na.rm = T),
      .groups = "drop") %>%
    group_by(Year) %>%
    mutate(PrecipCumIn = cumsum(PrecipIn))
  
  joined <- daily %>%
    left_join(HiRH, by = "Date") %>%
    group_by(Year) %>%
    mutate(
      DSV = mapply(dsv, .$TavgC.HiRH, .$HrsHiRH),
      DSVcum = cumsum(DSV),
      Pday = mapply(pday, .$TminF, .$TmaxF)) %>%
    mutate(Pdaycum = cumsum(Pday)) %>%
    ungroup() %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(RECORD = row_number() - 1, .before = everything())
  
  # replace NA with zero
  joined[is.na(joined)] <- 0
  
  return(joined)
}


# main process
runDat <- function(arg, name, remote) {
  local <- paste0(arg, ".dat")
  
  # copy dat to working directory
  file.copy(remote, local, overwrite = T)
  
  # process data
  hourly <- loadDat(local, name) %>%
    filter(Date >= dateCutoff)
  daily <- makeDaily(hourly)
  
  # save locally to csv
  write_csv(hourly, paste0(arg, "-hourly-", year, ".csv"))
  write_csv(daily, paste0(arg, "-", year, ".csv"))
  
  # upload to google sheets
  write_sheet(daily, gs_daily, name)
  write_sheet(hourly, gs_hourly, name)
  
  logtext(paste("Script completed for", name))
}



# Process station data ----------------------------------------------------

# google sheets targets
gs_daily <- "1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w"
gs_hourly <- "1cISP1FmwP25FY41o0pSSKARW0g7cbleaSJOLslrOaUo"

# filter values before this date
year <- strftime(Sys.Date(), "%Y")
dateCutoff <- paste0(year, "-05-01")

# valid stations
stn_codes <- c("han", "gma", "plo", "ant")
stn_names <- list(
  "han" = "Hancock",
  "gma" = "Grand Marsh",
  "plo" = "Plover",
  "ant" = "Antigo"
)
stn_files <- list(
  "han" = "C:/Campbellsci/LoggerNet/Hancock_Hourly.dat",
  "gma" = "C:/Campbellsci/LoggerNet/GrandMarsh_Hourly.dat",
  "plo" = "C:/Campbellsci/LoggerNet/Plover_Hourly.dat",
  "ant" = "C:/Campbellsci/LoggerNet/Antigo_Hourly.dat"
)

# read and log script arguments
args <- commandArgs(trailingOnly = TRUE)

message("Script called with args: ", args)
logtext(paste("Script called with args:", args))

arg <- args[1]

if (arg %in% stn_codes) {
  runDat(arg, stn_names[[arg]], stn_files[[arg]])
} else {
  message("'", arg, "' is not a valid station name.")
  logtext(paste0("ERROR: Invalid station name '", arg, "'"))
  Sys.sleep(3)
}


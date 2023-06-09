# Process CR1000 data logger outputs and generate DSV and PDays
# Developed by Ben Bradford, UW Madison, 2019 (bbradford@wisc.edu)

if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(googlesheets4)) install.packages("googlesheets4")

# Must authenticate if no cached oauth token exists
# gs4_auth()


# Define functions --------------------------------------------------------

c_to_f <- function(x) {
  x * 9 / 5 + 32
}

in_to_mm <- function(x) {
  x * 25.4
}

# debugging
logtext <- function(msg, logfile = "log.txt") {
  message(msg)
  cat(format(Sys.time(), "\n[%Y-%m-%d %X] "), msg, "\n", file = logfile, append = T)
}


# generate dsv from leaf wetness hours and avg temp during high RH hours
# leaf wetness (hours high RH) lw
# temp t in Celsius
dsv <- function(t, lw) {
  
  # return 0 if arguments are NA
  if (is.na(t) | is.na(lw)) {
    return(0)
  }
  
  # return dsvs based on temp and leaf wetness
  if (t < 7.2) return(0)
  
  if (t <= 11.6) {
    if (lw <= 16) return(0)
    if (lw <= 18) return(1)
    if (lw <= 21) return(2)
    return(3)
  }
  
  if (t <= 15) {
    if (lw <= 13) return(0)
    if (lw <= 15) return(1)
    if (lw <= 18) return(2)
    if (lw <= 21) return(3)
    return(4)
  }
  
  if (t <= 26.6) {
    if (lw <= 10) return(0)
    if (lw <= 12) return(1)
    if (lw <= 15) return(2)
    if (lw <= 18) return(3)
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


# load and parse data files from loggers
loadDat <- function(file, loc, yr = format(Sys.Date(), "%Y")) {
  
  # parse TOA5 file
  headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
  df = read.csv(file, skip = 4, header = F)
  colnames(df) = headers
  
  # format input data
  df %>%
    as_tibble() %>%
    select(-RECORD) %>%
    mutate(
      Location = loc,
      Date = as.Date(TIMESTAMP),
      Year = year(Date),
      DayOfYear = yday(Date)
    ) %>%
    filter(Year == yr) %>%
    select(
      Location,
      Timestamp = TIMESTAMP,
      Date,
      Year,
      DOY = DayOfYear,
      HourOfDay,
      MinTempC = Tair_C_Min,
      MaxTempC = Tair_C_Max,
      AvgTempC = Tair_C_Avg,
      MinSoilTempC = Tsoil_C_Min,
      MaxSoilTempC = Tsoil_C_Max,
      AvgSoilTempC = Tsoil_C_Avg,
      PrecipIn = Rain_in_Tot,
      AvgHrRH,
      everything()
    ) %>%
    mutate(
      MinTempF = c_to_f(MinTempC),
      MaxTempF = c_to_f(MaxTempC),
      AvgTempF = c_to_f(AvgTempC),
      .after = AvgTempC
    ) %>%
    mutate(
      MinSoilTempF = c_to_f(MinSoilTempC),
      MaxSoilTempF = c_to_f(MaxSoilTempC),
      AvgSoilTempF = c_to_f(AvgSoilTempC),
      .after = AvgSoilTempC
    ) %>%
    mutate(
      PrecipMm = in_to_mm(PrecipIn), .after = PrecipIn
    ) %>%
    mutate(HiRH = ifelse(AvgHrRH >= 95, 1, 0), .after = AvgHrRH) %>%
    mutate(across(
      where(is.numeric),  ~ case_when(
        is.nan(.x) ~ NA,
        .x < 0 ~ NA,
        T ~ .x
      ))
    ) %>%
    mutate(Record = row_number() - 1, .before = everything())
}



# convert hourly readings to daily summaries
makeDaily <- function(hourly) {
  
  # average temps from high RH hours
  HiRH <- hourly %>%
    filter(HiRH == 1) %>%
    group_by(Date) %>%
    summarise(AvgTempC_HiRH = mean(AvgTempC), .groups = "drop")
  
  # main daily summary
  daily <- hourly %>%
    group_by(Year, Location, Date) %>%
    summarise(
      MinTempC = min(MinTempC),
      MaxTempC = max(MaxTempC),
      AvgTempC = mean(AvgTempC),
      MinTempF = min(MinTempF),
      MaxTempF = max(MaxTempF),
      AvgTempF = mean(AvgTempF),
      AvgSoilTempC = mean(AvgSoilTempC),
      AvgSoilTempF = mean(AvgSoilTempF),
      MinRH = min(AvgHrRH),
      MaxRH = max(AvgHrRH),
      AvgRH = mean(AvgHrRH),
      HrsHiRH = sum(HiRH),
      PrecipIn = sum(PrecipIn),
      PrecipMm = sum(PrecipMm),
      .groups = "drop") %>%
    group_by(Year) %>%
    mutate(
      CumPrecipIn = cumsum(PrecipIn),
      CumPrecipMm = cumsum(PrecipMm)
    )
  
  joined <- daily %>%
    left_join(HiRH, by = "Date") %>%
    group_by(Year) %>%
    mutate(
      DSV = mapply(dsv, .$AvgTempC_HiRH, .$HrsHiRH),
      CumDSV = cumsum(DSV),
      PDay = mapply(pday, .$MinTempF, .$MaxTempF),
      CumPDay = cumsum(PDay)
    ) %>%
    mutate() %>%
    ungroup() %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(Record = row_number() - 1, .before = everything())
  
  # Pyranometer data
  if ("SlrMJm2_Tot" %in% names(hourly)) {
    insol <- hourly %>%
      group_by(Date) %>%
      summarise(SlrMJm2_Tot = round(sum(SlrMJm2_Tot), 2))
    joined <- joined %>%
      left_join(insol, by = "Date")
  }
  
  return(joined)
}

# Main process
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


# Define variables ----

# google sheets targets
gs_daily <- "1sKiKiZzLbW0C2y0piXrkJkxYlLYq2GW_DC45tvdinzk"
gs_hourly <- "1YvizlFmpkq5rVYhzuAylTmUQQRKPyAS3thBW8OCxOhs"

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


# Process CR1000 data logger outputs and generate DSV and PDays
# Developed by Ben Bradford, UW Madison, 2019 (bbradford@wisc.edu)

library(tidyverse)


library(googlesheets4)
gs4_auth()


# Define functions --------------------------------------------------------

# load and parse data files from loggers
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

# generate dsv from leaf wetness hours and avg temp during high RH hours
dsv <- function(tavgC, lw) {
  
  # return 0 if arguments are NA
  if (is.na(tavgC) | is.na(lw)) {
    return(0)
  }
  
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
    summarise(TavgC.HiRH = mean(Tair_C_Avg), .groups = "drop")
  
  # main daily summary
  daily <- df %>%
    group_by(Year, Location, Date) %>%
    mutate(
      TminF = (Tair_C_Min * 9 / 5) + 32,
      TmaxF = (Tair_C_Max * 9 / 5) + 32,
      TavgF = (Tair_C_Avg * 9 / 5) + 32) %>%
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
      .groups = "drop") %>%
    group_by(Year) %>%
    mutate(PrecipCumIn = cumsum(PrecipIn))
  
  joined <- daily %>%
    left_join(HiRH) %>%
    group_by(Year) %>%
    mutate(
      DSV = mapply(dsv, .$TavgC.HiRH, .$HrsHiRH),
      DSVcum = cumsum(DSV),
      Pday = mapply(pday, .$TminF, .$TmaxF)) %>%
    mutate(Pdaycum = cumsum(Pday)) %>%
    ungroup() %>%
    mutate_if(is.numeric, round, 2)
  
  # replace NA with zero
  joined[is.na(joined)] <- 0
  
  return(joined)
}



# process dat files -------------------------------------------------------

gma_old <- loadDat("old_data/GrandMarsh_Hr1.dat", "Grand Marsh")
han_old <- loadDat("old_data/Hancock_Hr1.dat", "Hancock")
plo_old <- loadDat("old_data/Plover_Hr1.dat", "Plover")

gma_old %>%
  group_by(Year) %>%
  do(write_csv(makeDaily(.), paste0("old_data/GrandMarsh_", .$Year[1], ".csv")))

han_old %>%
  group_by(Year) %>%
  do(write_csv(makeDaily(.), paste0("old_data/Hancock_", .$Year[1], ".csv")))

plo_old %>%
  group_by(Year) %>%
  do(write_csv(makeDaily(.), paste0("old_data/Plover_", .$Year[1], ".csv")))




han_old %>%
  group_by(Year) %>%
  do(write_csv(makeDaily(.), paste0("test_data/Hancock_", .$Year[1], ".csv")))


han_2019 <- filter(han_old, Year == 2019)

han_2019_daily <- makeDaily(han_2019)

han_2020 <- loadDat("han.dat", "Hancock")
han_2020_daily <- makeDaily(han_2020)
write_csv(han_2020_daily, "han-2020.csv")

gma_2020 <- loadDat("gma.dat", "Grand Marsh")
write_csv(makeDaily(gma_2020), "gma-2020.csv")

plo_2020 <- loadDat("plo.dat", "Plover")
write_csv(makeDaily(plo_2020), "plo-2020.csv")



# 2022 --------------------------------------------------------------------

file = "han.dat"
loc = "Hancock"
arg = "han"

han_hourly <- loadDat("han.dat", "Hancock")
write_csv(hourly, paste0(arg, "-hourly-", year, ".csv"))
write_sheet(daily, gs_hourly, arg)


han_daily = makeDaily(han_hourly)

sure_harvest_hourly

han_hourly %>%
  select(
    Location,
    Record = RECORD,
    DateTime = TIMESTAMP,
    DayOfYear,
    HourOfDay,
    Tair_C_Min,
    Tair_C_Max,
    Rain_in_Tot,
    AvgHrRH)
  
han_hourly %>%
  mutate(across(contains("_C_"), c_to_f, .names = "{.col}_F")) %>%
  rename_with(~ gsub("_C", "", .x), contains("_F")) %>%
  view()


c_to_f <- function(temp) {
  temp * 9 / 5.0 + 32
}

hourly <- loadDat("han.dat", "Hancock")
write_csv(hourly, paste0(arg, "-hourly-", year, ".csv"))
write_sheet(hourly, gs_hourly, arg)


daily <- makeDaily(hourly)
write_csv(daily, paste0(arg, "-", year, ".csv"))
write_sheet(daily, gs_daily, arg)


hourly
gsub("_C_", "_F_", names(hourly))

hourly %>%
  rename_with(~ paste0(gsub("_C", "", .x), "_C"), .cols = contains("_C_"))

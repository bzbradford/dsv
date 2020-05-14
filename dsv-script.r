#!Rscript

# Process CR1000 data logger outputs and generate DSV and PDays
# Developed by Ben Bradford, UW Madison, 2019 (bbradford@wisc.edu)

args = commandArgs(trailingOnly = TRUE)
stationNames = c("han", "gma", "plo", "ant")

# test if there is at least one argument: if not, return an error
if (length(args) == 0) {
  stop("Station name must be supplied as an argument (han, gma, plo, ant)",
       call. = FALSE)
} else if (!(args[1] %in% stationNames)) {
  stop("Invalid station name supplied", call. = FALSE)
}

if (length(args) > 1) {message("Ignoring extra arguments")}

# load required libraries
library(dplyr)
library(readr)
library(googlesheets4)

# load and parse data files from loggers
loadDat = function(file, loc) {
  require(dplyr)
  
  # parse file
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
  require(dplyr)
  
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

# function generates Farenheit p-days from daily min/max temps
pday <- function(tminF, tmaxF) {
  require(dplyr)
  
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

# convert hourly readings to daily summaries
makeDaily <- function(df) {
  require(dplyr)
  
  # average temps from high RH hours
  HiRH = df %>%
    filter(HiRH == 1) %>%
    group_by(Date) %>%
    summarise(TavgC.HiRH = mean(Tair_C_Avg))
  
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
      PrecipIn = sum(Rain_in_Tot)
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

# define main function
main <- function(ws) {
  require(dplyr)
  require(readr)
  require(googlesheets4)
  
  setwd("C:/dsv")
  
  # set target google sheet
  gs <- "1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w"
  
  
  if (ws == "han") {
    message("Processing Hancock station data...")
    file.copy("C:/Campbellsci/LoggerNet/Hancock_Hr1.dat",
              "han.dat",
              overwrite = TRUE)
    han_hr <-
      loadDat("han.dat", "Hancock") %>% filter(Date >= "2020-05-01")
    han <- makeDaily(han_hr)
    write_csv(han_hr, "han-hourly-2020.csv")
    write_csv(han, "han-2020.csv")
    write_sheet(han, gs, "han")
  } else if (ws == "gma") {
    message("Processing Grand Marsh station data...")
    file.copy("C:/Campbellsci/LoggerNet/GrandMarsh_Hr1.dat",
              "gma.dat",
              overwrite = TRUE)
    gma_hr <-
      loadDat("gma.dat", "Grand Marsh") %>% filter(Date >= "2020-05-01")
    gma <- makeDaily(gma_hr)
    write_csv(gma_hr, "gma-hourly-2020.csv")
    write_csv(gma, "gma-2020.csv")
    write_sheet(gma, ss = "1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w", sheet = "gma")
  } else if (ws == "plo") {
    message("Processing Plover station data...")
    file.copy("C:/Campbellsci/LoggerNet/Plover_Hr1.dat",
              "plo.dat",
              overwrite = TRUE)
    plo_hr <-
      loadDat("plo.dat", "Plover") %>% filter(Date >= "2020-05-01")
    plo <- makeDaily(plo_hr)
    write_csv(plo_hr, "plo-hourly-2020.csv")
    write_csv(plo, "plo-2020.csv")
    write_sheet(plo, ss = "1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w", sheet = "plo")
  } else if (ws == "ant") {
    message("Processing Antigo station data...")
    file.copy("C:/Campbellsci/LoggerNet/Antigo_Hr1.dat",
              "ant.dat",
              overwrite = TRUE)
    ant_hr <-
      loadDat("ant.dat", "Antigo") %>% filter(Date >= "2020-05-01")
    ant <- makeDaily(ant_hr)
    write_csv(ant_hr, "ant-hourly-2020.csv")
    write_csv(ant, "ant-2020.csv")
    write_sheet(ant, ss = "1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w", sheet = "ant")
  } else {
    message("No applicable processes for station '", ws, "'")
  }
}

# run main function
main(args[1])
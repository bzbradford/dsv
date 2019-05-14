# Required libraries
library(tidyverse)
library(googlesheets)

# Function definitions ----

loadCSV = function(file, loc) {
  # read cleaned csv
  read.csv(file, stringsAsFactors = F) %>%
    mutate(Location = loc,
           Date = as.Date(TIMESTAMP)) %>%
    mutate(Year = year(Date),
      HiRH = case_when(AvgHrRH >= 95 ~ 1, T ~ 0))
}

loadDat = function(file, loc) {
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

ctof = function(c) {
  (c * 9/5) + 32
}

# generate dsv from leaf wetness hours and avg temp during high RH hours
dsv = function(tavgC, lw) {
  # return 0 if arguments are NA
  if (is.na(tavgC) | is.na(lw)) {
    return(0)
  }
  
  # return dsvs based on temp and leaf wetness
  if (between(tavgC, 13, 18)) {
    return(case_when(
      lw <=  6 ~ 0,
      lw <= 15 ~ 1,
      lw <= 20 ~ 2,
      T ~ 3))
  }
  if (between(tavgC, 18, 21)) {
    return(case_when(
      lw <=  3 ~ 0,
      lw <=  8 ~ 1,
      lw <= 15 ~ 2,
      lw <= 22 ~ 3,
      T ~ 4))
  }
  if (between(tavgC, 21, 26)) {
    return(case_when(
      lw <=  2 ~ 0,
      lw <=  5 ~ 1,
      lw <= 12 ~ 2,
      lw <= 20 ~ 3,
      T ~ 4))
  }
  if (between(tavgC, 26, 29)) {
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
pday = function(tminF, tmaxF) {
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
makeDaily = function(df) {
  require(tidyverse)
  # main daily summary
  left = df %>%
    group_by(Year, Location, Date) %>%
    summarise(
      TminC = min(Tair_C_Min),
      TmaxC = max(Tair_C_Max),
      TavgC = mean(Tair_C_Avg),
      PrecipIn = sum(Rain_in_Tot),
      HrsHiRH = sum(HiRH)
    ) %>%
    mutate(
      TminF = ctof(TminC),
      TmaxF = ctof(TmaxC),
      tavgF = ctof(TavgC)
    )
  
  # average temps from high RH hours
  right = df %>%
    filter(HiRH == 1) %>%
    group_by(Location, Date) %>%
    summarise(TavgC.HiRH = mean(Tair_C_Avg))
  
  # join and return data frame
  joined = left_join(left, right) %>%
    ungroup() %>%
    mutate(DSV = mapply(dsv, .$TavgC.HiRH, .$HrsHiRH)) %>%
    mutate(Pday = mapply(pday, .$TminF, .$TmaxF)) %>%
    group_by(Year) %>%
    mutate(DSVcum = cumsum(DSV)) %>%
    mutate(Pdaycum = cumsum(Pday)) %>%
    ungroup()
  
  # replace NA with zero
  joined[is.na(joined)] = 0
  
  return(joined)
}


# Process 2018 csvs ----

han = makeDaily(loadCSV("data/han.csv", "Hancock") %>% filter(Year == 2018))
write.csv(han, "han18.csv")

gma = makeDaily(loadCSV("data/gma.csv", "GrandMarsh") %>% filter(Year == 2018))
write.csv(gma, "gma18.csv")

plo = makeDaily(loadCSV("data/plo.csv", "Plover") %>% filter(Year == 2018))
write.csv(plo, "plo18.csv")




# generate datasets ----

han =
  loadDat("data/Hancock CR1000_Hr1.dat", "Hancock") %>%
  makeDaily()
write.csv(han, "han19.csv")

gma =
  loadDat("data/Grand Marsh CR1000_Hr1.dat", "GrandMarsh") %>%
  makeDaily()
gma %>% write.csv("gma19.csv")

plo =
  loadDat("data/Plover CR1000_Hr1.dat", "Plover") %>%
  makeDaily()
plo %>% write.csv("plo19.csv")


# upload to google sheets ----
gs = gs_key("1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w")

# google test
gs_edit_cells(ss = gs, ws = "han", input = han, anchor = "A1", byrow = T)
gs_edit_cells(ss = gs, ws = "gma", input = gma, anchor = "A1", byrow = T)
gs_edit_cells(ss = gs, ws = "plo", input = plo, anchor = "A1", byrow = T)

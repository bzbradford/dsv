# Process CR1000 data logger outputs and generate DSV and PDays
# Developed by Ben Bradford, UW Madison, 2019 (bbradford@wisc.edu)


# Function definitions ----

# load and parse data files from loggers
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
  Daily = df %>%
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
    )
  
  # average temps from high RH hours
  HiRH = df %>%
    filter(HiRH == 1) %>%
    group_by(Date) %>%
    summarise(TavgC.HiRH = mean(Tair_C_Avg))
  
  joined = left_join(Daily, HiRH) %>%
    group_by(Year) %>%
    mutate(PrecipCumIn = cumsum(PrecipIn)) %>%
    mutate(DSV = mapply(dsv, .$TavgC.HiRH, .$HrsHiRH)) %>%
    mutate(DSVcum = cumsum(DSV)) %>%
    mutate(Pday = mapply(pday, .$TminF, .$TmaxF)) %>%
    mutate(Pdaycum = cumsum(Pday)) %>%
    select(-"TavgC.HiRH") %>%
    ungroup() %>%
    mutate_if(is.numeric, round, 2)
  
  # replace NA with zero
  joined[is.na(joined)] = 0
  
  return(joined)
}


# Run this section ----

require(tidyverse)

han =
  loadDat("C:/Campbellsci/LoggerNet/Data/Hancock_Hr1.dat", "Hancock") %>%
  filter(Date >= "2019-05-01") %>%
  makeDaily()
write.csv(han, "han19.csv")

gma =
  loadDat("C:/Campbellsci/LoggerNet/Data/GrandMarsh_Hr1.dat", "GrandMarsh") %>%
  filter(Date >= "2019-05-01") %>%
  makeDaily()
write.csv(gma, "gma19.csv")

plo =
  loadDat("C:/Campbellsci/LoggerNet/Data/Plover_Hr1.dat", "Plover") %>%
  filter(Date >= "2019-05-01") %>%
  makeDaily()
write.csv(plo, "plo19.csv")

# ant =
#   loadDat("C:/Campbellsci/LoggerNet/Data/Antigo_Hr1.dat", "Antigo") %>%
#   filter(Date >= "2019-05-01") %>%
#   makeDaily()
# write.csv(ant, "ant19.csv")

# upload to google sheets
require(googlesheets)
#gs = gs_key("1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w")

gs_edit_cells(ss = gs, ws = "han", input = han, anchor = "A1", byrow = T)
gs_edit_cells(ss = gs, ws = "gma", input = gma, anchor = "A1", byrow = T)
gs_edit_cells(ss = gs, ws = "plo", input = plo, anchor = "A1", byrow = T)
#gs_edit_cells(ss = gs, ws = "ant", input = ant, anchor = "A1", byrow = T)

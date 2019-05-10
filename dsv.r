library(tidyverse)
library(lubridate)

# han = read.csv("data/han.csv", stringsAsFactors = F) %>%
#   mutate(Location = "Hancock",
#          Date = as.Date(TIMESTAMP),
#          Time = ymd_hm(TIMESTAMP, tz = "US/Central")) %>%
#   filter(year(Date) == 2018) %>%
#   mutate(HiRH = case_when(AvgHrRH >= 95 ~ 1,
#                           T ~ 0))
# str(han)

loadFile = function(fn, loc) {
  read.csv(fn, stringsAsFactors = F) %>%
    mutate(Location = loc,
           Date = as.Date(TIMESTAMP),
           Time = ymd_hm(TIMESTAMP, tz = "US/Central")) %>%
    mutate(Year = year(Date),
      HiRH = case_when(AvgHrRH >= 95 ~ 1, T ~ 0))
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
  return(as.integer(Fpday))
}

makeDaily = function(df) {
  left = df %>%
    group_by(Location, Date) %>%
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
  right = df %>%
    filter(HiRH == 1) %>%
    group_by(Location, Date) %>%
    summarise(TavgC.HiRH = mean(Tair_C_Avg))
  left_join(left, right) %>%
    mutate(DSV = mapply(dsv, .$TavgC.HiRH, .$HrsHiRH)) %>%
    mutate(DSVcum = cumsum(DSV)) %>%
    mutate(Pday = mapply(pday, .$TminF, .$TmaxF)) %>%
    mutate(Pdaycum = cumsum(Pday))
}

# han_daily =
#   left_join(
#     han %>%
#       group_by(Location, Date) %>%
#       summarise(TminC = min(Tair_C_Min),
#                 TmaxC = max(Tair_C_Max),
#                 TavgC = mean(Tair_C_Avg),
#                 PrecipIn = sum(Rain_in_Tot),
#                 HrsHiRH = sum(HiRH)) %>%
#       mutate(TminF = ctof(TminC),
#              TmaxF = ctof(TmaxC),
#              tavgF = ctof(TavgC)),
#     han %>%
#       filter(HiRH == 1) %>%
#       group_by(Location, Date) %>%
#       summarise(TavgC.HiRH = mean(Tair_C_Avg))
#   ) %>%
#   mutate(DSV = mapply(dsv, .$TavgC.HiRH, .$HrsHiRH)) %>%
#   mutate(DSVcum = cumsum(DSV)) %>%
#   mutate(Pday = mapply(pday, .$TminF, .$TmaxF)) %>%
#   mutate(Pdaycum = cumsum(Pday))
# view(han_daily)


# Run computations ----

han = makeDaily(loadFile("data/han.csv", "Hancock") %>% filter(Year == 2018))
write.csv(han, "han2018.csv")

gma = makeDaily(loadFile("data/gma.csv", "GrandMarsh") %>% filter(Year == 2018))
write.csv(gma, "gma2018.csv")

plo = makeDaily(loadFile("data/plo.csv", "Plover") %>% filter(Year == 2018))
write.csv(plo, "plo2018.csv")



library(tidyverse)
library(lubridate)

han = read.csv("data/han.csv", stringsAsFactors = F) %>%
  mutate(Location = "Hancock",
         Date = as.Date(TIMESTAMP),
         Time = ymd_hm(TIMESTAMP, tz = "US/Central")) %>%
  filter(year(Date) == 2018) %>%
  mutate(HiRH = case_when(AvgHrRH >= 95 ~ 1,
                          T ~ 0))
str(han)

ctof = function(c) {
  (c * 9/5) + 32
}

dsv = function(tavgC, lw) {
  if (is.na(tavgC) | is.na(lw)) {return(0)}
  d = 0
  if (between(tavgC, 13, 18)) {
    d = case_when(lw <=  6 ~ 0,
                  lw <= 15 ~ 1,
                  lw <= 20 ~ 2,
                  T ~ 3)
  }
  if (between(tavgC, 18, 21)) {
    d = case_when(lw <=  3 ~ 0,
                  lw <=  8 ~ 1,
                  lw <= 15 ~ 2,
                  lw <= 22 ~ 3,
                  T ~ 4)
  }
  if (between(tavgC, 21, 26)) {
    d = case_when(lw <=  2 ~ 0,
                  lw <=  5 ~ 1,
                  lw <= 12 ~ 2,
                  lw <= 20 ~ 3,
                  T ~ 4)
  }
  if (between(tavgC, 26, 29)) {
    d = case_when(lw <=  3 ~ 0,
                  lw <=  8 ~ 1,
                  lw <= 15 ~ 2,
                  lw <= 22 ~ 3,
                  T ~ 4)
  }
  return(d)
}

pday = function(tminF, tmaxF) {
  if (is.na(tminF) | is.na(tmaxF)) {return(0)}
  p = 0
  t = data.frame(
    t1 = tminF,
    t2 = ((2 * tminF) + tmaxF) / 3,
    t3 = (tminF + (2 * tmaxF)) / 3,
    t4 = tmaxF
  )
  f = function(t){
    case_when(
      t < 45 ~ 0,
      between(t, 45, 70) ~ 10*(1-(((t-70)^2)/625)),
      between(t, 70, 85) ~ 10*(1-(((t-70)^2)/256)),
      T ~ 0
    )
  }
  tm = mutate_all(t, f)
  as.integer(with(tm, (1 / 24) * ((5 * t1) + (8 * t2) + (8 * t3) + (3 * t4))))
}

han_daily =
  left_join(
    han %>%
      group_by(Location, Date) %>%
      summarise(TminC = min(Tair_C_Min),
                TmaxC = max(Tair_C_Max),
                TavgC = mean(Tair_C_Avg),
                PrecipIn = sum(Rain_in_Tot),
                HrsHiRH = sum(HiRH)) %>%
      mutate(TminF = ctof(TminC),
             TmaxF = ctof(TmaxC),
             tavgF = ctof(TavgC)),
    han %>%
      filter(HiRH == 1) %>%
      group_by(Location, Date) %>%
      summarise(TavgC.HiRH = mean(Tair_C_Avg))
  ) %>%
  mutate(DSV = mapply(dsv, .$TavgC.HiRH, .$HrsHiRH)) %>%
  mutate(DSVcum = cumsum(DSV)) %>%
  mutate(Pday = mapply(pday, .$TminF, .$TmaxF)) %>%
  mutate(Pdaycum = cumsum(Pday))
view(han_daily)



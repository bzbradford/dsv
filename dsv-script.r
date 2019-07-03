require(tidyverse)
require(googlesheets)

setwd("L:/Bradford/Vegpath/DSV")

# load hourlies
han.hourly =
  loadDat("C:/Campbellsci/LoggerNet/Data/Hancock_Hr1.dat", "Hancock") %>%
  filter(Date >= "2019-05-01")
gma.hourly =
  loadDat("C:/Campbellsci/LoggerNet/Data/GrandMarsh_Hr1.dat", "GrandMarsh") %>%
  filter(Date >= "2019-05-01")
plo.hourly =
  loadDat("C:/Campbellsci/LoggerNet/Data/Plover_Hr1.dat", "Plover") %>%
  filter(Date >= "2019-05-01")
ant.hourly =
  loadDat("C:/Campbellsci/LoggerNet/Data/Antigo_Hr1.dat", "Antigo") %>%
  filter(Date >= "2019-05-01")

# make dailies
han = makeDaily(han.hourly)
gma = makeDaily(gma.hourly)
plo = makeDaily(plo.hourly)
ant = makeDaily(ant.hourly)

# write to csv
write.csv(han.hourly, "han19-h.csv")
write.csv(han, "han19.csv")
write.csv(gma.hourly, "gma19-h.csv")
write.csv(gma, "gma19.csv")
write.csv(plo.hourly, "plo19-h.csv")
write.csv(plo, "plo19.csv")
write.csv(ant.hourly, "ant19-h.csv")
write.csv(ant, "ant19.csv")

# upload to google sheets
#gs = gs_key("1cxdccapGiGpp8w2U4ZwlAH_oiwdLvhfniUtHuBhj75w")
gs_edit_cells(ss = gs, ws = "han", input = han, anchor = "A1", byrow = T)
gs_edit_cells(ss = gs, ws = "gma", input = gma, anchor = "A1", byrow = T)
gs_edit_cells(ss = gs, ws = "plo", input = plo, anchor = "A1", byrow = T)
gs_edit_cells(ss = gs, ws = "ant", input = ant, anchor = "A1", byrow = T)

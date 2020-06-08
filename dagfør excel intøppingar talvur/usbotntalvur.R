#script til at uppdatera talvurnar í excelørkunum
#hetta skal koyrast og fílarnir verða skrivaðir niður á eina ávísa mappu
#hetta riggar bert um tú hevur atgong til felagsdrevið hjá Vernd!

goympath <- "I:/VERND/databasi-usbotn/usbotn-mastersheet/datausbotn/"
goympathfilt <- "I:/VERND/databasi-usbotn/usbotn-mastersheet/datausbotnfilt/"

library(tidyverse)
library(DBI)

conread <- dbConnect(odbc::odbc(),
                 Driver="PostgreSQL Unicode(x64)",
                 Server = rstudioapi::showPrompt(title = "Server", message = "Server navn:", default = ""),
                 Database = rstudioapi::showPrompt(title = "Database", message = "Database navn:", default = ""),
                 UID = rstudioapi::showPrompt("Database Username", "Username"), 
                 PWD = rstudioapi::askForPassword("Database password"),
                 Port = 5432,
                 encoding = "windows-1252"
)

dbtaxontree <- tbl(conread, "taxontree") %>% collect()
dbtaxontree <- dbtaxontree %>% arrange(scientific_name)

dbalioki <- tbl(conread, "alioki") %>% select(alioki_nr, alifjord_nr) %>% collect()
dbbotndjor <- tbl(conread, "botndjor") %>% select(botndjor_id) %>% collect()
dbbotnheintari <- tbl(conread, "botnheintari") %>% select(botnheint_id, botnheint_navn) %>% collect()
dbbotnlysing <- tbl(conread, "botnlysing") %>%  select(botnlysing_id) %>% collect()
dbbotnslag <- tbl(conread, "botnslag") %>% select(botnslag_id) %>% collect()
dbbotnsyni <- tbl(conread, "botnsyni") %>% select(botn_id) %>% collect()
dbdjoralivsyni <- tbl(conread, "djoralivsyni") %>% select(djorliv_id, botn_id) %>% collect()
dbdjornogd <- tbl(conread, "djornogd") %>%  select(djornogd_id, djornogd_navn, deprecated) %>% collect()
dbdjorslag <- tbl(conread, "djorslag") %>% select(aphia_id, scientific_name, deprecated, accepted_aphia_id) %>% collect()
dbeind <- tbl(conread, "eind") %>% select(eind_id, eind_navn, deprecated) %>% collect()
dbevjudjugd <- tbl(conread, "evjutjugd") %>% select(evjutjugd_id, evjutjugd_navn, deprecated) %>% collect()
dbevni <- tbl(conread, "evni") %>% select(evni_id, evni_navn, deprecated) %>% collect()
dbfastleiki <- tbl(conread, "fastleiki") %>% select(fastleiki_id, fastleiki_navn, deprecated) %>% collect()
dbgrabbanogd <- tbl(conread, "grabbanogd") %>% select(grabbanogd_id, grabbanogd_navn, deprecated) %>% collect()
dbjn <- tbl(conread, "janei") %>% select(janei_id, janei_navn) %>% collect()
dbkanningarfelag <- tbl(conread, "kanningarfelag") %>% select(kfelag_id, kfelag_navn, deprecated) %>%  collect()
dbkanningarfolk <- tbl(conread, "kanningarfolk") %>% select(kfolk_id, kfolk_navn) %>% collect()
dbkanningarrapport <- tbl(conread, "kanningarrapport") %>% select(rap_id) %>% collect()
dblitur <- tbl(conread, "litur") %>% select(litur_id, litur_navn, deprecated) %>% collect()
dbluktur <- tbl(conread, "luktur") %>% select(luktur_id, luktur_navn, deprecated) %>% collect()
dbstoda <- tbl(conread, "stoda") %>% select(stoda_id, stoda_navn, deprecated) %>% collect()
dbstodslag <- tbl(conread, "stodslag") %>% select(stodslag_id, stodslag_navn, deprecated) %>% collect()
dbupptalt <- tbl(conread, "upptalt") %>% select(upptalt_id, upptalt_navn) %>% collect()
dbutgavuslag <- tbl(conread, "utgavuslag") %>% select(utgavuslag_id, utgavuslag_navn) %>% collect()
dbbasisdetection <- tbl(conread, "basisdetection") %>% select(basis, basis_navn, deprecated) %>% collect()
dbqualflag <- tbl(conread, "qualflag") %>% select(qflag, qflag_navn, deprecated) %>% collect()
dbmethodmatiovissa <- tbl(conread, "methodmatiovissa") %>% select(met_matiovissa, met_matiovissa_navn, deprecated) %>% collect()
dbkanningarstarvsstova <- tbl(conread, "kanningarstarvsstova") %>% select(starvsstova, starvsstova_navn, deprecated) %>% collect()

dbDisconnect(conread)

#skriva allar dataframes til csv, historic input
#listi av dataframes
dflist <- mget(ls(pattern = "db"))
dfnames <- ls(pattern = "db")
#listi av paths sum dataframes skulu goymast til
paths <- paste(goympath, ls(pattern = "db"), ".csv", sep = "")
pathsfilt <- paste(goympathfilt, ls(pattern = "db"), ".csv", sep = "")

#map2(dflist, dfnames, write_csv)

#functión til at filtrera alt sum er deprecated í databasanum
filt_fun <- function(test, dflist) {
  if (test == TRUE) {dflist}
  else {filter(dflist, deprecated == "0")}
}

#dflist eru allar dataframes, hesar skulu brúkast til inntøppingar skjalið sum er "historic"
#filt eru tær filtreraðu dataframes, hesar skulu brúkast til inntøppingar skjalið sum er "current"
dflistfilt <- tibble(dflist, dfnames,paths, pathsfilt) %>%
  mutate(test = map(map(dflist, "deprecated"), is.null)) %>%
  mutate(filt = map2(test, dflist, filt_fun))

#skriva csv fílir til "historic" data
map2(dflistfilt$dflist, dflistfilt$paths, write_csv, na = "")

#skriva csv fílir til "current data
map2(dflistfilt$filt, dflistfilt$pathsfilt, write_csv, na = "")

rm(list = ls())

cat("\n \n talvurnar eru nú uppdateraðar á felagsdrevinum")

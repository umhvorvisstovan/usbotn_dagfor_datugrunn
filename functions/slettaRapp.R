## sletta rapport functión v0.0
library(tidyverse)
library(readxl)
library(odbc)
library(DBI)

slettaRapp <- function(rapportnavn, orsok) {
  
  if(nchar(orsok) > 200) {
    stop("tín ORSOK hevur meira enn 200 tekn")
  }
  
  # hetta opnar forbindilisi til usbotn, tú verður biðin um at inntøppa
  # títt loyniorð í einum promti
  
  condel <- dbConnect(odbc::odbc(),
                      Driver = "PostgreSQL Unicode(x64)",
                      Server = rstudioapi::showPrompt(title = "Server", message = "Server navn:", default = ""),
                      Database = rstudioapi::showPrompt(title = "Database", message = "Database navn:", default = ""), 
                      UID = rstudioapi::showPrompt("Database Username", "Username"), 
                      PWD = rstudioapi::askForPassword("Database password"),
                      Port = 5432,
                      encoding = "windows-1252")
  
  if(dbIsValid(condel) == FALSE) {
    stop("tú hevur ikki atgongd við usbotn dátagrunnin")
  } 
  
  rap_id <- tbl(condel, "kanningarrapport") %>%
    select(rap_id) %>% 
    filter(rap_id == rapportnavn) %>% 
    collect()
  
  if(nrow(rap_id) == 0) {
    stop("Eingin rapport finst við navninum sum tú hevur skriva!")
  }
  
  #dagfør historyrap talvuna
  sql <- "call slettarapp(?rapportnavn, 'delete', ?orsok);"
  sqlrapdelete <- sqlInterpolate(condel, sql, rapportnavn = rapportnavn, orsok = orsok)
  
  DBI::dbExecute(condel, sqlrapdelete)
  DBI::dbDisconnect(condel)
  
  cat(paste("\n \n", rapport, "er nú SLETTAÐ á usbotn! Tú ert eitt skrín :) \n"))
  cat("\n")
  
}
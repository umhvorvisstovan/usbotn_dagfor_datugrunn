## innlesa rapport functión v0.0
library(tidyverse)
library(readxl)
library(odbc)
library(DBI)

innRapp_v0.0 <- function(rapport, alioki, filetoimport) {
  
  #### Eftirkanna hvat fyri versión excel fílan er
  
  if(sum(excel_sheets(filetoimport) == "version") == 1) {
    version_excel <- read_excel(filetoimport, sheet = "version", col_types = "text", n_max = 1)[[1]]
  } else {
    version_excel <- "v0.0"
  }
  
  #hetta "v.0.0" skal broytast tá ein nýggi excel_intøppingar versión verður gjørd!
  #minst til IKKI at sletta gamlar versiónir av rmd fílunum (ella functións scriptinum),
  #tí hesar skulu brúkast til at innlesa gamlar inntøppingar av nýggjum!!
  if((version_excel == "v0.0") == FALSE) {
    stop(paste("hevur tú skrivað rætta versións nr.? versiónin á excel fíluni er: ", version_excel))
  }
  
  #### Read excel
  
  kanningarrapport <- read_excel(filetoimport, 
                                 sheet = "KanningarRapport",
                                 col_types = c("text", "text", "text",
                                               "date", "text", "text",
                                               "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "date", "text", 
                                               "text", "text"), 
                                 n_max = 1)
  GIStype <- kanningarrapport$GIStype
  rap_id <- kanningarrapport$rap_id
  
  kanningarrapport <- kanningarrapport %>% select(-GIStype)
  
  botnsyni <- read_excel(filetoimport, 
                         sheet = "BotnSyni",
                         col_types = c("text", "text", "text", 
                                       "date", "text", "numeric", "numeric",
                                       "text", "text", "text", 
                                       "text", "numeric", "numeric", 
                                       "text", "text", "text",  
                                       "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text"))
  
  synikanningarfolk <- read_excel(filetoimport,
                                  sheet = "SyniKanningarfolk",
                                  col_types = c("text", "text", "text"))
  
  botndjor <- read_excel(filetoimport,
                         sheet = "BotnDjor",
                         col_types = c("text", "text", "text",
                                       "text", "text", "text",
                                       "text", "text", "text",
                                       "text", "text", "text",
                                       "text", "text", "text",
                                       "text", "text"))
  
  urslitsens <- read_excel(filetoimport,
                           sheet = "UrslitSens",
                           col_types = c("text", "text", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric", 
                                         "text", "text", "text",
                                         "text", "text", "text",
                                         "text", "numeric", "numeric",
                                         "text", "numeric", "text",
                                         "text", "text"))
  
  urslitkemi <- read_excel(filetoimport,
                           sheet = "UrslitKemi",
                           col_types = c("text", "text", "numeric",
                                         "text", "text"))
  
  djoralivsyni <- read_excel(filetoimport,
                             sheet = "DjorlivSyni",
                             col_types = c("text", "text", "text",
                                           "text", "text", "text",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "text", "text", "numeric"))
  
  nms <- names(read_excel(filetoimport, sheet = "UrslitDjoraliv"))
  
  urslitdjoraliv <- read_excel(filetoimport,
                               sheet = "UrslitDjoraliv",
                               col_types = c("numeric", "text", "numeric",
                                             "text", 
                                             rep("numeric", length(nms) - 4)))
  
  botnsyni <- botnsyni %>% 
    filter(!is.na(botn_id))%>% 
    mutate(botn_id = paste(rap_id, ":", botn_id, sep = ""))
  synikanningarfolk <- synikanningarfolk %>% 
    filter(!is.na(botn_id))%>% 
    mutate(botn_id = paste(rap_id, ":", botn_id, sep = ""))
  botndjor <- botndjor %>% 
    filter(!is.na(botn_id))%>% 
    mutate(botn_id = paste(rap_id, ":", botn_id, sep = ""))
  urslitsens <- urslitsens %>% 
    filter(!is.na(botn_id))%>% 
    mutate(botn_id = paste(rap_id, ":", botn_id, sep = ""))
  urslitkemi <- urslitkemi %>% 
    filter(!is.na(botn_id))%>% 
    mutate(botn_id = paste(rap_id, ":", botn_id, sep = ""))
  djoralivsyni <- djoralivsyni %>% 
    filter(!is.na(botn_id))%>% 
    mutate(botn_id = paste(rap_id, ":", botn_id, sep = ""),
           djorliv_id = paste(rap_id, ":", djorliv_id, sep = ""))
  urslitdjoraliv <- urslitdjoraliv %>% 
    filter(!is.na(scientific_name))
  
  #kanna fyri duplicate í djóraslag
  duplicate_aphia <- urslitdjoraliv %>%
    filter(!is.na(scientific_name)) %>% 
    group_by(aphia_id_auto, aphia_id_manuelt) %>%
    filter(n()>1) %>%
    select(aphia_id_auto, scientific_name, aphia_id_manuelt)
  if(nrow(duplicate_aphia)>0) {
    stop(paste("tað eru duplicat í djórasløgunum! aphia_id = ", list(unique(duplicate_aphia$scientific_name))))
  }
  
  if (length(djoralivsyni$botn_id)>=1 & nrow(urslitdjoraliv) >= 1) {
    urslitdjoraliv <- urslitdjoraliv %>%
      mutate(aphia_id_auto = if_else(is.na(aphia_id_auto),
                                     aphia_id_manuelt,
                                     aphia_id_auto)) %>%  
      filter(!is.na(aphia_id_auto)) %>% 
      gather(key = "djorliv_id", value = "tal", 5:length(nms)) %>% 
      filter(!is.na(tal)) %>% 
      mutate(djorliv_id = paste(rap_id, ":", djorliv_id, sep = "")) %>%
      rename(aphia_id = aphia_id_auto) %>%
      select(djorliv_id, aphia_id, tal, djoraliv_vidmerking)
  } else {
    urslitdjoraliv <- data.frame(djorliv_id = character(),
                                 aphia_id = numeric(),
                                 tal = numeric(),
                                 djoraliv_vidmerking = character())
  }
  
  #broyt koordinatar til kommatøl
  
  CoordList <- c("plan_lat", "plan_lng", "lat", "lng")
  CoordList2 <- c("lat", "lng")
  
  changeCoord <- function(x) {
    xlab <- deparse(substitute(x))
    df <- data.frame(x)
    df <- df %>% 
      separate(x, c("deg", "min", ".min")) %>% 
      unite("m.mm", c(min, .min), sep = ".", na.rm = TRUE) %>% 
      mutate(m.mm = as.numeric(m.mm)/60) %>% 
      mutate(xlab = (as.numeric(deg)+m.mm)) %>%
      select(-deg, -m.mm)
    df$xlab
  }
  
  if (GIStype == "deg min") {
    botnsyni <- botnsyni %>%
      mutate_at(CoordList, changeCoord)
  } else {
    botnsyni<- botnsyni %>%
      mutate_at(CoordList, as.numeric)
  }
  
  if (GIStype == "deg min") {
    djoralivsyni <- djoralivsyni %>%
      mutate_at(CoordList2, changeCoord)
  } else {
    djoralivsyni <- djoralivsyni %>%
      mutate_at(CoordList2, as.numeric)
  }
  
  botnsyni <- botnsyni %>% 
    mutate(lng = abs(lng) * -1) %>% 
    mutate(plan_lng = abs(plan_lng) * -1) %>% 
    mutate(lat = round(lat, 6),
           lng = round(lng, 6),
           plan_lat = round(plan_lat, 6),
           plan_lng = round(plan_lng, 6))
  
  djoralivsyni <- djoralivsyni %>% 
    mutate(lng = abs(lng) * -1) %>% 
    mutate(lat = round(lat, 6),
           lng = round(lng, 6))
  
  #### clean and normalise the tables so they are ready to import into the database
  
  #botnsyni - normalisera botnslag_id fyri hvørt botnsýni
  urslitbotnslag <- botnsyni %>% 
    select(botn_id, sandur:okent) %>%
    gather(key = "botnslag_id", value = "virdi_jn", -botn_id) %>% 
    filter(virdi_jn == "j") %>%
    select(-virdi_jn) %>% 
    arrange(botn_id)
  
  #botnsyni - tak botnslag burturúr talvuni og legg rap_id afturat sum foreign key
  botnsyni <- botnsyni %>% 
    select(-(sandur:okent)) %>% 
    mutate(rap_id = rep(rap_id, nrow(botnsyni))) %>% 
    left_join(botndjor %>% select(botn_id, bdjor_vidmerking), by = "botn_id") %>% 
    #left_join(urslitsens %>% select(botn_id, urslitsens_vidmerking), by = "botn_id") %>% 
    arrange(botn_id)
  
  #botndjor - talvan skal í tvey og skal normaliserast
  #urslit_botnsyni_djor_skjót
  urslitdjortald <- botndjor %>% 
    select(botn_id, tindadyr:capitella_capitata) %>% 
    gather(key = botndjor_id, value = "djornogd_id", -botn_id) %>% 
    filter(!is.na(djornogd_id)) %>% 
    arrange(botn_id)
  
  #urslit_botnlysing
  urslitbotnlysing <- botndjor %>% 
    select(botn_id, djor_alikervi:kann_djor) %>% 
    gather(key = botnlysing_id, value = "janei_id", -botn_id) %>%
    filter(!is.na(janei_id)) %>% 
    arrange(botn_id)
  
  #urslitkemi - hendan talvan kann koyrast inn sum hon er!
  urslitkemi <- urslitkemi %>% 
    mutate(evni = toupper(evni))
  
  #urslitsens - hetta skal kanska normaliserast...
  urslitsens <- urslitsens %>% 
    rename(ph = pH) %>% 
    mutate(grabbanogd = toupper(grabbanogd),
           evjutjugd = toupper(evjutjugd),
           fastleiki = toupper(fastleiki),
           litur = toupper(litur),
           luktur = toupper(luktur),
           stoda_ii_stig = tolower(stoda_ii_stig),
           stoda_iii_stig = tolower(stoda_iii_stig),
           stoda_iiogiii_stig = tolower(stoda_iiogiii_stig))
  
  #### Kanna fyri feilir í inntøpping
  cat("\n")
  cat("kannar fyri feilir í inntøppingini...")
  cat("\n")
  
  #rap_id ok?
  if((rap_id == rapport) == FALSE){
    stop("rapport navn tú hevur skrivað og tað, sum stendur í excel fíluni eru ikki eins!")
  }
  
  #hetta er fyri at eftirkannað um úrsltini fyri djóralívssýnini eru intappaði við duplicati
  nms_test <- tibble(djorliv_id = names(read_excel(filetoimport, sheet = "UrslitDjoraliv", .name_repair = "minimal"))) %>% 
    group_by_all() %>% 
    filter(n()>1)
  
  if(nrow(nms_test)>0) {
    stop(paste("tað eru duplicat í UrslitDjoraliv! djorliv_id  = ", list(unique(nms_test$djorliv_id))))
  }
  
  #Eftirkanna um synistøku dato er innskrivaður
  botnsyni_dato <- botnsyni %>% filter(is.na(synistoku_dato)) %>% select(botn_id)
  if(nrow(botnsyni_dato)>0) {
    stop(paste("synistøkudato er ikki innskrivaður, botn_id = ", list(unique(botnsyni_dato$botn_id))))
  }
  
  #Kanna botn_id
  #check for dublicates
  botn_id_duplicated <- botnsyni %>% select(botn_id) %>% group_by(botn_id) %>% filter(n()>1)
  if(nrow(botn_id_duplicated)>0) {
    stop(paste("tað eru dublicat í botnsýnunum, botn_id = ", list(unique(botn_id_duplicated$botn_id))))
  }
  
  #check if botn_id results are registered in botnsyni
  botn_id_notregistered <- anti_join(unique(bind_rows(urslitkemi[1], urslitsens[1], synikanningarfolk[1], botndjor[1])), botnsyni[1], djoralivsyni[2], by = "botn_id")
  if(nrow(botn_id_notregistered)>0) {
    stop(paste("tað eru botn_id í úrslitunum, sum ikki eru skrivað inn í botnsýni! botn_id = ", list(unique(botn_id_notregistered$botn_id))))
  }
  
  #kanna fyri duplicate í botnlysing
  urslitbotnlysing_duplicat <- urslitbotnlysing %>% unite("id_botnl", botn_id, botnlysing_id) %>% group_by(id_botnl) %>% filter(n()>1)
  if(nrow(urslitbotnlysing_duplicat)>0) {
    warning(paste("tað eru duplicat botnlýsingunum! botnid_botnlysing = ", list(unique(urslitbotnlysing_duplicat$id_botnl))))
  }
  
  #kanna fyri duplicate í urslitsens
  urslitsens_duplicat <- urslitsens %>% group_by(botn_id) %>% filter(n()>1)
  if(nrow(urslitsens_duplicat)>0) {
    warning(paste("tað eru duplicat í sonsorisku úrslitunum! botnid = ", list(unique(urslitsens_duplicat$botn_id))))
  }
  
  
  #kanna fyri duplicate í kemi
  urslitkemi_duplicat <- urslitkemi %>% unite("id_kemi", botn_id, evni) %>% group_by(id_kemi) %>% filter(n()>1)
  if(nrow(urslitkemi_duplicat)>0) {
    warning(paste("tað eru duplicat í kemiúrslitunum, eru hetta verðulig duplicat ella feilir?"))
  }
  
  #kanna fyri duplicate í sýnikanningarfólk
  synikanningarfolk_duplicat <- synikanningarfolk %>% unite("id_folk", botn_id, kfolk_id) %>% group_by(id_folk) %>% filter(n()>1)
  if(nrow(synikanningarfolk_duplicat)>0) {
    stop(paste("tað eru duplicat í synikanningarfolk! botnid-kfolkid = ", list(unique(synikanningarfolk_duplicat$id_folk))))
  }
  
  #kanna um UPPLAT tvs. hvussu djóralívsýni er upptalt er skrásett
  djoraliv_upptalt <- djoralivsyni %>% filter(is.na(upptalt)) %>% select(djorliv_id, botn_id)
  if(nrow(djoraliv_upptalt)>0) {
    stop(paste("hvussu djóralívssýni er UPPTALT er ikki skrásett, djorliv_id = ", list(unique(djoraliv_upptalt$djorliv_id))))
  }
  
  #kanna fyri duplicate í djóralív
  if(nrow(djoralivsyni)>0) {
    urslitdjoraliv_duplicat <- urslitdjoraliv %>% group_by(djorliv_id, aphia_id) %>% filter(n()>1)
    if(nrow(urslitdjoraliv_duplicat)>0) {
      stop(paste("tað eru duplicat djorasløg! aphia_id = ", list(unique(urslitdjoraliv_duplicat$aphia_id))))
    }
  }
  
  #eftirkanna foreign keys
  
  cat("\n eftirkannar nú foreign keys...")
  cat("\n")
  
  #connect to database
  library(odbc)
  con <- dbConnect(odbc::odbc(),
                   Driver = "PostgreSQL Unicode(x64)", 
                   Server = rstudioapi::showPrompt(title = "Server", message = "Server navn:", default = ""),
                   Database = rstudioapi::showPrompt(title = "Database", message = "Database navn:", default = ""), 
                   UID = rstudioapi::showPrompt("Database Username", "Username"), 
                   PWD = rstudioapi::askForPassword("Database password"),
                   Port = 5432,
                   encoding = "windows-1252")
  
  dbkanningarrapport <- tbl(con, "kanningarrapport") %>% select(rap_id) %>% collect()
  fk_kanningarrapport <- semi_join(kanningarrapport, dbkanningarrapport, by = "rap_id") %>% select(rap_id)
  if(nrow(fk_kanningarrapport)>0) {
    dbDisconnect(con)
    stop(paste("henda kanningarrapportin er longu í datagrunninum, rap_id = ", list(unique(fk_kanningarrapport$rap_id))))
  }
  
  dbkanningarfolk <- tbl(con, "kanningarfolk") %>% select(kfolk_id) %>% collect()
  fk_kanningarfolk <- anti_join(synikanningarfolk, dbkanningarfolk, by = "kfolk_id") %>% select(kfolk_id)
  if(nrow(fk_kanningarfolk)>0) {
    dbDisconnect(con)
    stop(paste("kanningarfólk ikki registrerað í datagrunni, kfolk_id = ", list(unique(fk_kanningarfolk$kfolk_id))))
  }
  
  dbbotnheintari <- tbl(con, "botnheintari") %>% select(botnheint_id) %>% collect()
  fk_botnheint <- anti_join(bind_rows(botnsyni,djoralivsyni), dbbotnheintari, by = "botnheint_id") %>% select(botnheint_id)
  if(nrow(fk_botnheint)>0) {
    dbDisconnect(con)
    stop(paste("botnheintari ikki registreraður í datagrunni, botnheint_id = ", list(unique(fk_botnheint$botnheint_id))))
  }
  
  dbupptalt <- tbl(con, "upptalt") %>% select(upptalt_id, upptalt_navn) %>% collect()
  fk_upptalt <- anti_join(djoralivsyni, dbupptalt, by = c("upptalt"="upptalt_id")) %>% select(upptalt)
  if(nrow(fk_upptalt)>0) {
    dbDisconnect(con)
    stop(paste("upptalt ikki registrerað í datagrunni, upptalt_id = ", list(unique(fk_upptalt$upptalt))))
  }
  
  dbdjorslag <- tbl(con, "djorslag") %>% select(aphia_id) %>% collect()
  fk_aphia <- anti_join(urslitdjoraliv, dbdjorslag, by = "aphia_id")
  if(nrow(fk_aphia)>0) {
    dbDisconnect(con)
    stop(paste("Djóraslag ikki registrerað í datagrunni, aphia_id = ", list(unique(fk_aphia$aphia_id))))
  }
  
  #dagfør usbotn, innlesingar
  DBI::dbWriteTable(con, "kanningarrapport", kanningarrapport, append = TRUE)
  DBI::dbWriteTable(con, "botnsyni", botnsyni, append = TRUE)
  DBI::dbWriteTable(con, "djoralivsyni", djoralivsyni, append = TRUE)
  DBI::dbWriteTable(con, "synikanningarfolk", synikanningarfolk, append = TRUE)
  DBI::dbWriteTable(con, "urslitbotnlysing", urslitbotnlysing, append = TRUE)
  DBI::dbWriteTable(con, "urslitbotnslag", urslitbotnslag, append = TRUE)
  DBI::dbWriteTable(con, "urslitdjoraliv", urslitdjoraliv, append = TRUE)
  DBI::dbWriteTable(con, "urslitdjortald", urslitdjortald, append = TRUE)
  DBI::dbWriteTable(con, "urslitkemi", urslitkemi, append = TRUE)
  DBI::dbWriteTable(con, "urslitsens", urslitsens, append = TRUE)
  
  #dagfør historyrap talvuna
  sql <- "insert into historyrap (rap_id, dato, brukari, del_add) values (?rapport, current_timestamp, current_user, 'append');"
  sqlrapappend <- sqlInterpolate(con, sql, rapport = rapport)
  
  DBI::dbExecute(con, sqlrapappend)
  DBI::dbDisconnect(con)
  
  cat(paste("\n \n", rapport, "er nú INNLISIN á usbotn! Tú ert eitt skrín :) \n"))
  cat("\n")
  
}
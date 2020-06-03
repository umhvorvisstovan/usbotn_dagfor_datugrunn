

#### INNLES !!!


# Hetta skripti verður nýtt til at INNLESA frágreiðingar á usbotn


# minst til at eftirkanna inntøppingina á shiny appini áðrenn tú innlesur!!!

# her skrivar tú navni á fílinum (rapportnavnið) sum skal importerast og aliøki
rapport <- "A12-120926"
alioki <- "A12"

# her skrivar tú hvat fyri excel versión (t.d. v0.0) inntøppingin er í
version_excel <- "v0.0"


# trýst so á ctrl + alt + r (allar í senn) so byrjar koyringin
# - tú verður biðin um at velja xlsm fílin við inntøppaðari frágreiðingini
# - tú verður biðin um at inntøppa títt brúkaranavn og loyniorð til postgres tá koyringin er komin ca. í helvt
# hald eyga við console vindeyganum, har koma feilmeldingar um nakrar eru!









#-----------
# ikki broyt nakað niðanfyri her!!!

library(tidyverse)


filetoimport <- rstudioapi::selectFile(caption = "Select xlsm file",
                               filter = "xlsm (*.xlsm)", path = "I:/VERND/databasi-usbotn/inntastað/",
                               existing = TRUE)
if(Encoding(filetoimport) == "unknown") {filetoimport <- iconv(filetoimport, "UTF-8")}

# innles functións fílirnar
funcfiles <- list.files("functions", pattern = ".R", full.names = TRUE)
walk(funcfiles, source, encoding = "UTF-8")

if(version_excel == "v0.0") {
  innRapp_v0.0(rapport, alioki, filetoimport)
} else if(version_excel == "v1.0") {
  innRapp_v1.0(rapport, alioki, filetoimport)
} else {
  stop("hevur tú skriva rætta excel versións nr. ? (t.d. v1.0)")
}

rm(list = ls())

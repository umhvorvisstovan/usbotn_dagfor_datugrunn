

#### SLETTA !!!


# Hetta skripti verður nýtt til at SLETTA frágreiðingar á usbotn




# her skrivar tú rap_id á tí rapportini, ið skal slettast á usbotn
rapport <- "A12-120926"


# grundgev í stuttum, hví tú slettar rapportina, max 200 tekn
orsok <- "birgitta testar"


# trýst so á ctrl + alt + r (allar í senn) so byrjar koyringin
# - tú verður biðin um at inntøppa títt brúkaranavn og loyniorð til postgres tá koyringin er komin ca. í helvt
# hald eyga við console vindeyganum, har koma feilmeldingar um nakrar eru!










#-----------
# ikki broyt nakað niðanfyri her!!!

library(tidyverse)

# innles functións fílirnar
funcfiles <- list.files("functions", pattern = ".R", full.names = TRUE)
walk(funcfiles, source, encoding = "UTF-8")

slettaRapp(rapport, orsok)

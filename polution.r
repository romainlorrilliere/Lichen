
require(data.table)




filename <- "data/Tab-airparif.csv"

pol <- fread(filename)


pol <- pol[!is.na(NO2),]
setorder(pol,station,année,mois,jour,heure)

seuil <- 20

pol[,sup_seuil := NO2 > seuil]
pol[,inc_heure := 1:.N, by = paste(station,sup_seuil)]
pol[,nb_heure := .N, by = paste(station,sup_seuil)]

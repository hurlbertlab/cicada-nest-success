###############################
#
###############################

library(dplyr)
library(readr)

#download the gbif file from:
#GBIF.org (08 October 2024) GBIF Occurrence Download  https://doi.org/10.15468/dl.mvpbd7

gbif_filepath = "copperheads/data/snakes/gbif-download-inat-squamata.csv"

#read in gbif download of all inat research-grade squamata observations within
#a custom US boundary
squamata <- readr::read_tsv(gbif_filepath,
                quote = "")

#hey, this is excluding all snake observations that haven't been identified to family. Think about if that's a good way to do this - I think we could assume that unidentified snakes are not copperheads, and boost the power of saying how many snakes were observed? But also, these are all the research grade observations, so they MUST have been identified further. Anyway, this why there's a difference between the number of records available on iNat (500k) vs the number downloaded from gbif (300 something k)

#make list of all snake families found on the iNat 'serpentes' page
snake_list <- c(
  "Acrochordidae",
  "Aniliidae",
  "Anomalepididae",
  "Anomochilidae",
  "Atractaspididae",
  "Boidae",
  "Bolyeriidae",
  "Colubridae",
  "Cyclocoridae",
  "Cylindrophiidae",
  "Elapidae",
  "Gerrhopilidae",
  "Homalopsidae",
  "Lamprophiidae",
  "Leptotyphlopidae",
  "Loxocemidae",
  "Micrelapidae",
  "Pareidae",
  "Prosymnidae",
  "Psammophiidae",
  "Pseudaspididae",
  "Pseudoxyrhophiidae",
  "Pythonidae",
  "Tropidophiidae",
  "Typhlopidae",
  "Uropeltidae",
  "Viperidae",
  "Xenodermidae",
  "Xenopeltidae",
  "Xenophidiidae",
  "Xenotyphlopidae"
)

serpentes <- 
  squamata %>%
  dplyr::filter(family %in% snake_list) %>%
  dplyr::ungroup()
 
#table(serpentes$year)
  
copperheads <- serpentes %>%
  dplyr::filter(species == "Agkistrodon contortrix") #11,300 observations :)

#table(copperheads$year)

write.csv(serpentes, "copperheads/data/snakes/inat-serpentes.csv", row.names = FALSE)
zip(zipfile = "copperheads/data/snakes/inat-serpentes.zip", files = "copperheads/data/snakes/inat-serpentes.csv")

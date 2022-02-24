setwd("C:\\Users\\abh00\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_07")
install.packages("iNaturalist")
install.packages("paleodb.org")
install.packages("rinat")
install.packages("spocc")
library(rinat)
library(spocc)
install.packages("rdryad")
library(rdryad)
?get_inat_obs
get_inat_obs(taxon_name = "Viperidae")
get_inat_obs(query = "South American Bushmaster")
?spocc
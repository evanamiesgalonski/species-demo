library(readr)
library(dplyr)
library(sf)
library(mapview)
library(stringr)
library(rmapshaper)
library(ggplot2)
library(tidyverse)
library(magrittr)

#### species data ####
# clean species data
species_bc <- readr::read_tsv("/Users/evanamies-galonski/Documents/gin thesis/bcsee_export.tsv")
names(species_bc) <- gsub(" ", "", names(species_bc))

# conservation status (red/blue list) function
species_bc %<>% mutate(COSEWIC = str_replace(COSEWIC, "\\(", ""),
                     COSEWIC = str_replace(COSEWIC, "\\)", "")) %>%
  separate(COSEWIC, c("COSEWIC", "Implemented Date"),
                       sep =" ", extra = "merge") %>%
  mutate(`Implemented Date` = lubridate::parse_date_time(`Implemented Date`, "m y"))

# cosewic status abbrevations changed
species_bc <- mutate(species_bc,
                     `COSEWIC Status` = case_when(COSEWIC == "E" ~ "Endangered",
                                                  COSEWIC == "XT" ~ "Extirpated",
                                                  COSEWIC == "T" ~ "Threatened",
                                                  COSEWIC == "X" ~ "Extinct",
                                                  COSEWIC == "SC" ~ "Special Concern",
                                                  COSEWIC == "NAR" ~ "Not At Risk",
                                                  COSEWIC == "DD" ~ "Data Deficient",
                                                  COSEWIC == "E/T" ~ "Endangered/Threatened",
                                                  TRUE ~ "No Status"))

# Dropping columns
species_bc %<>% select(-c(ScientificNameSynonyms, EnglishNameSynonyms, GlobalStatusReviewDate, ProvStatusChangeDate,
                          ProvStatusReviewDate, COSEWICComments, ProvincialFRPA, GOERT, MBCA, SARAComments,
                          BreedingBird, MappingStatus, X46, CDCMaps, COSEWIC))


#### ecosections ####
ecosections <- select(species_bc, ScientificName, Ecosection)

# this gets ecosections for each ScientificName
ecosections <- do.call("rbind", lapply(1:nrow(ecosections), function(x){
  data <- ecosections[x,]
  splits <- strsplit(data$Ecosection, ";")[[1]]
  data <- data.frame(ScientificName = rep(data$ScientificName, length(splits)),
                     Ecosection = splits, stringsAsFactors = FALSE)
}))

# Loading maps used in analysis
ecosection_map <- sf::st_read("~/Documents/gin thesis/ERC_ECOSEC_polygon.shp")

# ecosection_map2 <- bcmaps::ecosections()
bc_boundary <- bcmaps::bc_bound()

ecosection_simple <- ms_simplify(ecosection_map)

usethis::use_data(species_bc, overwrite = TRUE)
usethis::use_data(ecosection_simple, overwrite = TRUE, internal = TRUE)
usethis::use_data(bc_boundary, overwrite = TRUE, internal = TRUE)
usethis::use_data(ecosections, overwrite = TRUE, internal = TRUE)


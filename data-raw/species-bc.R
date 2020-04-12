library(readr)
library(dplyr)
library(sf)
library(mapview)
library(stringr)
library(rmapshaper)
library(ggplot2)
library(tidyverse)

# clean species data
species_bc <- readr::read_tsv("/Users/evanamies-galonski/Documents/gin thesis/bcsee_export.tsv")

names(species_bc) <- gsub(" ", "", names(species_bc))

### Dropping columns
species_bc$ScientificNameSynonyms <- NULL
species_bc$EnglishNameSynonyms <- NULL
species_bc$GlobalStatusReviewDate <- NULL
species_bc$ProvStatusChangeDate <- NULL
species_bc$ProvStatusReviewDate <- NULL
species_bc$COSEWICComments <- NULL
species_bc$ProvincialFRPA <- NULL
species_bc$GOERT <- NULL
species_bc$MBCA <- NULL
species_bc$SARAComments <- NULL
species_bc$BreedingBird <- NULL
species_bc$MappingStatus <- NULL
species_bc$X46 <- NULL
species_bc$CDCMaps <- NULL

### conservation status (red/blue list) function
species_bc <- mutate(species_bc,
                     COSEWIC = str_replace(COSEWIC, "\\(", ""),
                     COSEWIC = str_replace(COSEWIC, "\\)", ""))
species_bc <- separate(species_bc, COSEWIC, c("COSEWIC", "Implemented Date"),
                       sep =" ", extra = "merge")
species_bc <- mutate(species_bc,
                     `Implemented Date` = lubridate::parse_date_time(`Implemented Date`, "m y"))

### solution 1
# make new COSEWIC Status column from old (now called COSEWIC) so it doesnt get overwritten every time you run
# then delete the old one

x <- species_bc$`COSEWIC`
x <- gsub("E", "Endangered", x)
x <- gsub("XT", "Extirpated", x)
x <- gsub("T", "Threatened", x)
x <- gsub("X", "Extinct", x)
x <- gsub("SC", "Special Concern", x)
x <- gsub("NAR", "Not at Risk", x)
x <- gsub("DD", "Data Deficient", x)
x[is.na(x)] <- "No Status"

species_bc$`COSEWIC Status` <- x

# check that Cosewic statuses make sense
table(species_bc$COSEWIC)
table(species_bc$`COSEWIC Status`)

#### solution 2 - use mutate and case_when
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

table(species_bc$COSEWIC)
table(species_bc$`COSEWIC Status`)

### delete old cosewic column
species_bc$COSEWIC <- NULL

usethis::use_data(species_bc, overwrite = TRUE)

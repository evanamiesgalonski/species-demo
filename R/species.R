#' conservation_status
#'
#' @param species the species to provide
#' @return
#' @export
#'
#' @examples
#' conservation_status("Anemone occidentalis - Carex nigricans")
#'
conservation_status <- function(species) {

  chk::chk_character(species)

  if(!species %in% speciesdemo::bc_species$ScientificName){
    err::err("Invalid scientific name, see speciesdemo::bcspecies for reference")
  }

  species_conservation <- speciesdemo::bc_species[speciesdemo::bc_species$ScientificName == species,]
  species_conservation_columns <- species_conservation[c("BCList", "COSEWIC Status", "Implemented Date")]
  return(print(species_conservation_columns))
}

#' species_map
#'
#' @param species the species of interest
#' @return a map indicating all ecosections where the species is extant
#' @export
#'
#' @examples
#'
species_map <- function(species) {

  chk::chk_character(species)
  if(!species %in% speciesdemo::bc_species$ScientificName){
    err::err("Invalid scientific name, see speciesdemo::bcspecies for reference")
  }

  ecosections <- speciesdemo:::ecosections
  ecosection_simple <- speciesdemo:::ecosection_simple
  bc_boundary <- speciesdemo:::bc_boundary

  species_ecosections <- ecosections$Ecosection[ecosections$ScientificName == species]
  species_ecosections_spatial <- ecosection_simple[ecosection_simple$ECOSEC_CD %in% species_ecosections,]
  species_present <- ecosection_simple
  species_present$Present <- dplyr::if_else(species_present$ECOSEC_CD %in% species_ecosections, TRUE, FALSE)

  gp <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = bc_boundary, size = 0.2) +
    ggplot2::geom_sf(data = species_present, ggplot2::aes(fill = Present), size = 0.05) +
    ggplot2::scale_fill_manual(values = c("transparent", "red"),
                      name = " ",
                      labels = c("Species Absent", "Species Present")) +
    ggplot2::labs(title = "Species Distribution by Ecosection",
         subtitle = species)
  return(print(gp))
}

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

  species_conservation <- speciesdemo::bc_species[speciesdemo::bc_species$ScientificName == species,]
  species_conservation_columns <- species_conservation[c("BCList", "COSEWIC Status", "Implemented Date")]
  return(print(species_conservation_columns))
}

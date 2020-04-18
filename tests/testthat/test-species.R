test_that("conservation_status works", {

status <- conservation_status("Anemone occidentalis - Carex nigricans")

expect_identical(status$BCList, "Red")
expect_identical(status$`COSEWIC Status`, "No Status")

expect_true(inherits(status, c("tbl_df", "tbl", "data.frame")))
expect_error(conservation_status("DF"), "Invalid scientific name, see speciesdemo::bcspecies for reference")
})

test_that("ecosrctiodns map works", {

  map <- species_map("Anemone occidentalis - Carex nigricans")

  expect_true(inherits(map, c("gg", "ggplot")))
  expect_identical(map$labels$subtitle, "Anemone occidentalis - Carex nigricans")
  expect_identical(names(map$plot_env), c("gp", "species_present", "species_ecosections_spatial",
                                          "species_ecosections", "bc_boundary", "ecosection_simple",
                                          "ecosections", "species"))
})

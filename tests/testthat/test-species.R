test_that("conservation_status works", {

status <- conservation_status("Anemone occidentalis - Carex nigricans")

expect_identical(status$BCList, "Red")
expect_identical(status$`COSEWIC Status`, "No Status")

expect_true(inherits(status, c("tbl_df", "tbl", "data.frame")))


})

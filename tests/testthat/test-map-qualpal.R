test_that("map_qualpal preserves observed order for character input", {
  x <- c("B", "A", "C", "A")

  pal <- map_qualpal(x)

  expect_equal(names(pal), c("B", "A", "C"))
  expect_length(pal, 3)
})

test_that("map_qualpal uses factor levels and respects drop", {
  x <- factor(c("B", "A", "B"), levels = c("A", "B", "C"))

  pal_all <- map_qualpal(x)
  pal_drop <- map_qualpal(x, drop = TRUE)

  expect_equal(names(pal_all), c("A", "B", "C"))
  expect_equal(names(pal_drop), c("A", "B"))
})

test_that("map_qualpal uses explicit levels for empty input", {
  pal <- map_qualpal(character(0), levels = c("A", "B"))

  expect_equal(names(pal), c("A", "B"))
  expect_length(pal, 2)
})

test_that("map_qualpal returns empty output for empty input without levels", {
  expect_equal(map_qualpal(character(0)), character(0))
})

test_that("map_qualpal validates that explicit levels include observed values", {
  expect_error(
    map_qualpal(c("A", "B"), levels = "A"),
    "must include all categories present"
  )
})

test_that("map_qualpal applies fixed and override values by name", {
  pal <- map_qualpal(
    c("A", "B", "C"),
    fixed = c(B = "#000000"),
    override = c(C = "#ffffff")
  )

  expect_equal(unname(pal["B"]), "#000000")
  expect_equal(unname(pal["C"]), "#ffffff")
  expect_equal(names(pal), c("A", "B", "C"))
})

test_that("map_qualpal includes NA colour only when needed", {
  pal_with_na <- map_qualpal(c("A", NA))
  pal_without_na <- map_qualpal("A", na_color = NULL)

  expect_equal(unname(pal_with_na["NA"]), "#BEBEBE")
  expect_equal(names(pal_with_na), c("A", "NA"))
  expect_match(unname(pal_with_na["A"]), "^#")
  expect_equal(names(pal_without_na), "A")
  expect_false("NA" %in% names(pal_without_na))
})

test_that("map_qualpal rejects unknown names in fixed and override", {
  expect_error(
    map_qualpal(c("A", "B"), fixed = c(C = "#000000")),
    "must be a subset of the final level set"
  )

  expect_error(
    map_qualpal(c("A", "B"), override = c(C = "#000000")),
    "must be a subset of the final level set"
  )
})

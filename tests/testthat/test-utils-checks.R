test_that("input checks raise classed errors", {
  expect_error(check_numeric("a"), class = "rdsc_error_input")
  expect_error(check_numeric(c(1, NA)), class = "rdsc_error_input")
  expect_error(check_numeric(c(1, Inf)), class = "rdsc_error_input")
  expect_error(check_numeric(numeric()), class = "rdsc_error_input")
  expect_error(check_scalar_number(c(1, 2)), class = "rdsc_error_input")
  expect_error(check_scalar_number(0, lower = 0, lower_open = TRUE),
               class = "rdsc_error_input")
  expect_error(check_count(2.5), class = "rdsc_error_input")
  expect_error(check_level(1), class = "rdsc_error_input")
  expect_error(check_binary(factor(1:3)), class = "rdsc_error_input")
  expect_error(check_seed("a"), class = "rdsc_error_input")
  expect_identical(check_binary(c(TRUE, FALSE)), c(1L, 0L))
  expect_identical(check_binary(factor(c("a", "b"))), c(0L, 1L))
})

test_that("rbind_fill aligns columns", {
  out <- rbind_fill(list(data.frame(a = 1, b = "x"), data.frame(a = 2)))
  expect_identical(out$b, c("x", NA))
  expect_identical(out$a, c(1, 2))
})

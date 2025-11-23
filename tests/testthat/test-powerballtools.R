library(testthat)
library(powerballtools)

test_that("pb_most_common_numbers returns pb_freq with correct structure", {
  data("pb_draws")

  freq <- pb_most_common_numbers(type = "white", top_n = 5)

  expect_s3_class(freq, "pb_freq")
  expect_equal(ncol(freq), 2)
  expect_equal(names(freq), c("number", "count"))
  expect_equal(nrow(freq), 5)
})

test_that("pb_random_ticket creates a valid ticket", {
  ticket <- pb_random_ticket(weighted = FALSE)

  expect_length(ticket$white, 5)
  expect_true(all(ticket$white >= 1 & ticket$white <= 69))
  expect_length(ticket$powerball, 1)
  expect_true(ticket$powerball >= 1 && ticket$powerball <= 26)
})

test_that("pb_check_ticket identifies full matches", {
  data("pb_draws")

  first <- pb_draws[1, ]
  ticket_white <- c(first$n1, first$n2, first$n3, first$n4, first$n5)
  ticket_power <- first$powerball

  res <- pb_check_ticket(ticket_white, ticket_power)

  expect_equal(res$white_matches[1], 5)
  expect_true(res$powerball_match[1])
})

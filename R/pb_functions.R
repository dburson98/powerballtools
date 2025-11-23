#' Most common Powerball numbers
#'
#' Summarise how often each number has been drawn.
#'
#' @param draws Data frame like \code{pb_draws}.
#' @param type Either \code{"white"} for the five white balls
#'   or \code{"powerball"} for the red ball.
#' @param top_n How many numbers to return.
#'
#' @return A data frame with columns \code{number} and \code{count}
#'   and class \code{"pb_freq"}.
#' @export
pb_most_common_numbers <- function(draws = pb_draws,
                                   type = c("white", "powerball"),
                                   top_n = 10) {
  type <- match.arg(type)

  needed <- c("n1", "n2", "n3", "n4", "n5", "powerball")
  if (!all(needed %in% names(draws))) {
    stop("draws must have columns n1, n2, n3, n4, n5, powerball")
  }

  if (!is.numeric(top_n) || length(top_n) != 1 || top_n <= 0) {
    stop("top_n must be a positive number")
  }

  if (type == "white") {
    nums <- c(draws$n1, draws$n2, draws$n3, draws$n4, draws$n5)
  } else {
    nums <- draws$powerball
  }

  tab <- sort(table(nums), decreasing = TRUE)
  res <- head(
    data.frame(
      number = as.integer(names(tab)),
      count = as.integer(tab)
    ),
    top_n
  )

  class(res) <- c("pb_freq", class(res))
  attr(res, "type") <- type
  res
}

#' @export
print.pb_freq <- function(x, ...) {
  type <- attr(x, "type", exact = TRUE)
  cat("Most common", type, "numbers:\n")
  NextMethod()
}

#' @export
plot.pb_freq <- function(x, ...) {
  type <- attr(x, "type", exact = TRUE)
  ggplot2::ggplot(x, ggplot2::aes(x = factor(number), y = count)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = "Number",
      y = "Times drawn",
      title = paste("Most common", type, "numbers")
    )
}

#' Generate a random Powerball ticket
#'
#' @param weighted If TRUE, use historical frequencies from \code{pb_draws}
#'   as sampling weights. If FALSE, sample numbers uniformly.
#' @param draws Data frame like \code{pb_draws}.
#'
#' @return A list with elements \code{white} (five numbers) and
#'   \code{powerball} (one number).
#' @export
pb_random_ticket <- function(weighted = FALSE, draws = pb_draws) {
  if (!is.logical(weighted) || length(weighted) != 1) {
    stop("weighted must be TRUE or FALSE")
  }

  white_nums <- 1:69
  power_nums <- 1:26

  if (weighted) {
    white_freq <- pb_most_common_numbers(draws, type = "white", top_n = 69)
    power_freq <- pb_most_common_numbers(draws, type = "powerball", top_n = 26)

    w_prob <- white_freq$count / sum(white_freq$count)
    p_prob <- power_freq$count / sum(power_freq$count)

    white <- sort(
      sample(white_freq$number, size = 5, replace = FALSE, prob = w_prob)
    )
    power <- sample(power_freq$number, size = 1, prob = p_prob)
  } else {
    white <- sort(sample(white_nums, size = 5, replace = FALSE))
    power <- sample(power_nums, size = 1)
  }

  list(white = white, powerball = power)
}

#' Check a ticket against all historical draws
#'
#' @param ticket_white Integer vector of length 5 for the white balls.
#' @param ticket_power Single integer for the Powerball.
#' @param draws Data frame like \code{pb_draws}.
#'
#' @return Data frame with draw date, white matches, and whether
#'   the Powerball matched.
#' @export
pb_check_ticket <- function(ticket_white, ticket_power, draws = pb_draws) {
  if (length(ticket_white) != 5) {
    stop("ticket_white must have length 5")
  }
  if (!is.numeric(ticket_white) || !is.numeric(ticket_power)) {
    stop("ticket numbers must be numeric")
  }

  white_mat <- draws[, c("n1", "n2", "n3", "n4", "n5")]
  white_matches <- apply(
    white_mat,
    1,
    function(row) sum(row %in% ticket_white)
  )

  power_hit <- draws$powerball == ticket_power

  data.frame(
    draw_date = draws$draw_date,
    white_matches = white_matches,
    powerball_match = power_hit
  )
}

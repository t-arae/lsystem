
#' L-system
#' @param w character
#' @param p pattern
#' @param r replacement
#' @param n number of loop
#' @export
#' @examples
#' ### Model of the growth of algae
#' 0:6 |> lapply(\(x) lsystem("A", c("A", "B"), c("AB", "A"), x)) |> unlist()
#'
lsystem <- function(w, p, r, n) {
  p <- c(p, ".")
  for(i in seq_len(n)) {
    w_len <- nchar(w)
    new_w <- ""
    while(w_len > 0) {
      for(j in seq_along(p)) {
        if(stringr::str_starts(w, p[j])) {
          temp <- ifelse(p[j] != ".", r[j], stringr::str_sub(w, 1, 1))
          w <- stringr::str_sub(w, start = nchar(p[j]) + 1)
          new_w <- c(new_w, temp)
          w_len <- w_len - nchar(p[j])
          break
        }
      }
    }
    w <- paste0(new_w, collapse = "")
  }
  w
}

#' L-system version 2
#' @param w character
#' @param pr replacement rule
#' @param n number of loop
#' @export
#' @examples
#' ### Model of the growth of algae
#' 0:6 |> lapply(\(x) lsystem2("A", c("A" = "AB", "B" = "A"), x)) |> unlist()
#'
#' ### Fibonacci numbers
#' 0:6 |> lapply(\(x) lsystem2("A", c("A" = "B", "B" = "AB"), x)) |> unlist()
#'
#' ### Cantor set
#' 0:4 |> lapply(\(x) lsystem2("A", c("A" = "ABA", "B" = "BBB"), x)) |> unlist()
#'
#' ### Koch curve
#' 0:4 |> lapply(\(x) lsystem2("F", c("F" = "F+F-F-F+F"), x)) |> unlist()
#'
lsystem2 <- function(w, pr, n) {
  for(i in seq_len(n)) {
    ws <- stringr::str_extract_all(w, ".")[[1]]
    new_w <- dplyr::if_else(is.na(pr[ws]), ws, pr[ws])
    w <- paste0(new_w, collapse = "")
  }
  w
}

# bench::mark(
#   lsystem("A", c("A", "B"), c("AB", "A"), 10),
#   lsystem2("A", c("A" = "AB", "B" = "A"), 10)
# )

angle <- ws <- x <- y <- NULL

#' Convert L-system symbol to data which able to plot by ggplot2
#' @param w L-system symbol
#' @export
#' @examples
#' lsystem2("F", c("F" = "F+F-F-F+F"), 4) |> w2curve()
#'
w2curve <- function(w) {
  angle <- 0
  x <- 0
  y <- 0
  ws <- stringr::str_extract_all(w, ".")[[1]]
  for(i in ws) {
    if(i == "F") {
      x <- c(x, cos(angle) + x[length(x)])
      y <- c(y, sin(angle) + y[length(y)])
    } else if (i == "+") {
      angle <- angle + pi/2
    } else if (i == "-") {
      angle <- angle - pi/2
    }
  }
  tibble::tibble(x, y)
}

#' w2curve() version 2
#' @param w L-system symbol
#' @export
#' @examples
#' lsystem2("F", c("F" = "F+F-F-F+F"), 4) |> w2curve2()
#'
w2curve2 <- function(w) {
  tbl <-
    tibble::tibble(ws = stringr::str_extract_all(w, ".")[[1]]) |>
    dplyr::mutate(angle = dplyr::case_when(
      ws == "+" ~ pi/2,
      ws == "-" ~ -pi/2,
      TRUE ~ 0
    )) |>
    {\(.) dplyr::bind_rows(tibble::tibble(ws = "", angle = 0), .)}() |>
    dplyr::mutate(angle = cumsum(angle)) |>
    dplyr::filter(ws == "F") |>
    dplyr::mutate(x = cos(angle), y = sin(angle)) |>
    {\(.) dplyr::bind_rows(tibble::tibble(ws = "", angle = 0, x = 0, y = 0), .)}() |>
    dplyr::mutate(x = cumsum(x), y = cumsum(y))
  tbl
}

#' w2curve() version 3 (with simple angle rule)
#' @param w L-system symbol
#' @param theta angle theta
#' @export
#' @examples
#' lsystem2("F", c("F" = "F+F-F-F+F"), 4) |> w2curve3()
#' lsystem2("F", c("F" = "F+F-F-F+F"), 4) |> w2curve3(theta = pi/3)
#'
w2curve3 <- function(w, theta = pi/2) {
  tbl <-
    tibble::tibble(ws = stringr::str_extract_all(w, ".")[[1]]) |>
    dplyr::mutate(angle = dplyr::case_when(
      ws == "+" ~ theta,
      ws == "-" ~ -theta,
      TRUE ~ 0
    )) |>
    {\(.) dplyr::bind_rows(tibble::tibble(ws = "", angle = 0), .)}() |>
    dplyr::mutate(angle = cumsum(angle)) |>
    dplyr::filter(ws == "F") |>
    dplyr::mutate(x = cos(angle), y = sin(angle)) |>
    {\(.) dplyr::bind_rows(tibble::tibble(ws = "", angle = 0, x = 0, y = 0), .)}() |>
    dplyr::mutate(x = cumsum(x), y = cumsum(y))
  tbl
}

#' w2curve() version 4 (with angle rule)
#' @param w L-system symbol
#' @param char_segment specify segment character
#' @param angle_rule angle rule
#' @export
#' @examples
#' lsystem2("F", c("F" = "F+F-F-F+F"), 4) |> w2curve4()
#' lsystem2("F", c("F" = "F+F-F-F+F"), 4) |> w2curve4(angle_rule = c("+" = pi/3, "-" = pi/2))
#'
w2curve4 <- function(
  w, char_segment = "F", angle_rule = c("+" = pi/2, "-" = -pi/2)
) {
  tbl <-
    tibble::tibble(ws = stringr::str_extract_all(w, ".")[[1]]) |>
    dplyr::mutate(
      angle = dplyr::if_else(ws %in% names(angle_rule), angle_rule[ws], 0)
    ) |>
    {\(.) dplyr::bind_rows(tibble::tibble(ws = "", angle = 0), .)}() |>
    dplyr::mutate(angle = cumsum(angle)) |>
    dplyr::filter(ws %in% char_segment) |>
    dplyr::mutate(x = cos(angle), y = sin(angle)) |>
    {\(.) dplyr::bind_rows(tibble::tibble(ws = "", angle = 0, x = 0, y = 0), .)}() |>
    dplyr::mutate(x = cumsum(x), y = cumsum(y))
  tbl
}

#' Plot tibble curve data
#' @param tbl_curve tibble output from w2curve()
#' @export
#' @examples
#' lsystem2("F", c("F" = "F+F-F-F+F"), 4) |>
#'   w2curve2() |>
#'   plot_curve()
#'
plot_curve <- function(tbl_curve) {
  tbl_curve |>
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggforce::stat_link2(n = 1) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}

# lsystem2("F", c("F" = "F+F-F-F+F"), 4) |>
#   w2curve2() |>
#   plot_curve()


# lsystem2("FX", c("X" = "X+YF+", "Y" = "-FX-Y"), 8) |>
#   w2curve2() |>
#   plot_curve()

# tbl_plot <-
#   16:0 %>%
#   purrr::map(~ lsystem2("FX", c("X" = "X+YF+", "Y" = "-FX-Y"), .x)) %>%
#   purrr::map(w2curve2) %>%
#   purrr::map2(as.character(16:0), ~ dplyr::mutate(.x, num = .y)) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(num = forcats::fct_inorder(num))
#
# gp <-
#   tbl_plot %>%
#   ggplot(aes(x, y)) +
#   ggforce::stat_link2(size = .8, n = 1, aes(color = num)) +
#   coord_equal() +
#   scale_color_grey() +
#   theme_void() +
#   theme(legend.position = "none")
# ggsave(filename = "~/Desktop/dragon16.png", plot = gp,
#        width = 10, height = 10, dpi = 600)



# bench::mark(
#   lsystem2("FX", c("X" = "X+YF+", "Y" = "-FX-Y"), 5) |> w2curve(),
#   lsystem2("FX", c("X" = "X+YF+", "Y" = "-FX-Y"), 5) |> w2curve2(),
#   lsystem2("FX", c("X" = "X+YF+", "Y" = "-FX-Y"), 5) |> w2curve3(),
#   lsystem2("FX", c("X" = "X+YF+", "Y" = "-FX-Y"), 5) |> w2curve4(),
#   check = FALSE
# )

# # コッホ曲線
# lsystem2(w = "F", pr = c("F" = "F+F-F-F+F"), n = 6) |>
#   w2curve4(char_segment = "F", angle_rule = c("+" = pi/2, "-" = -pi/2)) |>
#   plot_curve()
#
# lsystem2(w = "F", pr = c("F" = "F+F-F-F+F"), n = 3) |>
#   w2curve4(char_segment = "F", angle_rule = c("+" = pi/3, "-" = -pi/3)) |>
#   plot_curve()
#
# lsystem2(w = "F", pr = c("F" = "F+F-F-F+F"), n = 5) |>
#   w2curve4(char_segment = "F", angle_rule = c("+" = pi/1.5, "-" = -pi/1.5)) |>
#   plot_curve()
#
# lsystem2(w = "F", pr = c("F" = "F+F--F+F"), n = 3) |>
#   w2curve4(char_segment = "F", angle_rule = c("+" = pi/3, "-" = -pi/3)) |>
#   plot_curve()
#
# lsystem2(w = "F--F--F--", pr = c("F" = "F+F--F+F"), n = 3) |>
#   w2curve4(char_segment = "F", angle_rule = c("+" = pi/3, "-" = -pi/3)) |>
#   plot_curve()

# # ゴスペル曲線
# lsystem2(w = "A", pr = c("A" = "A-B--B+A++AA+B-",
#                          "B" = "+A-BB--B-A++A+B"), n = 3) |>
#   w2curve4(char_segment = c("A", "B"), angle_rule = c("+" = pi/3, "-" = -pi/3)) |>
#   plot_curve()

# # シェルピンスキーの三角形
# lsystem2(w = "A", pr = c("A" = "B-A-B", "B" = "A+B+A"), n = 7) |>
#   w2curve4(char_segment = c("A", "B"), angle_rule = c("+" = -pi/3, "-" = pi/3)) |>
#   plot_curve()

# N <- 10
# tbl_plot <-
#   N:0 %>%
#   purrr::map(~ lsystem2("A", c("A" = "B-A-B", "B" = "A+B+A"), .x)) %>%
#   purrr::map(~ stringr::str_replace_all(.x, "[AB]", "F")) %>%
#   purrr::map(w2curve4, angle_rule = c("+" = -pi/3, "-" = pi/3)) %>%
#   purrr::map2(as.character(N:0), ~ dplyr::mutate(.x, num = .y)) %>%
#   dplyr::bind_rows() %>%
#   dplyr::mutate(num = forcats::fct_inorder(num))
#
# tbl_plot %>%
#   ggplot(aes(x, y)) +
#   ggforce::stat_link2(size = .8, n = 1, aes(color = num)) +
#   coord_equal() +
#   scale_color_viridis_d() +
#   theme_void() +
#   theme(legend.position = "none")

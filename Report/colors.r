library(colorhull)
library(colorscience)
library(dplyr)
library(magick)
library(magrittr)
library(purrr)
library(reticulate)


get_theme_colors <- function(year) {
  path <- paste0("ikea/", year, "/")
  images <- list.files(path, pattern = "*.jpg")
  map(
    images,
    function(x) {
      print(x)
      colorhull::get_theme_colors_from_image(paste0(path, x))
    }
  )
}

# Note: takes a really, really long time to run!
get_themes_for_all_catalogs <- function() {
  for (year in seq(1950, 2021)) {
    print(paste("--------------", year, "------------"))
    rgb_list <- get_theme_colors(year)
    save(rgb_list, file = paste0("data/colors", year, ".RData"))
  }
}

get_saturation_from_image <- function(filepath) {
  im <- image_read(filepath) %>%
    image_resize("10%", filter = "Point") %>%
    image_data("rgb") %>%
    as.numeric()
  d <- dim(im)
  colors_rgb <- reticulate::array_reshape(im, c(d[[1]] * d[[2]], 3))
  colors_hsv <- colorscience::RGB2HSV(colors_rgb)
  colors_ycbcr <- colorscience::RGB2YCbCr(colors_rgb)
  stats <- list(
    n_pixels = d[[1]] * d[[2]],
    rgb_mean = colMeans(colors_rgb),
    saturation_mean = mean(colors_hsv[, 2]),
    saturation_median = median(colors_hsv[, 2]),
    luminance_mean = mean(colors_ycbcr[, 1]),
    luminance_median = median(colors_ycbcr[, 1])
  )
  stats
}

get_saturation <- function(year) {
  path <- paste0("ikea/", year, "/")
  images <- list.files(path, pattern = "*.jpg")
  map(
    images,
    function(x) {
      get_saturation_from_image(paste0(path, x))
    }
  )
}

# Note: takes a long time to run!
get_stats_for_all_catalogs <- function() {
  for (year in seq(1950, 2021)) {
    print(paste("--------------", year, "------------"))
    stats_list <- get_saturation(year)
    save(stats_list, file = paste0("data/stats", year, ".RData"))
  }
}

library(readr)
library(stringr)
library(tidyverse)


download_images <- function(year) {
  domain <- "https://ikeacatalogues.ikea.com"
  path <- paste0("ikea/", year)
  if (!dir.exists(path)) {
    dir.create(path)
  }

  url <- paste0(domain, "/sv-", year, "/page/1")
  text <- readr::read_file(url)
  m <- stringr::str_match_all(text, '"at2400":"([^"]+)"')

  image_paths <- m[[1]][, 2]
  image_urls <- paste0(domain, image_paths)
  extensions <- stringr::str_match(image_paths, "\\.([^\\.]+)$")[, 2]
  page_names <- sprintf("page%03d", seq(length(image_paths)))
  file_names <- paste0(path, "/", page_names, ".", extensions)

  for (i in seq_along(image_urls)) {
    download.file(
      image_urls[[i]],
      file_names[[i]],
      quiet = TRUE,
      method = "wget",
      extra = "--wait=1 --random-wait"
    )
    Sys.sleep(0.25)
  }
}

download_all_catalogs <- function() {
  for (year in seq(1950, 2021)) {
      download_images(year)
  }
}

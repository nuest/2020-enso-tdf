#' @title Download data from Zenodo
#' @description
#' `download_zenodo()`: Download tables.gpkg from Zenodo.
#' @template url
#' @name download
download_zenodo = function(
  url = "https://zenodo.org/record/3981437/files/tables.gpkg") {
  if (file.exists("data/tables.gpkg")) {
    cat("tables.gpkg already exists in the data/ folder") 
  } else {
    download.file(url,
                  destfile = here::here("data/tables.gpkg"))
  }
}
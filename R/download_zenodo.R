#' @title Download data from Zenodo
#' @description
#' `download_zenodo()`: Download tables.gpkg from Zenodo.
#' @importFrom curl curl_download
#' @importFrom sf st_read
#' @importFrom glue glue
#'
#' @template url
#' @name download
download_zenodo = function(url) {
  if (!file.exists("data/tables.gpkg")) {
    curl_download(url,
                  destfile = "data/tables.gpkg", 
                  quiet = FALSE)
  }
}
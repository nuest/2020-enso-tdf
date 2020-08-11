# define own error functions
err_nrmse = function(obs, pred, DIFF = diff(range(obs))) {
  err = list(
    bias = mean(obs - pred), 
    stddev = sd(obs - pred), 
    mse = mean((obs - pred)^2), 
    rmse = sqrt(mean((obs - pred)^2)),
    mad = mad(obs - pred), 
    median = median(obs - pred), 
    iqr = IQR(obs - pred))
  err$nrmse = sqrt(err$mse) / DIFF * 100
  err$count = length(obs)
  return(err)
}

# Adjust partition_means: select a location and keep all years
# of this location in the test/training dataset
#' @importFrom dplyr group_by_at select pull mutate
#' @importFrom sperrorest as.represampling 
partition_kmeans_factor = 
  function(data, coords = c("x", "y"), nfold = 5, repetition = 1, fac, 
           seed1 = NULL, return_factor = FALSE, balancing_steps = 1, 
           order_clusters = TRUE, ...) {
    if (length(repetition) < 2) {
      repetition = seq(1, repetition, by = 1)
    }
    balancing_steps = max(1, balancing_steps)
    resampling = list()
    for (cnt in repetition) {
      if (!is.null(seed1)) {
        set.seed(seed1 + cnt)
      }
      kms = list()
      # just keep unique coordinates
      fil = data[!duplicated(data[, fac]), ]
      for (i in 1:balancing_steps) {
        kms[[i]] = kmeans(fil[, coords], centers = nfold, ...)
      }
      kmgini = function(x) {
        p = x$size / sum(x$size)
        return(1 - sum(p^2))
      }
      km = kms[[which.max(sapply(kms, kmgini))]]
      if (order_clusters) {
        o = rank(km$centers[, 1], ties.method = "first")
        km$cluster = o[km$cluster]
      }
      data[!duplicated(data[, fac]), "cluster"] = km$cluster
      tile = dplyr::group_by_at(data, fac) %>%
        dplyr::mutate(cluster = cluster[1]) %>%
        dplyr::pull(cluster)
      tile = factor(tile)
      if (!return_factor) {
        tile = sperrorest::as.resampling(tile)
      }
      resampling[[as.character(cnt)]] = tile
    }
    if (!return_factor) {
      resampling = sperrorest::as.represampling(resampling)
    }
    return(resampling)
  }

#' @title Apply a model to response variables for a spatial prediction
#' @param data A dataframe containing the predictor and response variables.
#' @param model A model output, e.g., a mgcv::gam output, to apply to the data.
#' @param year The year given as a factor for the prediction should be done.
#' @param pred_raster The raster containing the predictor raster of the year
#'   for which the spatial prediction should be done.
#' @importFrom raster values stack extend predict
pred_fun = function(data, model, year, ndvi_raster) {
  data = data[data$year == year, ]
  # just keep positive NDVI values
  r = ndvi_raster[ndvi_raster >= 0, drop = FALSE]
  y = r
  # make sure to use the year as a factor variable with 4 levels (2011, 2012,
  # 2016, 2017), if you just type factor(2011) or factor(2012) the predictions
  # will be all made for the first level, i.e., the first year (2011) which is
  # surely not what we want
  raster::values(y) = data$year[1]
  s = raster::stack(r, y)
  names(s) = c(gsub("_\\d\\d$", "", names(ndvi_raster)), "year")
  # return the prediction
  pred = raster::predict(s, model, type = "response")
  # use the same extent as the input data, this way we can stack the resulting
  # prediction rasters
  raster::extend(pred, ndvi_raster)
  }

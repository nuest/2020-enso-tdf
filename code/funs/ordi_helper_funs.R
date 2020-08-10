#' @title Function to apply several ordination methods
#' @details This function lets you use DCA, NMDS and Isomap while also returning
#'   the variance explained by the ordination axes.
#' @param data Plot-species matrix with an id column named \code{pnr} which will
#'   be used to rename the rownames, and then will be removed prior to executing
#'   the ordination.
#' @param env_data Dataframe containing environmental variables. The row order
#'   should correspond to the row order of the \code{data} dataframe.
#' @param method Ordination method to apply. Supported arguments are:
#'   \code{"dca"}, \code{"nmds"} and \code{"isomap"}.
#' @return Output of the ordination will be returned.
#' @author Jannes Muenchow

ord_fun = function(data, env_data, method) {
  row.names(data) = data$pnr
  data$pnr = NULL
  if (method == "dca") {
    ord = decorana(data, iweigh = TRUE)
    cat("single axis contribution: ", 
        round(ord$evals / sum(ord$evals), 2), "\n")
    cat("cumulative axes contribution: ",
        round(cumsum(ord$evals / sum(ord$evals)), 2))  
  }
  if (method == "nmds") {
    ord = metaMDS(data, k = 4, try = 10)
    cat("R^2 first two axes: ", 
        round(cor(vegdist(data), dist(scores(ord)[, 1:2]))^2, 3), "\n")
    cat("R^2 first axis: ", 
        round(cor(vegdist(data), dist(scores(ord)[, 1]))^2, 3))
  }
  
  if (method == "isomap") {
    ord = bestisomap(vegdist(data, "bray"), k = 2)
    cat("Cumulative explained variance (%): ", 
        ord$`Cumulative explained variance in %`)
    ord = ord$Scores
    x11()
  }
  
  fit = envfit(ord, dplyr::select(env_data, -pnr, -id), perm = 999)
  plot(ord)
  plot(fit, p.max = 0.05)
  return(ord)
}

# from facet_wrap documentation
# ggplot(sc, aes(DCA1, DCA2)) +
#   geom_point(data = transform(sc, year = NULL), colour = "grey85") +
#   geom_point() +
#   facet_wrap(~year)

#  applying the same idea to lattice
#' @title Multipanel plot grouped by a factor variable
#' @details Function creates a multipanel lattice plot conditioned by a factor
#'   variable.
#' @param x Name of the column in data which should represent the x-coordinate
#'   (NSE).
#' @param y Name of the column in data which should represent the y-coordinate
#'   (NSE).
#' @param by Name of the column representing the conditioning variable (NSE).
#' @param id Name of the id column (NSE)
#' @param data The dataframe containing the columns.
#' @return xyplot
#' @author Jannes Muenchow

multi_panel_plot = function(x, y, by, data, id = "id") {
  # well, I have to admit I am not a big fan of NSE, way too complicated...
  lim = range(data[, rlang::as_string(rlang::enexpr(y))]) - c(0.25, -0.5)
  p_1 = substitute(
    xyplot(y ~ x | by, data = data, pch = 16, col = "black", as.table = TRUE,
           ylim = lim,
           groups = by, cex = 0.8, layout = c(2, 2),
           scales = list(y = "same",
                         alternating = c(1, 0),
                         tck = c(1, 0)),
           strip = strip.custom(bg = "white",
                                par.strip.text = list(cex = 1)),
           panel = panel.superpose,
           # normally, one would use x, y, here, we use x_2 and y_2, otherwise
           # substitute would change x and y to the names of the ordination axes
           # which is not what we want here
           panel.groups = function(x_2, y_2, group.number, ...) { 
             # first plot all x and y values
             all_x = pull(data, x)
             all_y = pull(data, y)
             panel.points(all_x, all_y, col = "grey85", pch = 16)
             # secondly, overlay them with the points of the factor
             panel.points(x_2, y_2, ...)
             # add labels
             fil = dplyr::select(data, by) %>% pull %>% as.factor %>% levels
             fil = fil[group.number]
             lab = filter(data, by == fil) %>% pull(id)
             # a reviewer asked us to make the numbers legible, therefore only
             # use a subset
             ind = which(lab %in% c(1, 3, 11, 24, 33, 47, 50))
             panel.text(x_2[ind], y_2[ind], labels = lab[ind], cex = 1, pos = 3)
           })
    )
  eval(p_1)
}

# by label group idea found here:
# browseURL(file.path("https://magesblog.com/post/", 
#                     "2014-02-18-adding-labels-within-lattice-panels-by/"))
# nevertheless, plotting labels by group should be simpler...
# otherwise you could also do:
# trellis.focus("panel", 1, 1)
# panel.text(x, y, label)
# trellis.unfocus()
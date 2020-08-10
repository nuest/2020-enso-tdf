#' @title Function retrieves the DCA scores of the first two axes and plots them
#'   against two environmental tables
#' @details Function applies a DCA, retrieves the scores of the first axes and
#'   plots the scores against the environmental variables.
#' @param londo Plot-species matrix with an id column named \code{pnr} which
#'   will be used to rename the rownames, and then will be removed prior to
#'   executing the ordination.
#' @param expl Dataframe containing environmental variables. The dataframe
#'   should have an id column which should have a correspondence in the
#'   species-plot-matrix with the same name.
#' @param choices Number of DCA axes to be retained. Only \code{1:2} is
#'   currently supported.
#' @return A \code{list} containing two [lattice::xyplot()]s.
#' @author Jannes Muenchow
explore_dca = function(londo, id = "pnr", choices = 1:2, expl) {
  # just keep observations found in response and explanatory tables
  expl = inner_join(dplyr::select(londo, id), expl, by = id)
  # get rid off id but keep the id as rownames
  rownames(londo) = londo[, id]
  londo = dplyr::select(londo, -id)  
  if (any(colSums(londo) == 0)) {
    stop("There are empty columns")
  }
  # check if each plot has at least one observation
  if (!all(rowSums(londo) > 0)) {
    stop("There are empty rows!")
  }
  
  dca = decorana(londo, iweigh = TRUE)
  # extract scores and add id
  dca = dplyr::select(as.data.frame(scores(dca, display = "sites")), choices)
  dca = cbind(as.integer(row.names(dca)), dca)
  names(dca)[1] = id
  # join environmental data
  tmp = inner_join(dca, expl, by = id)
  tmp = select(tmp, -id)
  dca1 = reshape2::melt(select(tmp, -DCA2), id.var = "DCA1")
  dca2 = reshape2::melt(select(tmp, -DCA1), id.var = "DCA2")
  p_1 = xyplot(DCA1 ~ value | variable, data = dca1,
               col = "salmon", pch = 16,
               scales = list(relation = "free"),
               panel = function(x, y, ...) {
                 panel.xyplot(x, y, ...)
                 panel.loess(x, y, span = 0.9, lwd = 2.5, col = "gray")
               })
  p_2 = xyplot(DCA2 ~ value | variable, data = dca2,
               col = "salmon", pch = 16,
               scales = list(relation = "free"),
               panel = function(x, y, ...) {
                 panel.xyplot(x, y, ...)
                 panel.loess(x, y, span = 0.9, lwd = 2.5, col = "gray")
               })
  return(list(p_1, p_2))
}

#' @title Variation partitioning applied to an ordination result or
#'   Hellinger-transformed matrix
#' @details Function applies variation partitioning to the scores of a DCA or
#'   hellinger-transforms the input matrix prior to the variation paritioning.
#' @param londo Plot-species matrix with an id column which will be used to join
#'   the environmental data and to rename the rownames prior to its deletion
#'   before executing the ordination.
#' @param expl_1 Dataframe containing environmental variables. The dataframe
#'   should have an id column which should have a correspondence in the
#'   species-plot-matrix with the same name.
#' @param expl_2 Dataframe containing environmental variables. The dataframe
#'   should have an id column which should have a correspondence in the
#'   species-plot-matrix with the same name.
#' @param choices Number of DCA axes to be retained (default: 1:2).
#' @return Output of [vegan::varpart()].
#' @author Jannes Muenchow
ordi_varpart = function(londo, choices = 1:2,  id = "pnr", expl_1, expl_2,
                        dca = TRUE) {
  # just keep observations found in response and explanatory tables
  expl_1 = inner_join(dplyr::select(londo, id), expl_1, by = id)
  expl_2 = inner_join(dplyr::select(londo, id), expl_2, by = id)
  # get rid off id but keep the id as rownames
  rownames(londo) = londo[, id]
  londo = dplyr::select(londo, -id)  
  if (any(colSums(londo) == 0)) {
    stop("There are empty columns")
  }
  # check if each plot has at least one observation
  if (!all(rowSums(londo) > 0)) {
    stop("There are empty rows!")
  }
  # apply DCA
  if (isTRUE(dca)) {
    # d = decostand(londo, "pa")
    dca = decorana(londo, iweigh = TRUE)
    # plot(dca, display = "sites")
    # Single axis contribution
    sc = round(dca$evals / sum(dca$evals), 2)
    # Cumulative contribution
    cc = round(cumsum(dca$evals / sum(dca$evals)), 2)
    # extract scores and add id
    dca = dplyr::select(as.data.frame(scores(dca, display = "sites")), choices)
    dca = cbind(as.integer(row.names(dca)), dca)
    names(dca)[1] = id
    vp = varpart(dplyr::select(dca, -id), dplyr::select(expl_1, -id), 
                dplyr::select(expl_2, -id))
    # # you can apply a weight to the result (but the result is more or less the
    # # same just with the difference that the explained variance is much smaller)
    # d = vp$part$indfract[1:3, 3] 
    # weight = cc[ncol(dplyr::select(dca, -id))]
    # vp$part$indfract[1:3, 3] = d * weight 
    # vp$part$indfract[4, 3] = vp$part$indfract[4, 3] + sum(d - (d * weight))
        
    # test fraction a & c
    a = anova.cca(rda(dplyr::select(dca, -id), dplyr::select(expl_1, -id), 
                      dplyr::select(expl_2, -id), step = 1000))
    #Test of fraction c
    c = anova.cca(rda(dplyr::select(dca, -id), dplyr::select(expl_2, -id),
                      dplyr::select(expl_1, -id), step = 1000))
    breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1)
    labs = labels = c("***", "**", "*", ".", "")
    a = cut(a$`Pr(>F)`[1], breaks, labs, include.lowest = FALSE)
    c = cut(c$`Pr(>F)`[1], breaks, labs, include.lowest = FALSE)

    return(list("vp" = vp, "a" = a, "c" = c))
  } else {
    return(varpart(londo, expl_1, expl_2, transfo = "hel"))
  }
}


bestisomap <- function (distance, kmin=3, ndim=3, criterion="overall") {

################################################################################
##                                                                            ##
## "bestisomap" finds the Isomap-solution offering a maximum of explained     ##
## variance on all or the first dimsension. Isomap is calculated using the    ##
## vegan-package (Oksanen, Kindt, Legendre, O'Hara, Simpson, Solymos,         ##
## Stevens, & Wagner, 2009, http://vegan.r-forge.r-project.org/)              ##
##                                                                            ##
##                          VERSION 1.0                                       ##
##                                                                            ##
## Author: Hannes Feilhauer, Vegetation Geography, University of Bonn         ##
##                                                                            ##
## ARGUMENTS                                                                  ##
##                                                                            ##
## distance   object containing inter-sample dissimilarities, e.g.,           ##
##            output from vegdist ()                                          ##
## kmin       smalles k-value as starting point; k determines the number of   ##
##            nearest neightbors considered in Isomap. Small k-values cause   ##
##            a non-linear solution, growing k-values increase linearity      ##
## ndim       dimensionality of the Isomap-space                              ##
## criterion  "overall" finds the solution with maximized explained variance  ##
##            across all dimensions defined by ndim (recommended), "first"    ##
##            finds a solution with maximum explained variance on the first   ##
##            dimension                                                       ##
##                                                                            ##
################################################################################


  require (vegan) || stop ("Requires the vegan package")

## minimum dimensionality of the Isomap-space = 2

  if (ndim < 2)
    ndim <- 2

## show distribution of the input-distances

#  hist (distance, xlab="Distance", ylab="Frequency", main="")

## transfromation desired? if yes, transform

#  trans <- readline ("Transform distances (none, sqrt, square)?")
#
#  if (trans == "sqrt") {
#    distance <- sqrt (distance)
#    hist (distance, xlab="Distance", ylab="Frequency", main="") }

#  if (trans == "square") {
#    distance <- distance^2
#    hist (distance, xlab="Distance", ylab="Frequency", main="") }
    
## determine number of samples, list of k-values to be tested, & build memory

  nsam <- nrow (as.matrix (distance))
  klist <- c (kmin : (nsam - 1))
  nruns <- length (klist)
  ev <- matrix (0, nruns, ndim)
    rownames (ev) <- klist
  
## set progress bar

  pb <- txtProgressBar (min=0, max=(nruns + 1), style=3)
    setTxtProgressBar(pb, 0)
    
## calculate Isomap-solutions for growing k-values

  for (i in c (1:nruns)) {
    # suppressWarnings (geod <- isomapdist (distance, k=klist[i], ndim=ndim))
    suppressWarnings (geod <- isomapdist (distance, k=klist[i], ndim=ndim,
                                          fragmentedOK = TRUE))
    #Question: Why do use the cmdscale-command instead of the isomap-command
    #Is it the same, as cmdscale is implicitly used by isomap -> yep, just type in:
    #isomap
    #and you receive:
    #function (dist, ndim = 10, ...) {
    #dist <- isomapdist(dist, ...)
    #out <- cmdscale(dist, k = ndim, eig = TRUE)
    
    score <- scores (cmdscale(geod, k=ndim, eig=TRUE))
    if (nrow (score) != nsam)
      ev[i,] <- NA
  
    if (nrow (score) == nsam) {
      for (j in 1:ndim)
        ev[i,j] <- cor (distance, dist (score[,1:j]))^2 * 100
      }
  
    setTxtProgressBar(pb, i) }
  
## filter fragmented solutions

  ev <- na.omit (ev)
  klist <- as.numeric (rownames (ev))

## find best solution

  if (criterion == "overall")
    kopt <- klist[which.max (ev[,ndim])]
  
  if (criterion == "first")
    kopt <- klist[which.max (ev[,1])]
    
## calculate best solution again, extract scores
  
  ism <- isomap (distance, k=kopt, ndim=ndim)                           
  ismscores <- scores (ism)
    setTxtProgressBar(pb, nruns + 1)
    
## graphic representation of the solution

  x11 (width=9, height=6)
  layout (matrix(c(1,2,3,3,3,3), 2, 3))

## plot explained variances

  plot (klist, ev[,ndim], type="n", ylim=c (0, 100), xlab="k", ylab="EV [%]", 
        axes=F, main="Cumulative Explained Variance" )
    for (n in ndim:1){ 
      polygon (c (klist[1], klist, klist[length (klist)]), c (0, ev[,n], 0), 
               col=rainbow (ndim)[n])}
    axis (1)
    axis (2, las=2)

## list Isomap parameters and EVs of the selected solution
  
  plot (c (1:(ndim+2)), c (1:(ndim+2)), type="n", axes=F, xlab="", ylab="")
    text (1, ndim+2, labels="k", pos=4)
    text (floor ((ndim/2)+2), ndim+2, labels=kopt, pos=4)
    text (1, ndim+1, labels="ndim", pos=4)
    text (floor ((ndim/2)+2), ndim+1, labels=ndim, pos=4)
    text (1, ndim, labels="Dim 1", pos=4, col=rainbow (ndim)[1])
    text (floor ((ndim/2)+2), ndim, labels=round (ev[as.character (kopt), 1], 
          2), pos=4)  
    label <- paste ("Dim 1:", ndim:2, sep="")  
    for (m in (ndim - 1):1) {
       text (1, m, labels=label[m], pos=4, col=rainbow (ndim)[c (ndim:1)[m]])
       text (floor ((ndim/2)+2), m, labels=round (ev[as.character (kopt), 
             c (ndim:1)[m]], 2), pos=4) }

## plot Isomap

  suppressWarnings (plot (ism, xlab="Dim 1", ylab="Dim2", main="Isomap", 
                          axes=F))
    points (ismscores[,1:2], pch=19)
    axis (1)
    axis (2, las=2)
      
  list("Scores" = ismscores, 
       "k" = kopt,
       "ndim" = ndim,
       "Cumulative explained variance in %" = round(ev[as.character (kopt),], 2))
             }
             
# item generator
.__whatkind_ <- function(kind)
{
  if (kind %in% c('.__item_clothing', '.__item_furnishings'))
    .__item_qual <- c("Broken", "Shoddy", "", "Fine", "Magnificent")
  else if (kind=='.__item_provisions')
    .__item_qual <- c("rancid", "stale", "palatable", "fresh", "yummy")

  return(.__item_qual)
}

make_item <- function(kind='rand', qual='rand')
{
  if (kind=='rand')
    kind <- samp(c('.__item_furnishings', '.__item_clothing', '.__item_provisions'))

  .__item_qual <- .__whatkind_(kind)
  kind <- eval(parse(text=kind))
  
  if (qual=='rand')
    qual <- sample(1:5, size=1, prob=c(.05, .2, .5, .2, .05))
  
  furn <- samp(kind[, 2])
  mine <- kind[which(kind[, 2]==furn), ]
  if (mine[1] != "")
    num <- sample(seq(1, 10, .25), 1)
  else
    num <- ""
  
  qualmod <- t(data.frame(c(0, .4), c(.4, .8), c(.8, 1.2), c(1.2, 1.6), c(1.6, 2)))
  
  value <- max(0, round(abs(as.numeric(mine[3])*(runif(1, qualmod[qual, 1], qualmod[qual, 2])))))
  value <- value*max(as.numeric(num), 1, na.rm=TRUE)
  what <- as.character(kind[which(kind[, 2]==furn), 1])
  unit <- as.character(kind[which(kind[, 2]==furn), 4])
  weight <- as.numeric(mine[5])*max(as.numeric(num), 1, na.rm=TRUE)
  
  return( fixstr(paste( num, " ", what, " ", .__item_qual[qual], " ", furn, " ", "(", value, unit, ", ", weight, " lbs)" , sep="")) )
}


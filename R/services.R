#lodging_qual <- 

menu <- function(qual='rand')
{
  if (qual=='rand')
    qual <- samp(1:5)
  
  qualmod <- t(data.frame(c(0, .4), c(.4, .8), c(.8, 1.2), c(1.2, 1.6), c(1.6, 2)))
  
  value <- max(0, round(abs(as.numeric(mine[3])*(runif(1, qualmod[qual, 1], qualmod[qual, 2])))))
  value <- value*max(as.numeric(num), 1, na.rm=TRUE)
}

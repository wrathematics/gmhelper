# ---------------------------------
# PHB Methods
# ---------------------------------

m.ii <- function(.)
{
  out <- numeric(6)
  for (i in 1:6){
    out[i] <- sum(sample(1:6, size=3, replace=TRUE))
    test <- sum(sample(1:6, size=3, replace=TRUE))
    if (out[i] < test) out[i] <- test
  }
  return(sort(out))
}

m.iii <- function(.)
{
  out <- numeric(6)
  for (i in 1:6){
    out[i] <- sum(sample(1:6, size=3, replace=TRUE))
  }
  return(sort(out))
}

m.iv <- function(.)
{
  out <- numeric(12)
  for (i in 1:12){
    out[i] <- sum(sample(1:6, size=3, replace=TRUE))
  }
  return(sort(tail(sort(out), 6)))
}

m.v <- function(.)
{
  out <- numeric(6)
  for(i in 1:6){
    out[i] <- sum(tail(sort(sample(1:6, size=4, replace=TRUE)), 3))
  }
  return(sort(out))
}


m.vi <- function(.)
{
  stats <- rep(8, 6)
  dies <- sample(1:6, size=7, replace=TRUE)
  
  i <- j <- 1
  while(TRUE){
  test <- stats[i] + dies[j]
    if (test <= 18){
      stats[i] <- test
      j <- j+1
    } else i <- i+1
    
    if (i > 6 || j > 7) break
  }
  return(sort(stats))
}

# ---------------------------------
# PO:S&P Methods
# ---------------------------------

# vii
m.vii <- function(.)
{
  return(75/6)
}

# viii
# depends on distribution of dice:
m.viii <- function(k)
{
  dist <- list(c(4,4,4,4,4,4), c(5,4,4,4,4,3), c(5,5,4,4,3,3), c(5,5,5,3,3,3), c(6,4,4,4,3,3), c(6,5,4,3,3,3), c(6,6,3,3,3,3))
  
  stats <- numeric(6)
  for (i in 1:6){
    stats[i] <- sum(tail(sort(sample(1:6, size=dist[[k]][i], replace=TRUE)), 3))
  }
  return(sort(stats))
}

# ix
m.ix <- function(.)
{
  mean(c(68, 70, 72, 72, 74, 74, 76, 76, 78, 78, 80)/6)
}

# ---------------------------------
# (5d6, keep top 3)x6 --- high rollin
# ---------------------------------

m.5d6 <- function(.)
{
  out <- numeric(6)
  for(i in 1:6){
    out[i] <- sum(tail(sort(sample(1:6, size=5, replace=TRUE)), 3))
  }
  return(sort(out))
}

# ---------------------------------
# meta
# ---------------------------------

stats_roller <- list(
  phb=list(m.ii, m.iii, m.iv, m.v, m.vi), 
  snp=list(m.vii, m.vii, m.ix), 
  other=list(m.5d6)
)





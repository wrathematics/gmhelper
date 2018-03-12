# sampler shorthand
samp <- function(x, ex=FALSE) sample(x, size=1)

# remove gaps in strings
fixstr <- function(...)
{
  str = paste(list(...), collapse=" ")
  
  str <- unlist(strsplit(str, " "))
  str <- paste(str[which(str!="")], collapse=" ")
  
  str
}

# currency exchange
exchange <- function(amt, inn, out)
{
  money <- c("cp", "sp", "ep", "gp", "pp")
  conv <- c(1, 10, 50, 100, 500)
  
  inn <- which(money==inn)
  out <- which(money==out)
  
  ret <- floor( amt * conv[inn]/conv[out] )
  if (ret==0)
    stop("Conversion is not possible")
  else
    return( ret )
}

# swap 'a' for 'b' in str
strswap <- function(str, a, b)
{
  strs <- unlist(strsplit(str, a))
  for (i in 1:(length(strs)-1)){
    strs[i] <- paste(strs[i], b, sep="")
  }
  return( paste(strs, collapse="") )
}

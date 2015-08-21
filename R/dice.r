### 11 = 6+2+3 (3d6, e.g.)
addup_dice <- function(formula)
{
  sum(as.numeric(unlist(strsplit(formula, split="[+]"))))
}



roller <- function(total="0", accumulate=FALSE, n, d)
{
  # If roller(roller()) is called, orders get funny
  invisible(eval(total))
  
  d <- as.integer(d)
  
  new.vals <- sample(x=1L:d, size=n, replace=TRUE)
  
  ret <- paste(new.vals, collapse="+")
  
  if (accumulate && total != "0")
  {
    return( paste(total, ret, sep="+") )
  }
  else
    return( ret )
}



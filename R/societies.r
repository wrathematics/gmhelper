coinflip <- function() sample(0:1, size=1)
fixspace <- function(str) gsub(str, pattern=" +", replacement=" ")

society <- function()
{
  adjective <- c("mystic", "enlightened", "secret", "weathered", "rare",
    "broken", "knowing", "fiery")

  type <- c("sorority", "society", "assassins", "star", "chapter", "association",
    "order", "fraternity", "sisterhood", "priesthood", "brotherhood", "couple",
    "school", "fellowship", "chapter")

  fin <- c("key", "enlightened", "lodge", "circle", "oxen", "asp", "crocodile",
    "eel", "flame", "robes")


  if (coinflip())
    pretype <- sample(adjective, size=1)
  else
    pretype <- ""

  if (coinflip())
    prefin <- sample(adjective, size=1)
  else
    prefin <- ""

  soc <- paste("The", pretype, sample(type, size=1), "of the", prefin, sample(fin, size=1))

  gsub(soc, pattern="  ", replacement=" ")
}

society()

# Meta
char_name <- function(race, first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (race=='rand')
    race <- sample(c("half-elf", "human", "dwarf", "elf", "halfling", "gnome"), 1)

  eval(parse(text=paste(race, "_name(", first, ",", middle, ",", last,")", sep="")))
}

# -------------------------------------------------
# Humanoids
# -------------------------------------------------

# internal meta
.__name_maker <- function(race, sex, last)
{
  if (sex=='m' || sex=='M')
    col <- 2
  else
    col <- 3
  
  if (last==FALSE)
    fl <- 1
  else{
    col <- 5
    fl <- 4
  }
  
  stem1 <- sample(race[which(race[, fl] != ""), fl], 1)
  stem2 <- sample(race[which(race[, col] != ""), col], 1)
  name <- paste(stem1, stem2, sep="")

  return(name)
}

# Human --- names are mixes of real saxon, viking, norman, welsh, and gaelic historical names
human_name <- function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (sex=='m' || sex=='M')
    col <- 1
  else
    col <- 2

  if (first)
    f <- as.character(sample(.__human_[which(.__human_[, col] != ""), col], 1))
  else
    f <- ""
  
  if (middle)
    m <- as.character(sample(.__human_[which(.__human_[, col] != ""), col], 1))
  else
    m <- ""
  
  if (last)
    l <- .__name_maker(.__human_, sex, TRUE)
  else
    l <- ""
  
  return( fixstr( paste( f, m, l ) ) )
}

# Dwarves
dwarf_name <- function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (first)
    f <- .__name_maker(.__dwarf_, sex, FALSE)
  else
    f <- ""
  
  if (middle)
    m <- .__name_maker(.__dwarf_, sex, FALSE)
  else
    m <- ""
  
  if (last)
    l <- .__name_maker(.__dwarf_, sex, TRUE)
  else
    l <- ""
  
  return( fixstr( paste( f, m, l ) ) )
}

# Elves
elf_name <- function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (first)
    f <- .__name_maker(.__elf_, sex, FALSE)
  else
    f <- ""
  
  if (middle)
    m <- .__name_maker(.__elf_, sex, FALSE)
  else
    m <- ""
  
  if (last)
    l <- .__name_maker(.__elf_, sex, TRUE)
  else
    l <- ""
  
  return( fixstr( paste( f, m, l ) ) )
}

# Halfling
halfling_name <- function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (first)
    f <- .__name_maker(.__halfling_, sex, FALSE)
  else
    f <- ""
  
  if (middle)
    m <- .__name_maker(.__halfling_, sex, FALSE)
  else
    m <- ""
  
  if (last)
    l <- .__name_maker(.__halfling_, sex, TRUE)
  else
    l <- ""
  
  return( fixstr( paste( f, m, l ) ) )
}

# Gnomes
gnome_name <- function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (first)
    f <- .__name_maker(.__gnome_, sex, FALSE)
  else
    f <- ""
  
  if (middle)
    m <- .__name_maker(.__gnome_, sex, FALSE)
  else
    m <- ""
  
  if (last)
    l <- .__name_maker(.__gnome_, sex, TRUE)
  else
    l <- ""
  
  return( fixstr( paste( f, m, l ) ) )
}


# Orcs
orc_name <- function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (first)
    f <- .__name_maker(.__orc_, sex, FALSE)
  else
    f <- ""
  
  if (middle)
    m <- .__name_maker(.__orc_, sex, FALSE)
  else
    m <- ""
  
  if (last)
    l <- barbarian_name()
  else
    l <- ""
  
  return( fixstr( paste( f, m, l ) ) )
}

# Trolls
troll_name <- function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (first)
    f <- .__name_maker(.__troll_, sex, FALSE)
  else
    f <- ""
  
  if (middle)
    m <- .__name_maker(.__troll_, sex, FALSE)
  else
    m <- ""
  
  if (last)
    l <- barbarian_name()
  else
    l <- ""
  
  return( fixstr( paste( f, m, l ) ) )
}

# -------------------------------------------------
# Last names
# -------------------------------------------------

# Barbarian
barbarian_name <- function()
{
  f <- sample(.__barbarian_[, 1], 1)
  l <- sample(.__barbarian_[which(.__barbarian_[, 2] != ""), 2], 1)
  
  return( fixstr( paste(f, l, sep="") ) )
}

# 

# government type
#monarchy, democracy, anarchy

# Services/points of interest
services <- list(
  c("Herbalist", "Fur trader", "Tavern", "Butcher"),
  c("Thieves", "Inn", "Clothier"),
  c("Instruments", "Furnishings", "Temple", "Blacksmith", "Armoury", "Weaponsmith"),
  c("Tack and Harness", "Magical Sundries"),
  c("Torture", "Shipyard")
)



# points of interest/landmarks
# by an (elf-name) river, at the foot of the (dwarf-name) mountains
poi <- list(
  c(""),
  c(""),
  c("Statue"),
  c(""),
  c("")
)


# interesting facts/plot hooks
other <- list(
  c("Orcs nearby"),
  c(""),
  c(""),
  c(""),
  c("")
)



# important people names
important_people <- function(race, sex='m')
{
  name <- char_name(race, first=TRUE, middle=FALSE, last=FALSE, sex='m')
  
  # title
  title_first <- 
  
  
  return( paste(name, "the", title) )
}


# culture/description
# e.g., if no tavern, roll for alcohol outlawed













# RACES:  human, elf, dwarf, halfling, gnome
make_a_place <- function(race='rand', size='rand')
{
  # Name
  name <- town_name(race)
  
  if (size=='rand')
    size <- sample(1:5, 1)
  
  if (race=='human'){
    sizenames <- c("Hamlet", "Village", "Town", "Keep", "Castle")
  
    if (size %in% 1:3)
      name <- paste("The", sizenames[size], "of", name)
    else
      name <- paste(name, sizenames[size])
  } else if (race=='elf'){
  
  }
  
  # Population
  sizepop_mn <- c(15, 50, 100, 250, 500)
  sizepop_sd <- c(10, 20, 40, 75, 150)/2
  pop <- round(abs(rnorm(1, sizepop_mn[size], sizepop_sd[size])))
  
  
  
  return( list(name, pop) )
}




# -------------------------------------------------
# Old school adventure namer
# -------------------------------------------------

adventure_name <- function()
{
  wh <- sample(1:3, 1)
  
  # first/name
  firstname <- c("Vault of", "Shrine of", "Scourge of", "Treachery of", "Deceit of")
  people <- c("Elf King", "Dwarven Glory", "Magus", "Fallen Bishop", "Goblin Horde", "Slave Lords", "Drow", "Pirate King", "Serpent Cult", "False God", "Orc Warlord", "Brigand Barons")

  # first/place
  firstplace <- c("Expedition to", "Beneath", "Atop", "Battle for")
  adj <- c("Black", "White", "Red", "Green", "Ruined", "Ice", "Fire", "Forsaken", "Lost", "Secret", "Forgotten", "Deep", "Dark", "Stone", "Bone")
  places <- c("Hall", "Hills", "Valley", "Gate", "Tower", "Tomb", "Castle", "Keep", "Temple", "Cathedral", "Mountain", "Caverns", "Desert", "Barrows", "Plains", "Fane", "Undercity", "Maze")

  # first/name-or-place
  either <- c("Horror of", "Saga of", "Hunt for", "Disappearance of", "Church of", "Slave Pits of", "Palace of", "Beyond", "Ruins of", "Assassins of", "Lair of", "Monastary of", "Search for")

  # first/feeling
  that <- c("Shades of", "Temple of")
  tfwqual <- c("Eldrich", "Sunless", "Ancient", "Black", "White", "Elder", "Dark", "Crimson", "Bloody", "Shadowy", "Golden")
  feelwhen <- c("Sorrows", "Nightmares", "Ruin", "Insanity", "Shadow", "Evil", "Secrets", "Ages", "Death", "Madness", "Annihilation", "Chaos", "Doom", "Dread", "Woe", "Suffering", "Horror", "Gloom", "Devestation", "Sorrow", "Terror", "Malice")

  if (wh==1){
    first <- sample(c(firstname, either), 1)
    last <- sample(people, 1)
    out <- paste("The", first, "the", last)
  } 
  else if (wh==2){
    first  <- sample(c(firstplace, either), 1)
    adj <- sample(adj, 1)
    place <- sample(places, 1)
    cf <- sample(1:2, 1)
    if (cf==1)
      name <- paste("of", char_name('rand', TRUE, FALSE, FALSE))
    else
      name <- ""
    out <- paste(first, "the", adj, place, name)
  } 
  else {
    first <- sample(that, 1)
    qual <- sample(tfwqual, 1)
    feel <- sample(feelwhen, 1)
    out <- paste(first, "the", qual, feel)
  }

  return( fixstr( out ) )
}

# -------------------------------------------------
# Dungeons
# -------------------------------------------------

dungeon_name <- function(qual=NA)
{
  # Data
  qualifier <- c("", "Ruined ", "Forsaken ", "Dread ", "Dark ", "Lost ", "Secret ", "Forsaken ", "Black ", "Sinister ", "Fiendish ")

  type <- c("Keep", "Castle", "City", "Cliffs", "Fane", "Cave", "Island", "Room", "Undercity", "Delve", "Lair", "Tunnels", "Hive", "Chambers", "Shrine", "Vaults", "Catacombs", "Dungeon", "Crypt", "Prison", "Sepulcher", "Tomb", "Tower", "Pit", "Caverns", "Barrow", "Halls", "Warrens", "Temple", "Labyrinth", "Sanctum")

  feeltype <- c("", "Unending", "Treacherous", "Ghastly", "Dismal", "Mighty", "Profane", "Elemental", "Baleful", "Dire", "Deranged", "Bloody", "Unearthly", "Unimaginable", "Demonic", "Dire", "Infernal", "Gloomy")
  feel <- c("Doom", "Dread", "Woe", "Suffering", "Horror", "Gloom", "Devestation", "Sorrow", "Terror", "Malice")

  vagueowner <- c("Sorrows", "Nightmares", "Ruin", "Insanity", "Shadow", "Evil", "Secrets", "Ages", "Death", "Madness", "Annihilation", "Chaos")

  owner_qualifier <- c("", "Eldrich", "Sunless", "Ancient", "Black", "White", "Elder", "Dark", "Crimson", "Bloody", "Shadow", "Golden")

  owner1 <- list(
    o1 <- c("Souls", "Worms"),
    o2 <- c("Horror", "Forgotten Goddess"),
    o3 <- c("Wyrm", "Gargoyle"),
    o4 <- c("Dragon"),
    o5 <- c("Elder God")
  )

  owner2 <- list(
    o1 <- c("Goblin", "Orc", "Dwarven", "Swordmaster"),
    o2 <- c("Giant"),
    o3 <- c("Vampire", "Wraith"),
    o4 <- c("Lich"),
    o5 <- c('Devil', 'God', 'Tarrasque')
  )

  owner_post_qualifier <- c("", " Lunatic", " Dutchess", " Warlord", " Lady", " Emperor", " Archmage", " Count", " Duke", " Earl", " Prince", " Princess", " Countess", " Queen", " Lord", " King", " Knight", " Mage", " Baron")

  # Function
  x1 <- paste(samp(qualifier), samp(type), sep="")

  f_v_o <- sample(1:3, size=1)
  if (f_v_o==1)
    x2 <- paste(samp(feeltype), " ", samp(feel), sep="") else
  if (f_v_o==2)
    x2 <- samp(vagueowner) 
  else {
    cf <- sample(1:2, size=1)
    if (cf==1) {
      owner <- owner1
      owner_post_qualifier <- ""
    } else if (cf==2) {
      owner <- owner2
    }
    if (is.na(qual))
      x2 <- paste("the ", samp(owner_qualifier), " ", samp(unlist(samp(owner))), samp(owner_post_qualifier), sep="")
    else
      x2 <- paste("the ", samp(owner_qualifier), " ", samp(owner[[qual]]), samp(owner_post_qualifier), sep="")
  }

  return( fixstr( paste(x1, "of", x2) ) )
}

# -------------------------------------------------
# Inns/Bars
# -------------------------------------------------

tavern_name <- function(tavern='rand', inn='rand')
{
  if (inn==FALSE)
    pinn <- ""
  else if (inn==TRUE)
    pinn <- "Inn"
  else if (inn=='rand')
    pinn <- sample(c("", "Inn"), 1)
  
  if (tavern==FALSE)
    type <- ""
  else if (tavern==TRUE)
    type <- sample(.__tavern_[which(.__tavern_[, 3] != ""), 3], 1)
  else if (tavern=='rand')
    type <- sample(c("", as.character(sample(.__tavern_[which(.__tavern_[, 3] != ""), 3], 1))), 1)
  
  if (pinn=="" || type=="")
    and <- ""
  else
    and <- "and"
  
  cf <- sample(1:2, 1)
  
  if (cf==1){
    adj <- sample(.__tavern_[which(.__tavern_[, 1] != ""), 1], 1)
    noun1 <- ""
  } else {
    adj <- ""
    noun1 <- paste(sample(.__tavern_[which(.__tavern_[, 2] != ""), 2], 1), "and")
  }
  
  noun2 <- sample(.__tavern_[which(.__tavern_[, 2] != ""), 2], 1)
#  type <- sample(.__tavern_[which(.__tavern_[, 3] != ""), 3], 1)
  
  name <- paste( "The", adj, noun1, noun2, type, and, pinn )
  
  return( fixstr( name ) )
}

# -------------------------------------------------
# Towns
# -------------------------------------------------

town_name <- function(race='rand')
{
  if (race=='rand')
    race <- sample(1:4, 1)
  else if (race=='human' || race=='h')
    race <- 1
  else if (race=='dwarf' || race=='d')
    race <- 2
  else if (race=='elf' || race=='e')
    race <- 3
  else if (race=='halfling' || race=='hl')
    race <- 4
  
  stem1 <- sample(.__town_[which(.__town_[, 2*(race-1)+1] != ""), 2*(race-1)+1], 1)
  stem2 <- sample(.__town_[which(.__town_[, 2*(race-1)+2] != ""), 2*(race-1)+2], 1)
  
  return( fixstr( paste( stem1, stem2, sep="") ) )
}

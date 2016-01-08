# Maps/Directions
gen_map <- function(qual="rand")
{
  .__map_qual <- c("Tattered", "Crude", "", "Fine", "Pristine")
  
  if (qual=="rand")
    qual <- sample(1:5, 1)
  
  cf <- sample(1:2, 1)
  if (cf==1)
    return( fixstr( paste(sample(.__map_qual, 1) , "Map of the", dungeon_name(qual=qual)) ) )
  else
    return( fixstr( paste(sample(.__map_qual, 1) , "Directions to the", dungeon_name(qual=qual)) ) )
}

# Mundane
gen_mundane <- function(qual="rand")
{
  if (qual=="rand") 
    qual <- sample(1:5, 1)
  
  return( fixstr( sample(as.character(.__mundane_[which(as.character(.__mundane_[, qual]) != ""), qual]), 1) ) )
}

# Food and drink
gen_food <- function(qual="rand")
{
  .__food_qual <- c("rancid", "stale", "palatable", "fresh", "yummy")

  if (qual=="rand")
    qual <- sample(1:5, 1)

  fq <- .__food_qual[qual]
  fd <- sample(as.character(unlist(.__food_)), 1)
  return( fixstr( paste(fq, fd) ) )
}

gen_drink <- function(qual="rand")
{
  .__drink_qual <- c("broken", "half empty", "", "fine", "large")

  if (qual=="rand")
    qual <- sample(1:5, 1)

  dq <- .__drink_qual[qual]
  dk <- sample(as.character(unlist(.__drink_)), 1)
  return( fixstr( paste(dq, dk) ) )
}

# Keys
gen_key <- function(qual="rand")
{
  if (qual=="rand")
    qual <- sample(1:5, 1)
  
  .__key_qual <- list(
    k1 <- c("wooden", "wax", "copper", "lead"),
    k2 <- c("pewter", "brass", "iron"),
    k3 <- c("bronze", "steel"),
    k4 <- c("silver", "gold"),
    k5 <- c("platinum", "mithrel")
  )
  
  .__key_size <- c("tiny", "small", "", "large", "enormous")
  
  return( fixstr( paste(sample(.__key_size, 1), sample(.__key_qual[[qual]], 1), "key") ) )
}

# Money
gen_money <- function(qual='rand')
{
  if (qual=='rand') 
    qual <- sample(1:6, size=1, prob=c(.1, .25, .3, .15, .15, .05))

  .__money_ <- c("", "cp", "sp", "ep", "gp", "pp")

  mn_sd <- c(1, 125, 100, 75, 50, 25)

  if (qual > 1)
    mn <- paste( floor(abs(rnorm(1, 10, mn_sd[qual])))+1, .__money_[qual] )
  else 
    mn <- paste( sample(1:7, 1), "pieces of fools gold" )

  return( mn )
}

gen_gems <- function(qual='rand')
{
  if (qual=='rand') 
    qual <- sample(1:6, size=1, prob=c(.565, .25, .1, .05, .025, .01))
  
  .__gems_qual <- c("ornamental", "semi-precious", "fancy", "precious", "gem", "jewel")
  .__gems_val <- c(10, 50, 100, 500, 1000, 5000)
  
  gm <- sample(.__gems_[which(.__gems_[, qual]!=""), qual], 1)
  worth <- floor(abs(rnorm(1, .__gems_val[qual], 2*log(.__gems_val[qual], 10))))
  
  return( paste(gm, " (", .__gems_qual[qual], ", ", worth, "gp)", sep="") )
}

# Jewelry
gen_jewelry <- function(qual='rand')
{
  if (qual=='rand') 
    qual <- sample(1:6, size=1, prob=c(.34, .25, .2, .15, .05, .01))

  .__jewelry_qual <- list(
    c("steel", "lead", "iron", "zinc", "nickel", "copper"),
    c("cobalt", "bronze", "pewter"),
    c("silver", "hepatizon"),
    c("gold", "amalgam", "osmiridium"),
    c("billon", "platinum"),
    c("mithril", "adamantine", "electrum", "orichalcum")
  )

  .__jewelry_val <- c(2, 8, 32, 128, 512, 2048)

  jewelry_sd <- c(1, 4, 16, 64, 256, 1024)
  jval <- paste( floor(abs(rnorm(1, .__jewelry_val[qual], jewelry_sd[qual]))), "gp" )
  
  # include gem
  if (sample(1:10, 1) == 10){
    gem <-  unlist(strsplit(unlist(strsplit(unlist(strsplit(gen_gems(qual=qual), split="[(]")), split=",")), split="[)]"))
    addgem <- paste("inset with small", gem[1])
    plus <- "+"
  } else {
    gem <- addgem <- plus <- ""
  }
  
  value <- paste("(", jval, plus, gem[length(gem)], ")", sep="")
  
  return( fixstr( paste( sample(.__jewelry_qual[[qual]], 1), sample(.__jewelry_[, ], 1), addgem, value ) ) )
}

# Potions
gen_potions <- function(qual='rand')
{
  if (qual=='rand')
    qual <- sample(1:5, size=1, prob=c(.66, .255, .05, .025, .01))

  pt <- sample(as.character(.__potions_[which(as.character(.__potions_[, qual]) != ""), qual]), 1)
  
  return( fixstr( paste("Potion of", pt ) ) )
}

# Potion generator
gen_pot_desc <- function(qual='rand', label='rand', labellang='rand', extra='rand')
{
  if (qual=='rand')
    qual <- sample(1:5, size=1, prob=c(.66, .255, .05, .025, .01))
  if (label=='rand')
    label <- as.character(sample(.__pot_desc_[which(.__pot_desc_[, 3]!=""), 3], 1))
  if (labellang=='rand')
    labellang <- as.character(sample(.__pot_desc_[which(.__pot_desc_[, 4]!=""), 4], 1))
  
  if (label=='None'){
    cf <- sample(1:2, size=1, prob=c(.2, .8))
    if (cf==1)
      potion <- "DRINK ME"
    else {
      potion <- 'None'
      labellang <- "NA"
    }
  } 
  else
    potion <- gen_potions(qual=qual)
  
  if (label!='Correct')
    actual <- paste("\nActual potion:\t\t", sample(as.character(.__potions_[which(as.character(.__potions_[, qual]) != ""), qual]), 1), sep="")
  else
    actual <- "\nActual Potion:\t\tNA"
  
  taste <- as.character(sample(.__pot_desc_[which(.__pot_desc_[, 1]!=""), 1], 1))
  container <- as.character(sample(.__pot_desc_[which(.__pot_desc_[, 2]!=""), 2], 1))
  color <- as.character(sample(.__pot_desc_[which(.__pot_desc_[, 5]!=""), 5], 1))

  if (extra=='rand')
    cf <- sample(1:2, prob=c(.4, .8), 1)
  if (cf==1)
    extra <- "None"
  else
    extra <- as.character(sample(.__pot_desc_[which(.__pot_desc_[, 6]!=""), 6], 1))
  
  # taking care of extras
  extrarow <- which(.__pot_desc_[, 6]==extra)
  # extra dice rolling
  test <- unlist(strsplit(extra, "DICE"))
  if (length(test)>1){
    dice <- unlist(strsplit(as.character(.__pot_desc_[extrarow, 8]), "d"))
    dice <- die(dice[1], dice[2])
    extra <- paste(test[1], dice, test[2])
  }
  # extra colors
  test <- unlist(strsplit(extra, "COLOR"))
  if (length(test)>1){
    color2 <- unlist(strsplit(as.character(.__pot_desc_[extrarow, 1]), "d"))
    if (color2=='var'){
      color <- "Varies; see 'Extra'"
      
    }
    else {
      color2 <- as.character(sample(.__pot_desc_[which(.__pot_desc_[, 5]!=""), 5], 1))
      extra <- strswap(extra, "COLOR", color2)
      if (length(unlist(strsplit(extra, "MAINCOL")))>1){
        extra <- strswap(extra, "MAINCOL", color)
        color <- "Special; see 'Extra'"
      }
    }
  }
  # smell
  test <- unlist(strsplit(extra, "SMELL"))
  if (length(test)>1){
    smell <- as.character(sample(.__pot_desc_[which(.__pot_desc_[, 1]!=""), 1], 1))
    extra <- strswap(extra, "SMELL", smell)
  }
  
  
  return( fixstr( paste("Label:\t\t\t\t", potion, "\nLabel Language:\t", labellang, "\nLabel Accuracy:\t\t", label, actual, "\nColor:\t\t\t\t", color, "\nTaste:\t\t\t\t", taste, "\nContainer:\t\t\t", container, "\nExtra:\t\t\t\t", extra, sep="") ) )
}


#simple_items <- list(item_maps, mundane, food, drink, key, money, gems, potions)
#sample(simple_items, size=1)[[1]]()



pocketloot <- function(qual)
{
  if (qual=="RANDOM")
  {
    qual <- 'rand'
    cangen <- list(gen_map, gen_mundane, gen_food, gen_key, gen_gems, gen_jewelry, gen_potions)
    moneyqual <- sample(1:6, 1)
  }
  else if (qual=="Pauper")
  {
    cangen <- list(gen_mundane, gen_food, gen_key)
    qual <- 1
    moneyqual <- sample(1:2, 1)
  }
  else if (qual=="Commoner")
  {
    cangen <- list(gen_map, gen_mundane, gen_food, gen_key)
    qual <- 2
    moneyqual <- sample(1:3, 1)
  }
  else if (qual=="Merchant")
  {
    cangen <- list(gen_map, gen_mundane, gen_food, gen_key, gen_potions)
    qual <- 3
    moneyqual <- sample(3:5, 1)
  }
  else if (qual=="Noble")
  {
    cangen <- list(gen_map, gen_mundane, gen_key, gen_gems, gen_potions)
    qual <- 4
    moneyqual <- sample(4:5, 1)
  }
  else if (qual=="King")
  {
    cangen <- list(gen_map, gen_mundane, gen_key, gen_gems, gen_jewelry, gen_potions)
    qual <- 5
    moneyqual <- 6
  }
  
  howmany <- sample(0:5, 1, prob=c(.1, .225, .225, .2, .15, .1))
  whichones <- sample(cangen, size=howmany, replace=TRUE)
  
  if (length(whichones)>0)
  {
    stuff <- lapply(seq_along(whichones), function(i) whichones[[i]](qual=qual) )
    money <- gen_money(moneyqual)
    loot <- c(stuff, money)
  } 
  else
    loot <- gen_money(moneyqual)
  
  
  loot
}

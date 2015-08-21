#' gmhelper
#' 
#' @import gWidgets
#' @import gWidgetsRGtk2
#' 
#' @name ngram-package
#' @docType package
#' @author Drew Schmidt \email{wrathematics AT gmail.com}
#' @keywords Package
NULL



#' @export
gmhelper <- function(.)
{
  # FUNCTIONS
  gennames <- function(h, ...)
  {
    whichsex <- svalue(names_radios)
    
    whichfml <- svalue(fml_boxes)
    if (length(whichfml)==0)
      svalue(names_textbox) <- "You must select at least one of 'First', 'Middle', or 'Last'"
    else {
      if ("First" %in% whichfml)
        first <- TRUE
      else
        first <- FALSE
      if ("Middle" %in% whichfml)
        middle <- TRUE
      else
        middle <- FALSE
      if ("Last" %in% whichfml)
        last <- TRUE
      else
        last <- FALSE
        
      whichrace <- svalue(race_radios)

      if (whichrace=="Human")  
        svalue(names_textbox) <- lapply(1:10, function(.) human_name(first=first, middle=middle, last=last, sex=whichsex) )
      else if (whichrace=="Elf")  
        svalue(names_textbox) <- lapply(1:10, function(.) elf_name(first=first, middle=middle, last=last, sex=whichsex) )
      else if (whichrace=="Dwarf")
        svalue(names_textbox) <- lapply(1:10, function(.) dwarf_name(first=first, middle=middle, last=last, sex=whichsex) )
      else if (whichrace=="Gnome")
        svalue(names_textbox) <- lapply(1:10, function(.) gnome_name(first=first, middle=middle, last=last, sex=whichsex) )
      else if (whichrace=="Halfling")
        svalue(names_textbox) <- lapply(1:10, function(.) halfling_name(first=first, middle=middle, last=last, sex=whichsex) )
      else if (whichrace=="Orc")
        svalue(names_textbox) <- lapply(1:10, function(.) orc_name(first=first, middle=middle, last=last) )
      else if (whichrace=="Troll")
        svalue(names_textbox) <- lapply(1:10, function(.) troll_name(first=first, middle=middle, last=last) )
    }
  }

  gentowns <- function(h, ...)
  {
    whichrace <- svalue(town_radios)
    if (whichrace=="Human")
      race <- 'h'
    if (whichrace=="Dwarf")
      race <- 'd'
    if (whichrace=="Elf")
      race <- 'e'
    if (whichrace=="Halfling")
      race <- 'hl'
    
    svalue(town_textbox) <- lapply(1:10, function(.) town_name(race=race) )
  }

  gentaverns <- function(h, ...)
  {
    tvinns <<- svalue(tavern_radios)
    if (length(tvinns)==0){
      svalue(tavern_textbox) <- lapply(1:10, function(.) tavern_name(tavern='rand', inn='rand') )
    } else {
      if (length(tvinns)==2){
        tavern <- TRUE
        inn <- TRUE
      }
      else if (tvinns=="Inn"){
        tavern <- FALSE
        inn <- TRUE
      }
      else if (tvinns=="Tavern"){
        tavern <- TRUE
        inn <- FALSE
      }
      
      svalue(tavern_textbox) <- lapply(1:10, function(.) tavern_name(tavern=tavern, inn=inn) )
    }
  }

  gendungeons <- function(h, ...)
  {
    whichdn <- svalue(dungeon_radios)
    if (whichdn=="RANDOM")
      qual <- NA
    else if (whichdn=="1-4")
      qual <- 1
    else if (whichdn=="5-8")
      qual <- 2
    else if (whichdn=="9-12")
      qual <- 3
    else if (whichdn=="13-15")
      qual <- 4
    else if (whichdn=="16-20")
      qual <- 5

    svalue(dungeon_textbox) <- lapply(1:10, function(.) dungeon_name(qual=qual) )
  }

  genadventure <- function(h, ...)
  {
    svalue(adventure_textbox) <- lapply(1:10, function(.) adventure_name() )
  }

  genpocket <- function(h, ...)
  {
    whichwl <- svalue(loot_radios)
    if (whichwl=="RANDOM"){
      qual <- 'rand'
      cangen <- list(gen_map, gen_mundane, gen_food, gen_key, gen_gems, gen_jewelry, gen_potions)
      moneyqual <- sample(1:6, 1)
    }
    else if (whichwl=="Pauper"){
      cangen <- list(gen_mundane, gen_food, gen_key)
      qual <- 1
      moneyqual <- sample(1:2, 1)
    }
    else if (whichwl=="Commoner"){
      cangen <- list(gen_map, gen_mundane, gen_food, gen_key)
      qual <- 2
      moneyqual <- sample(1:3, 1)
    }
    else if (whichwl=="Merchant"){
      cangen <- list(gen_map, gen_mundane, gen_food, gen_key, gen_potions)
      qual <- 3
      moneyqual <- sample(3:5, 1)
    }
    else if (whichwl=="Noble"){
      cangen <- list(gen_map, gen_mundane, gen_key, gen_gems, gen_potions)
      qual <- 4
      moneyqual <- sample(5, 1)
    }
    else if (whichwl=="King"){
      cangen <- list(gen_map, gen_mundane, gen_key, gen_gems, gen_jewelry, gen_potions)
      qual <- 5
      moneyqual <- 6
    }

    howmany <- sample(0:5, 1, prob=c(.1, .225, .225, .2, .15, .1))
    whichones <- sample(cangen, size=howmany, replace=TRUE)
    if (length(whichones)>0){
      svalue(loot_textbox) <- lapply(seq_along(whichones), function(i) whichones[[i]](qual=qual) )
      add(loot_textbox, "")
      add(loot_textbox, gen_money(moneyqual) )
    } else
      svalue(loot_textbox) <- gen_money(moneyqual)
  }

  genpotion <- function(h, ...)
  {
    if (svalue(potion_radios)=="RANDOM")
      qual <- 'rand'
    else
      qual <- as.numeric(svalue(potion_radios))

    if (svalue(potion_radios2)=="RANDOM")
      label <- 'rand'
    else
      label <- as.character(svalue(potion_radios2))

    if (svalue(potion_radios3)=="RANDOM")
      labellang <- 'rand'
    else
      labellang <- as.character(svalue(potion_radios3))

    svalue(potion_textbox) <- gen_pot_desc(qual=qual, label=label, labellang=labellang)
  }

  clear_dice <- function(h, ...)
  {
    svalue(simple_dice_box) <- svalue(simple_dice_sum) <- 0
  }

  simple_dice <- function(h, ...)
  {
    dienum <- as.numeric(svalue(simple_dice_number))
    die <- svalue(simple_dice_radios)
    extranum <- as.numeric(svalue(extra_dice_num))
    extrasides <- as.numeric(svalue(extra_dice_sides))
    addto <- svalue(extra_dice_addto)
    
    if (die=="d2 ")
      die <- 2
    else if (die=="d4 ")
      die <- 4
    else if (die=="d6 ")
      die <- 6
    else if (die=="d8 ")
      die <- 8
    else if (die=="d10 ")
      die <- 10
    else if (die=="d12 ")
      die <- 12
    else if (die=="d20 ")
      die <- 20
    else if (die=="d50 ")
      die <- 50
    else
      die <- 100
    
    if (!addto){
      svalue(simple_dice_box) <- paste(sample(1:die, size=dienum, replace=TRUE), collapse="+", sep="")
    } else {
      if (svalue(simple_dice_box)==0)
        svalue(simple_dice_box) <- paste(paste(sample(1:die, size=dienum, replace=TRUE), collapse="+"), collapse="+", sep="")
      else
        svalue(simple_dice_box) <- paste(svalue(simple_dice_box), "+", paste(sample(1:die, size=dienum, replace=TRUE), collapse="+"), collapse="+", sep="")
    }
    if (as.numeric(extranum>0))
      svalue(simple_dice_box) <- paste(svalue(simple_dice_box), paste(sample(1:as.numeric(extrasides), size=extranum, replace=TRUE), collapse="+"), collapse="+", sep="+")

    svalue(simple_dice_sum) <- eval(parse(text=svalue(simple_dice_box)))
  }

  gennpc <- function(h, ...)
  {
    stats <- svalue(gens_stats)
    if (stats=='RANDOM Stats')
      stats <- sample(1:5, 1)
    else if (stats=='Weak')
      stats <- 1
    else if (stats=='Below Average')
      stats <- 2
    else if (stats=='Average')
      stats <- 3
    else if (stats=='Above Average')
      stats <- 4
    else
      stats <- 5
    
    race <- svalue(gens_races)
    if (race=='RANDOM Race')
      race <- 'rand'
    else
      race <- tolower(race)
    
    class <- svalue(gens_class)
    if (class=='RANDOM Class')
      class <- 'rand'
    else
      class <- tolower(class)
    
    age <- 'rand'
    
    x <- roll_a_guy(strength=stats, race=race, class=class, alignment='rand', age='rand', htwt='rand', sex='rand', name='randomnameasdfqwert', level=1, l1hpreroll='max')
    svalue(generators_textbox) <- paste("Name:  ", x[[1]][1])
    add(generators_textbox, paste(paste("\nRace:  ", x[[1]][2]), paste("Class:  ", x[[1]][3]), sep="\t\t"), sep="      ")
    add(generators_textbox, paste(paste("\nAge:  ", x[[1]][4], sep="       "), paste("Sex:  ", x[[1]][5]), sep="\t\t"))
  }

  # -------------------------------------------------
  # ~gui~
  # -------------------------------------------------

  menuactions <- list(quit=gaction(label="Quit", icon="quit", handler=function(...) dispose(win)), 
                      about=gaction("About", icon="about", handler=function(...) aboutgmhelper()))
  aboutgmhelper <- function(...) gmessage("~buttz~")

  win <- gwindow(title="The GM Helper (TM)", height=200, width=500)

  # Quit menu for weirdos without window bars
  gmenu(list(File=menuactions), container=win)

  # Main tab wrangling
  nb <- gnotebook(container=win)

  # -----------------------
  # Names tab
  # -----------------------
  names <- ggroup(container=nb, label="Names", spacing=10, horizontal=FALSE)

  names_nb <- gnotebook(container=names, tab.pos=2, expand=TRUE)
  group_names <- ggroup(horizontal=FALSE, container=names_nb, label="NPC")
  names_textbox <- gtext("", width=400, height=177, container=group_names)  
  group_names_ <- gframe("Options", container=group_names, horizontal=TRUE, expand=TRUE)
  group_names_sex <- gframe("Sex", container=group_names_, horizontal=TRUE)
  names_radios <- gradio(c("M", "F"), container=group_names_sex, horizontal=TRUE)
  group_names_fml <- gframe("Name", container=group_names_, horizontal=TRUE)
  fml_boxes <- gcheckboxgroup(checked=c(T, F, T), c("First", "Middle", "Last"), container=group_names_fml, horizontal=TRUE)
  group_names_race <- gframe("Race", container=group_names_, horizontal=TRUE, expand=TRUE)
  race_radios <- gcombobox(c("Human", "Elf","Dwarf", "Halfling", "Gnome", "Orc", "Troll"), container=group_names_race, expand=TRUE)
  names_btn <- gbutton(
    text      = "GENERATE!",
    container = group_names,
    handler   = gennames
  )

  # Places tab
  group_dungeon <- ggroup(horizontal=FALSE, container=names_nb, label="Dungeon")
  dungeon_textbox <- gtext("", width=350, height=177, container=group_dungeon)
  group_dungeon_ <- gframe(text="Party Level", container=group_dungeon, horizontal=TRUE, expand=TRUE)
  dungeon_radios <- gradio(c("RANDOM", "1-4", "5-8", "9-12", "13-15", "16-20"), container=group_dungeon_, horizontal=TRUE)
  dungeon_btn <- gbutton(
    text      = "GENERATE!",
    container = group_dungeon,
    handler   = gendungeons
  )

  group_adventure <- ggroup(horizontal=FALSE, container=names_nb, label="Old School\n Adventure")
  adventure_textbox <- gtext("", width=350, height=177, container=group_adventure)
  group_adventure_ <- gframe(text="", container=group_adventure, horizontal=TRUE, expand=TRUE)
  adventure_btn <- gbutton(
    text      = "GENERATE!",
    container = group_adventure,
    handler   = genadventure
  )

  group_tavern <- ggroup(horizontal=FALSE, container=names_nb, label="Tavern & Inn")
  tavern_textbox <- gtext("", width=350, height=177, container=group_tavern)
  group_tavern_ <- gframe("Type of Establishment (select neither for random)", container=group_tavern, horizontal=TRUE, expand=TRUE)
  tavern_radios <- gcheckboxgroup(c("Tavern", "Inn"), container=group_tavern_, horizontal=TRUE)
  tavern_btn <- gbutton(
    text      = "GENERATE!",
    container = group_tavern,
    handler   = gentaverns
  )

  group_town <- ggroup(horizontal=FALSE, container=names_nb, label="Town")
  town_textbox <- gtext("", width=350, height=177, container=group_town)
  group_town_ <- gframe("Founding Race", container=group_town, horizontal=TRUE, expand=TRUE)
  town_radios <- gradio(c("Human", "Dwarf", "Elf", "Halfling"), container=group_town_, horizontal=TRUE)
#  glabel("", container=group_town_)
#  glabel("", container=group_town_)
  town_btn <- gbutton(
    text      = "GENERATE!",
    container = group_town,
    handler   = gentowns
  )

  # -----------------------
  # Loot
  # -----------------------

  loot <- ggroup(container=nb, label="Loot", spacing=10, horizontal=FALSE)

  loot_nb <- gnotebook(container=loot, tab.pos=2, expand=TRUE)
  group_loot <- ggroup(horizontal=FALSE, container=loot_nb, label=" Picked\nPockets")
  loot_textbox <- gtext("", width=400, height=177, container=group_loot)  
  group_loot_ <- gframe("Wealth of the Mark", container=group_loot, horizontal=TRUE, expand=TRUE)
  loot_radios <- gradio(c("RANDOM", "Pauper", "Commoner", "Merchant", "Noble", "King"), container=group_loot_, horizontal=TRUE)
  loot_btn <- gbutton(
    text      = "GENERATE!",
    container = group_loot,
    handler   = genpocket
  )

#  potion_nb <- gnotebook(container=loot, tab.pos=2, expand=TRUE)
  group_potion <- ggroup(horizontal=FALSE, container=loot_nb, label="Potions")
  potion_textbox <- gtext("", width=400, height=177, container=group_potion)  
  group_potion_ <- gframe("Options", container=group_potion, horizontal=TRUE, expand=TRUE)
  pot1 <- gframe("Potion Quality", container=group_potion_, horizontal=TRUE)
  potion_radios <- gradio(c("RANDOM", 1:5), container=pot1, horizontal=TRUE)
#  pot21 <- gframe(container=group_potion_, horizontal=TRUE)
  pot2 <- gframe("Label", container=group_potion_, horizontal=TRUE)
  potion_radios2 <- gcombobox(c("RANDOM", "None", "Correct", "Incorrect"), container=pot2, horizontal=TRUE)
  pot3 <- gframe("Label Language", container=group_potion_, horizontal=TRUE)
  potion_radios3 <- gcombobox(c("RANDOM", "Common", "Dwarf", "Elvish", "Halfling"), container=pot3, horizontal=TRUE)
  potion_btn <- gbutton(
    text      = "GENERATE!",
    container = group_potion,
    handler   = genpotion
  )


  # -----------------------
  # Rolling
  # -----------------------

  roll <- ggroup(container=nb, label="Rolling", spacing=10, horizontal=FALSE)

  roll_nb <- gnotebook(container=roll, tab.pos=2, expand=TRUE)
  group_roll <- ggroup(horizontal=FALSE, container=roll_nb, label="Dice")
  

  row0 <- ggroup(horizontal=FALSE, container=group_roll, expand=TRUE)
  row0f <- gframe("Standard Dice", horizontal=TRUE, container=row0, expand=TRUE)
  glabel(" ", container=row0)
  simple_dice_number_frame <- gframe("Number", container=row0f)
  simple_dice_number <- gedit("1", container=simple_dice_number_frame, width=6)
  simple_dice_radios_frame <- gframe(spacing=10, "Sides", container=row0f, expand=TRUE)
  simple_dice_radios <- gradio(c("d2 ", "d4 ", "d6 ", "d8 ", "d10 ", "d12 ", "d20 ", "d50 ", "d100"), selected=7, container=simple_dice_radios_frame, horizontal=TRUE)
  extra_dice_frame <- gframe("Options", container=row0, expand=TRUE)
  extra_dice_group <- gframe(spacing=10, "Additional Dice", container=extra_dice_frame, horizontal=TRUE)
  extra_dice_num <- gedit("0", container=extra_dice_group, width=3)
  glabel("d", container=extra_dice_group)
  extra_dice_sides <- gedit("0", container=extra_dice_group, width=3)
  glabel("                          ", container=extra_dice_frame)
  extra_dice_addto_frame <- gframe("Rerolling", container=extra_dice_frame, horizontal=TRUE)
  extra_dice_addto <- gcheckbox("Accumulate?", container=extra_dice_addto_frame)
  glabel("           ", container=extra_dice_addto_frame)
  roll_simple_btn <- gbutton("Reset", container=extra_dice_addto_frame, handler=clear_dice)
  glabel(" ", container=row0)
  simple_dice_sum_frame <- gframe("Total", container=row0, expand=TRUE)
  simple_dice_sum <- gedit("0", container=simple_dice_sum_frame, width=10)
  simple_dice_box_frame <- gframe("Total Breakdown", container=row0, expand=TRUE)
  simple_dice_box <- gedit("0", container=simple_dice_box_frame, width=66)
  roll_simple_btn <- gbutton(
    text      = "ROLL THE DIE!",
    container = row0,
    handler   = simple_dice
  )


  # -----------------------
  # Generators
  # -----------------------

  generators <- ggroup(container=nb, label="Generators", spacing=10, horizontal=FALSE)

  generators_nb <- gnotebook(container=generators, tab.pos=2, expand=TRUE)
  group_generators <- ggroup(horizontal=FALSE, container=generators_nb, label="NPC")
  generators_textbox <- gtext("", width=400, height=177, container=group_generators)  
  group_generators_ <- gframe("Options", container=group_generators, horizontal=TRUE, expand=TRUE)
  group_gens_char <- gframe("Race/Class", container=group_generators_, horizontal=FALSE, expand=TRUE)
    gens_races <- gcombobox(c("RANDOM Race", "Human", "Dwarf", "Elf", "Half-Elf", "Halfling", "Gnome"), container=group_gens_char, horizontal=TRUE)
    gens_class <- gcombobox(c("RANDOM Class", "Fighter", "Paladin", "Ranger", "Wizard", "Cleric", "Druid", "Thief", "Bard"), container=group_gens_char, horizontal=TRUE)
    gens_level <- gcombobox(c("RANDOM Level", 1:20), container=group_gens_char)
  group_gens_about <- gframe("About", container=group_generators_, horizontal=FALSE, expand=TRUE)
    gens_name <- gedit("Name (blank for random)", container=group_gens_about)
    gens_sex <- gradio(c("RANDOM", "Male", "Female"), container=group_gens_about, horizontal=TRUE)
    gens_stats <- gcombobox(c("RANDOM Stats", "Weak", "Below Average", "Average", "Above Average", "Powerful"), container=group_gens_about, horizontal=TRUE)
  generators_btn <- gbutton(
    text      = "GENERATE!",
    container = group_generators,
    handler   = gennpc
  )

  # -----------------------
  # Last bit
  # -----------------------

  font(generators_btn) <- font(adventure_btn) <- font(roll_simple_btn) <- font(loot_btn) <- font(names_btn) <- font(town_btn) <- font(tavern_btn) <- font(dungeon_btn) <- c(color="orange", style="bold")
  status_bar <- gstatusbar(" :3", container = win)

}

# roll_a_guy
# roll_a_guy(race, class, alignment='rand', age='rand', htwt='rand', sex='rand', name='randomnameasdfqwert', level=1, l1hpreroll='max', stats_opt=c(1,3,2,4,6,5), method=stats_roller$phb[[1]])



#gmhelper()







#.onLoad <- function(libname, pkgname){
#  package_data <- data(package="gmhelper")
#  data(list=package_data$results[, 3], package="gmhelper")
#}

.onLoad <- function(libname, pkgname)
{
  # --------------------------------#
  # Load all the necessary datasets #
  # --------------------------------#

  # Name files
  .__barbarian_ <<- read.csv(system.file("extdata/names/barbarian_.csv", package="gmhelper"), sep=";")
  .__dwarf_ <<- read.csv(system.file("extdata/names/dwarf_.csv", package="gmhelper"), sep=";")
  .__elf_ <<- read.csv(system.file("extdata/names/elf_.csv", package="gmhelper"), sep="\t")
  .__human_ <<- read.csv(system.file("extdata/names/human_.csv", package="gmhelper"), sep=";")
  .__gnome_ <<- read.csv(system.file("extdata/names/gnome_.csv", package="gmhelper"), sep="\t")
  .__halfling_ <<- read.csv(system.file("extdata/names/halfling_.csv", package="gmhelper"), sep="\t")
  .__human_ <<- read.csv(system.file("extdata/names/human_.csv", package="gmhelper"), sep="\t")


  .__troll_ <<- read.csv(system.file("extdata/names/troll_.csv", package="gmhelper"), sep="\t")
  .__orc_ <<- read.csv(system.file("extdata/names/orc_.csv", package="gmhelper"), sep="\t")



  .__tavern_ <<- read.csv(system.file("extdata/names/tavern_.csv", package="gmhelper"), sep="\t")
  .__town_ <<- read.csv(system.file("extdata/names/town_.csv", package="gmhelper"), sep="\t")

  # "Findables"
  .__drink_ <<- read.csv(system.file("extdata/findable/drink_.csv", package="gmhelper"), sep=";")
  .__food_ <<- read.csv(system.file("extdata/findable/food_.csv", package="gmhelper"), sep=";")
  .__gems_ <<- read.csv(system.file("extdata/findable/gems_.csv", package="gmhelper"), sep=";")
  .__jewelry_ <<- read.csv(system.file("extdata/findable/jewelry_.csv", package="gmhelper"), sep=";")
  .__mundane_ <<- read.csv(system.file("extdata/findable/mundane_.csv", package="gmhelper"), sep=";")
  .__potions_ <<- read.csv(system.file("extdata/findable/potions_.csv", package="gmhelper"), sep=";")
  .__pot_desc_ <<- read.csv(system.file("extdata/findable/potions_desc_.csv", package="gmhelper"), sep="\t")
  
  # "Buyables"
  .__item_furnishings <<- read.csv(system.file("extdata/buyable/item_furnishings.csv", package="gmhelper"), sep=";")
  .__item_clothing <<- read.csv(system.file("extdata/buyable/item_clothing.csv", package="gmhelper"), sep=";")
  .__item_provisions <<- read.csv(system.file("extdata/buyable/item_provisions.csv", package="gmhelper"), sep="\t")
  
  
  options("guiToolkit"="RGtk2")
}

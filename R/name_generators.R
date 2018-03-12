# Meta
char_name = function(race, first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  if (race=='rand')
    race = sample(c("human", "dwarf", "elf", "halfling", "gnome"), 1)

  eval(parse(text=paste(race, "_name(", first, ",", middle, ",", last,")", sep="")))
}



sample_name = function(dataset, col)
{
  sample(dataset[which(dataset[, col] != ""), col], 1)
}



# internal meta
name_maker = function(race, sex, last)
{
  if (sex=='m' || sex=='M')
    col = 2
  else
    col = 3
  
  if (last==FALSE)
    fl = 1
  else
  {
    col = 5
    fl = 4
  }
  
  stem1 = sample_name(race, fl)
  stem2 = sample_name(race, col)
  name = paste(stem1, stem2, sep="")

  name
}



name_barbarian = function()
{
  barbarian = gmh_racenames$barbarian
  
  f = sample_name(barbarian, 1)
  l = sample_name(barbarian, 2)
  
  fixstr( paste(f, l, sep="") )
}



# Human --- names are mixes of real saxon, viking, norman, welsh, and gaelic historical names
name_human = function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  human = gmh_racenames$human
  
  if (sex=='m' || sex=='M')
    col = 1
  else
    col = 2
  
  if (first)
    f = sample_name(human, col)
  else
    f = ""
  
  if (middle)
    m = sample_name(human, col)
  else
    m = ""
  
  if (last)
    l = name_maker(human, sex, TRUE)
  else
    l = ""
  
  fixstr(paste(f, m, l))
}



name_dwarf = function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  dwarf = gmh_racenames$dwarf
  
  if (first)
    f = name_maker(dwarf, sex, FALSE)
  else
    f = ""
  
  if (middle)
    m = name_maker(dwarf, sex, FALSE)
  else
    m = ""
  
  if (last)
    l = name_maker(dwarf, sex, TRUE)
  else
    l = ""
  
  fixstr( paste( f, m, l ) )
}



name_elf = function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  elf = gmh_racenames$elf
  
  if (first)
    f = name_maker(elf, sex, FALSE)
  else
    f = ""
  
  if (middle)
    m = name_maker(elf, sex, FALSE)
  else
    m = ""
  
  if (last)
    l = name_maker(elf, sex, TRUE)
  else
    l = ""
  
  fixstr( paste( f, m, l ) )
}



name_halfling = function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  halfling = gmh_racenames$halfling
  
  if (first)
    f = name_maker(halfling, sex, FALSE)
  else
    f = ""
  
  if (middle)
    m = name_maker(halfling, sex, FALSE)
  else
    m = ""
  
  if (last)
    l = name_maker(halfling, sex, TRUE)
  else
    l = ""
  
  fixstr( paste( f, m, l ) )
}



name_gnome = function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  gnome = gmh_racenames$gnome
  
  if (first)
    f = name_maker(gnome, sex, FALSE)
  else
    f = ""
  
  if (middle)
    m = name_maker(gnome, sex, FALSE)
  else
    m = ""
  
  if (last)
    l = name_maker(gnome, sex, TRUE)
  else
    l = ""
  
  fixstr( paste( f, m, l ) )
}



name_orc = function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  orc = gmh_racenames$orc
  
  if (first)
    f = name_maker(orc, sex, FALSE)
  else
    f = ""
  
  if (middle)
    m = name_maker(orc, sex, FALSE)
  else
    m = ""
  
  if (last)
    l = name_barbarian()
  else
    l = ""
  
  fixstr( paste( f, m, l ) )
}



name_troll = function(first=TRUE, middle=FALSE, last=TRUE, sex='m')
{
  troll = gmh_racenames$troll
  
  if (first)
    f = name_maker(troll, sex, FALSE)
  else
    f = ""
  
  if (middle)
    m = name_maker(troll, sex, FALSE)
  else
    m = ""
  
  if (last)
    l = name_barbarian()
  else
    l = ""
  
  fixstr( paste( f, m, l ) )
}

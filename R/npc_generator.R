# str dex con int wis cha

# class block lookup
.__whichclass <- function(class)
{
  if (class %in% c('warrior', 'fighter', 'paladin', 'ranger'))
    return('warrior')
  else if (class %in% c('wizard', 'mage'))
    return('wizard')
  else if (class %in% c('priest', 'cleric', 'druid'))
    return('priest')
  else if (class %in% c('rogue', 'thief', 'bard'))
    return('rogue')
  else
    stop("Class not supported at this time")
}


# ---------------------------------
# Rollers
# ---------------------------------

# die roller
die <- function(n=1, d=4, ..., con=10)
{
  roll <- sample(1:d, size=n, replace=TRUE)
  
  if (con == 20)
    roll[which(roll==1)] <- 2
  if (con %in% 21:22)
    roll[which(roll==1 | roll==2)] <- 3
  if (con %in% 23:25)
    roll[which(roll==1 | roll==2 | roll==3)] <- 4
  
  return(sum(roll))
}

# roll for stats 
.__roll_stats <- function(method, opt)
{
  roll <- method()
  stats <- numeric(6)
  for (i in 1:6){
    stats[which(opt==i)] <- roll[length(roll)]
    roll <- roll[1:(length(roll)-1)]
  }
  names(stats) <- c("str", "dex", "con", "int", "wis", "cha")
  return(stats)
}

.__exceptional_str <- function(str, race)
{
  if (race!='halfling' && str==18)
    return(die(1, 100))
  else
    return(NA)
}

# determine hp
.__roll_hp <- function(con, level, class, reroll=1)
{
  con_hp_adj <- c(-3,-2,-2,-1,-1,-1,0,0,0,0,0,0,0,0,1,2,3,4,5,5,6,6,6,7,7)
  class <- .__whichclass(class)
  
  if (class=='warrior'){
    hd <- 10
    hd_num <- c(1:9, rep(9, 11))
    hd_plus <- c(rep(0, 9), 3*1:11)
    hp_adj <- con_hp_adj[con]
  } else hp_adj <- min(2, con_hp_adj[con])
  
  if (class=='rogue'){
    hd <- 6
    hd_num <- c(1:10, rep(10, 10))
    hd_plus <- c(rep(0, 10), 2*1:10)
  } else if (class=='wizard'){
    hd <- 4
    hd_num <- c(1:10, rep(10, 10))
    hd_plus <- c(rep(0, 10), 1:10)
  } else if (class=='priest'){
    hd <- 8
    hd_num <- c(1:9, rep(9, 11))
    hd_plus <- c(rep(0, 9), 2*1:11)
  }
  
  # max hd at first level
  if (reroll=='max'){
    hp <- hd
    reroll <- 0
  } 
  
  # reroll if hp=1 at first level
  while(reroll>0){
    hp <- die(n=1, d=hd[1], con=con) 
    
    if (hp==1){
      reroll <- reroll-1
      hp <- die(n=1, d=hd[1], con=con)
    }
    else
      reroll <- 0
  }
  
  if (level>1)
    hp <- hp + die(n=hd_num[level-1], d=hd, con=con)
  
  hp <- hp + hd_plus[level] + hp_adj
  
  # reroll if hp <= 0
  if (hp < 1)
    hp <- .__roll_hp(con, level, class, reroll=1)
  
  return(hp)
}

# determine thac0
.__thac0 <- function(level, class)
{
  class <- .__whichclass(class)
  if (class=='warrior')
    prog <- c(1,1)
  else if (class=='priest')
    prog <- c(2,3)
  else if (class=='rogue')
    prog <- c(1,2)
  else if (class=='wizard')
    prog <- c(1,3)
  else
    stop("Class not supported at this time")
  
  return( 20-floor((level-1)/prog[2])*prog[1] )
}

# racial ability adjustment
.__race_ab_adj <- function(race, ab)
{
  if (race=='dwarf'){
    adj <- c(0,0,1,0,0,-1)
  } else if (race=='elf'){
    adj <- c(0,1,-1,0,0,0)
  } else if (race=='gnome'){
    adj <- c(0,0,0,1,-1,0)
  } else if (race=='half-elf'){
    adj <- rep(0, 6)
  } else if (race=='halfling'){
    adj <- c(-1,1,0,0,0,0)
  } else if (race=='human'){
    adj <- rep(0, 6)
  } else
    stop("Race not supported at this time")
  
  return(ab + adj)
}

# age
.__rand_age <- function(race)
{
  if (race %in% c('human', 'half-elf'))
    age <- round(rchisq(1, 3)*3+16)
  else if (race=='halfling')
    age <- round(rchisq(1, 5)*3+20)
  else if (race=='elf')
    age <- round(rchisq(1, 20)*5+50)
  else if (race=='dwarf')
    age <- round(rchisq(1, 10)*3+30)
  else if (race=='gnome')
    age <- round(rchisq(1, 5)*5 + 50 + sample(1:10, 1))
  
  return(age)
}

# height and weight
.__rand_ht_wt <- function(race, str, sex)
{
  if (race=='dwarf'){
    if (sex=='f')
      mean <- c(44, 11)
    else
      mean <- c(48, 14)
  } else if (race=='elf'){
    if (sex=='f')
      mean <- c(54, 3)
    else
      mean <- c(58, 7)
  } else if (race=='gnome'){
    if (sex=='f')
      mean <- c(40, 5)
    else
      mean <- c(44, 7)
  } else if (race=='half-elf'){
    if (sex=='f')
      mean <- c(62, 3)
    else
      mean <- c(67, 7)
  } else if (race=='halfling'){
    if (sex=='f')
      mean <- c(33, 2.5)
    else
      mean <- c(35, 4)
  } else if (race=='human'){
    if (sex=='f')
      mean <- c(63, 4)
    else
      mean <- c(69, 8)
  } else
    stop("Race not supported at this time")

    ht <- rnorm(1, mean[1], 3)
    # based on bmi, with random * str modifier
    wt <- round(21/703.06958*ht^2 + rnorm(1, mean[2], 1) *(str-7.5))
    names(wt) <- NULL

  return(list(ht=floor(ht), wt=floor(wt)))
}

.__rand_alignment <- function(.)
{
  dim1 <- c("lawful", "", "chaotic")
  dim2 <- c("good", "neutral", "evil")

  return( fixstr(paste(sample(dim1, 1), sample(dim2, 1) )) )
}

# ---------------------------------
# Restrictions
# ---------------------------------

# check racial ab reqs
.__check_race_ab <- function(race, ab)
{
  if (race=='dwarf'){
    min <- c(8, 3, 11, 3, 3, 3)
    max <- c(18, 17, 18, 18, 18, 17)
  } else if (race=='elf'){
    min <- c(3, 6, 7, 8, 3, 8)
    max <- rep(18, 6)
  } else if (race=='gnome'){
    min <- c(6, 3, 8, 6, 3, 3)
    max <- rep(18, 6)
  } else if (race=='half-elf'){
    min <- c(3, 6, 6, 4, 3, 3)
    max <- rep(18, 6)
  } else if (race=='halfling'){
    min <- c(7, 7, 10, 6, 3, 3)
    max <- c(18, 18, 18, 18, 17, 18)
  } else if (race=='human'){
    min <- rep(1, 6)
    max <- rep(Inf, 6)
  } else 
      stop("Race not supported at this time")
  
  return( all(ab>=min) && all(ab<=max) )
}

# class ability requirements
.__check_class_ab <- function(class, ab)
{
  if (class=='fighter')
    min <- c(9, 1, 1, 1, 1, 1)
  else if (class=='paladin')
    min <- c(12, 1, 9, 1, 13, 17)
  else if (class=='ranger')
    min <- c(13, 13, 14, 1, 14, 1)
  else if (class=='wizard')
    min <- c(1, 1, 1, 9, 1, 1)
  else if (class=='cleric')
    min <- c(1, 1, 1, 1, 9, 1)
  else if (class=='druid')
    min <- c(1, 1, 1, 1, 12, 15)
  else if (class=='thief')
    min <- c(1, 9, 1, 1, 1, 1)
  else if (class=='bard')
    min <- c(1, 12, 1, 13, 1, 15)
  else 
      stop("Class not supported at this time")
  
  return( all(ab>=min) )
}

# check racial class restriction
.__class_allowed_race <- function(class, race)
{
  if (class=='paladin' && race!='human')
    return(FALSE)
  else if (class=='ranger' && !(race %in% c('human', 'elf', 'half-elf')))
    return(FALSE)
  else if (class=='wizard' && !(race %in% c('human', 'elf', 'half-elf')))
    return(FALSE)
  else if (class=='druid' && !(race %in% c('human', 'half-elf')))
    return(FALSE)
  else if (class=='bard' && !(race %in% c('human', 'half-elf')))
    return(FALSE)
  else
    return(TRUE)
}

# check racial class level restrictions
.__class_race_level <- function(class, race, level)
{
  
}

# check alignment
.__check_alignment <- function(class)
{
  
}


# ---------------------------------
# Saving throws
# ---------------------------------

.__saving_throws <- function(class, level, race, con)
{
  class <- .__whichclass(class)
  if (class=='priest'){
    ppdm <- c(10, 10, 10, 9, 9, 9, 7, 7, 7, 6, 6, 6, 5, 5, 5, 4, 4, 4, rep(2, 12))
    rsw <- c(14, 14, 14, 13, 13, 13, 11, 11, 11, 10, 10, 10, 9, 9, 9, 8, 8, 8, rep(6, 12))
    pply <- c(13, 13, 13, 12, 12, 12, 10, 10, 10, 9, 9, 9, 8, 8, 8, 7, 7, 7, rep(5, 12))
    bw <- c(16, 16, 16, 15, 15, 15, 13, 13, 13, 12, 12, 12, 11, 11, 11, 10, 10, 10, rep(8, 12))
    spl <- c(15, 15, 15, 14, 14, 14, 12, 12, 12, 11, 11, 11, 10, 10, 10, 9, 9, 9, rep(7, 12))
  } else if (class=='rogue'){
    ppdm <- c(13, 13, 13, 13, 12, 12, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9, 9, rep(8, 10))
    rsw <- c(14, 14, 14, 14, 12, 12, 12, 12, 10, 10, 10, 10, 8, 8, 8, 8, 6, 6, 6, 6, rep(4, 10))
    pply <- c(12, 12, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9, 9, 8, 8, 8, 8, rep(7, 10))
    bw <- c(16, 16, 16, 16, 15, 15, 15, 15, 14, 14, 14, 14, 13, 13, 13, 13, 12, 12, 12, 12, rep(11, 10))
    spl <- c(15, 15, 15, 15, 13, 13, 13, 13, 11, 11, 11, 11, 9, 9, 9, 9, 7, 7, 7, 7, rep(5, 10))
  } else if (class=='warrior'){
    ppdm <- c(14, 14, 13, 13, 11, 11, 10, 10, 8, 8, 7, 7, 5, 5, 4, 4, rep(3, 14))
    rsw <- c(16, 16, 15, 15, 13, 13, 12, 12, 10, 10, 9, 9, 7, 7, 6, 6, rep(5, 14))
    pply <- c(15, 15, 14, 14, 12, 12, 11, 11, 9, 9, 8, 8, 6, 6, 5, 5, rep(4, 14))
    bw <- c(17, 17, 16, 16, 13, 13, 12, 12, 9, 9, 8, 8, 5, 5, 4, 4, rep(4, 14))
    spl <- c(17, 17, 16, 16, 14, 14, 13, 13, 11, 11, 10, 10, 8, 8, 7, 7, rep(6, 14))
  } else if (class=='wizard'){
    ppdm <- c(14, 14, 14, 14, 14, 13, 13, 13, 13, 13, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, rep(8, 10))
    rsw <- c(11, 11, 11, 11, 11, 9, 9, 9, 9, 9, 7, 7, 7, 7, 7, 5, 5, 5, 5, 5, rep(3, 10))
    pply <- c(13, 13, 13, 13, 13, 11, 11, 11, 11, 11, 9, 9, 9, 9, 9, 7, 7, 7, 7, 7, rep(5, 10))
    bw <- c(15, 15, 15, 15, 15, 13, 13, 13, 13, 13, 11, 11, 11, 11, 11, 9, 9, 9, 9, 9, rep(7, 10))
    spl <- c(12, 12, 12, 12, 12, 10, 10, 10, 10, 10, 8, 8, 8, 8, 8, 6, 6, 6, 6, 6, rep(4, 10))
  } else
      stop("Class not supported at this time")
  
  svt <- c(ppdm=ppdm[level], rsw=rsw[level], pply=pply[level], bw=bw[level], spl=spl[level])
  
  if (race=='dwarf'){
    adj <- c(rep(0, 3), rep(1, 3), rep(2, 4), rep(3, 3), rep(4, 4), rep(5, 8))
    svt <- svt - adj[con]
  }
  
  return(svt)
}

# ---------------------------------
# Proficiencies
# ---------------------------------



# ---------------------------------
# Money/Equipment
# ---------------------------------

.__money <- function(level, class){
  class <- .__whichclass(class)
  
  if (class=='warrior')
    gp <- 10*die(5, 4)
  else if (class=='wizard')
    gp <- 10*(die(1, 4)+1)
  else if (class=='rogue')
    gp <- 10*die(2, 6)
  else if (class=='priest')
    gp <- 10*die(3, 6)
  
  if (level>1)
    gp <- gp + round(rnorm(1, 600, 100)*5^(level/5))
  
  return( gp )
}

# ---------------------------------
# Class rollers
# ---------------------------------

roll_a_guy <- function(race, class, alignment='rand', age='rand', htwt='rand', sex='rand', name='randomnameasdfqwert', level=1, l1hpreroll='max', strength=3)
{
  if (race=='rand')
    race <- sample(c("human", "half-elf", "elf", "dwarf", "halfling", "gnome"), 1)
  
  if (class=='rand')
    class <- sample(c("fighter", "paladin", "ranger", "wizard", "priest", "cleric", "druid", "rogue", "thief", "bard"), 1)
  
  # initial checks
  if(!.__class_allowed_race(class, race))
    stop(paste("A", race, "is not permitted to be a", class))

  # name/age/sex
  if (race=='half-elf')
    namerace <- sample(c('human', 'elf'), 1)
  else
    namerace <- race
  
  if (name=='randomnameasdfqwert')
    name <- char_name(race=namerace, middle=TRUE, sex=sex)
  
  if (alignment=='rand')
    alignment <- .__rand_alignment()

  if (age=='rand')
    age <- .__rand_age(race)
  
  if (sex=='rand')
    sex <- sample(c('m', 'f'), 1)
  
  # stats
  if (strength==1 || strength==2)
    method <- stats_roller$phb[[2]]
  if (strength==3)
    method <- stats_roller$phb[[1]]
  if (strength==4 || strength==5)
    method <- stats_roller$other[[1]]
  
  if (class=='fighter')
    stats_opt <- c(1,3,2,4,6,5)
  else if (class=='paladin')
    stats_opt <- c(3,5,4,6,2,1)
  else if (class=='ranger')
    stats_opt <- c(3,4,1,5,2,6)
  else if (class=='wizard')
    stats_opt <- c(6,3,4,1,2,5)
  else if (class=='cleric')
    stats_opt <- c(5,4,2,3,1,6)
  else if (class=='druid')
    stats_opt <- c(6,4,3,5,2,1)
  else if (class=='thief')
    stats_opt <- c(4,1,2,3,6,5)
  else if (class=='bard')
    stats_opt <- c(6,2,4,3,5,1)
  
  stats <- .__race_ab_adj(race, .__roll_stats(method, stats_opt))
  while(!.__check_race_ab(race, stats) || !.__check_class_ab(class, stats))
    stats <- .__race_ab_adj(race, .__roll_stats(method, stats_opt))
  
  if (strength==1)
    stats <- stats-1
  if (strength==5)
    stats <- stats+1
  
  pct_str <- .__exceptional_str(stats[1], race)
  
  # height/weight
  if (htwt=='rand')
    htwt <- .__rand_ht_wt(race, stats[1], sex)
  
  # hp
  hp <- .__roll_hp(con=stats[3], level=level, class=class, reroll=l1hpreroll)
  
  # thac0
  thac0 <- .__thac0(level, class)

  # saving throws
  svt <- .__saving_throws(class, level, race, stats[3])

  # money
  gp <- .__money(level, class)

  return(list( list(name=name, race=race, class=class, age=age, sex=sex), htwt, level=level, stats=stats, hp=hp, thac0=thac0, svt=svt, gp=gp ))
}



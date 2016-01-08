gencrit <- function(type)
{
  if (type == "Melee")
  {
    crits <- c(
      # x2
      "SMASHING! Multiply damage by 2.",
      # damage twice
      "DOUBLE SLASH! Roll damage twice and add the results.",
      # max damage
      "FURIOUS ANGER! Deal max weapon damage.",
      # -2 AC
      "UNSTOPPABLE! You blow smashes through your enemy's armor; they receive a -2 penalty to AC. Deal normal damage.",
      # fatigued (-2 to STR and DEX)
      "IMPRESSIVE DISPLAY! Your opponent becomes fatigued (-2 to STR and DEX). Reroll weapon damage if dealing less than half of max weapon damage.",
      # exploding damage
      "I'LL FUCKING KILL YOU! Exploding weapon dice. Re-roll if the first die roll is a 1."
    )
  }
  else if (type == "Ranged")
  {
    crits <- c(
      # +2 to hit next attack
      "SHOOT FROM THE HIP! Receive a +2 to hit on your next attack roll this combat.",
      # attack again
      "BLOT OUT THE SUN! After dealing damage, immediately attack another target.",
      # additional 1d6 bleeding
      "SHOT THROUGH THE HEART (AND YOU'RE TO BLAME)! Deal an additional 1d6 bleeding damage.",
      # +2 to DEX 1d4 rounds
      "LUCKY SHOT! Gain +2 to DEX for the next 1d4 rounds.",
      # opponent can't move
      "DON'T MOVE! Your missile hits the target in the ankle, leaving them unable to move. (if non-humanoid, figure something else out).",
      # poisoned
      "DUDE, WHO LACED THIS?! Your missile was poisoned. Enemy must make a saving throw or become poisoned."
    )
  }
  
  sample(crits, size=1)
}



genmiss <- function(type)
{
  if (type == "Melee")
  {
    misses <- c(
      # Blind 1d4 rounds
      "POCKET SAND! Before you can attack, your opponent throws sand in your eyes. You are blinded for 1d4 rounds.",
      # go prone
      "WHOOPSIE DAISY! You slip and fall prone to the ground.",
      # drop weapon
      "BUTTER FINGERS! You swing wildly and your weapon slips out of your hands and falls to the ground.",
      # hit yourself
      "STOP HITTING YOURSELF! You miss the target entirely and your weapon slips and hits you in the leg. Roll for damage against yourself.",
      # enemy makes full move away
      "LOOK OVER THERE! Your opponet points behind you and you turn to look.  He runs a full move away from you in the direction of choice of the DM.",
      # dazed and -2 penalty to AC 
      "WHAT DOES IT ALL MEAN?! You suffer an existential crisis of the DM's discretion. You are dazed until your next turn and suffer a -2 penalty to AC."
    )
  }
  else if (type == "Ranged")
  {
    misses <- c(
      # weapon jam
      "DAMN, A JAM! Your weapon jams/breaks and requires a full round to fix it.",
      # enemy ducks behind cover
      "TOO SLOW! You miss, and your target ducks behind the nearest cover (possibly another person).",
      # attack nearest
      "DUCK SEASON! You become confused and attack the nearest friendly target in your line of sight.",
      # panicked
      "OH SHIT! You become panicked and run away at full move. You regain your composure at the beginning of your next turn.",
      # flat footed (no dex bonus)
      "NOT FIT FOR DUTY! You are flat footed for 1d6 rounds (no dex bonus to AC).",
      # entangled (-2 attack, -4 dex)
      "VICTIM OF CIRCUMSTANCE! You become entangled in your surroundings (-2 attack and -4 DEX) until the end of your next turn."
    )
  }
  
  sample(misses, size=1)
}

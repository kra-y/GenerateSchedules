# BuildBuilding2.R
library(dplyr)
library(stats)
library(gtools)

IfZeroNada <- function(char, arg) {
  if (arg <= 0) {
    out <- NULL
  } else {
    out <- rep(char, arg)
  }
  return(out)
}

random_scramble <- function(word) {
  # split the word into individual letters
  letters <- strsplit(word, "")[[1]]
  
  # use sample to shuffle the letters
  shuffled <- sample(letters)
  
  # join the shuffled letters back into a string
  return(paste(shuffled, collapse = ""))
}

DurationCode <- function(Ablocks, Bblocks, Cblocks) {
  DURATION_CODE <- paste0(
    c(IfZeroNada("A",Ablocks),IfZeroNada("B",Bblocks),IfZeroNada("C",Cblocks)),
    collapse = "" # Modified this line to collapse the output
  )
  return(DURATION_CODE)
}

RoomBlockPermutations<-function(Block){
  
  
  perm<-random_scramble(Block)
  return(perm) 
  
}




naiveschedule <- data.frame("A" = rep(seq(0,16),70),
                            "B" = rep(seq(0,9),119),
                            "C" = rep(seq(0,6),170))

naiveschedule0 <- naiveschedule %>% 
  mutate(FULLHOURS = A*.833333+B*1.5+C*2) %>% 
  filter(FULLHOURS <= 13.5 & FULLHOURS >= 7) %>%
  rowwise() %>% # Modified this line to use rowwise function
  mutate(DURATION_CODE = DurationCode(Ablocks = A, Bblocks = B, Cblocks = C),
         RANDOM_ORDER = RoomBlockPermutations(DURATION_CODE),
         )




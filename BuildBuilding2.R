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

DurationCode <- function(Ablocks, Bblocks, Cblocks) {
  DURATION_CODE <- paste0(
    c(IfZeroNada("A",Ablocks),IfZeroNada("B",Bblocks),IfZeroNada("C",Cblocks)),
    collapse = "" # Modified this line to collapse the output
  )
  return(DURATION_CODE)
}

RoomBlockPermutations<-function(Block){
  
  
  characters<- strsplit(as.character(Block),split = "")
  numberofclasses<-length(characters[[1]])
  other<-length(characters[[1]])
  # return(is.numeric(numberofclasses))
  perms<-permutations(n=numberofclasses,r=other,characters[[1]],set = F)
  return(perms) 
  
}


naiveschedule <- data.frame("A" = rep(seq(0,16),70),
                            "B" = rep(seq(0,9),119),
                            "C" = rep(seq(0,6),170))

naiveschedule0 <- naiveschedule %>% 
  mutate(FULLHOURS = A*.833333+B*1.5+C*2) %>% 
  filter(FULLHOURS <= 13.5 & FULLHOURS >= 7) %>%
  rowwise() %>% # Modified this line to use rowwise function
  mutate(DURATION_CODE = DurationCode(Ablocks = A, Bblocks = B, Cblocks = C))



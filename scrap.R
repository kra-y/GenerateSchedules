library(dplyr)
# scrap
#A room can be either occupied or unoccupied
# 730 - 8 - 830 - 9 - 930 -10 -1030 -11 - 1130 - 12 - 1230 - 1 - 130 - 2 - 230 - 3 - 330 - 4 - 430 - 5 - 530
# |-------------------------------------------------------------------------------------------------------|
# |                                                                                                       |
# |                                                                                                       |
# |                                                                                                       |
# |                                                                                                       |
# |                                                                                                       |
# so a class can either be an hour or an hour and a half.
# We need to create all the permutations or combinations

# what are all the ways we can split a 10 hour day into blocks of 1 hour (A) or 1.5 hrs(B)
# use algebra X + (1.5)Y = 13.5,
 # two equations
# two unknowns (the number of A classes, the number of B classes)
# A + B*1.5 = 13.5
# B = (13.5-A)/1.5

# ax+by = 10 where A is the number of 1 hour long classes, and b is the number of 1.5 hr classes
 # a <= 13.5
# b <= 9
# a = 1 , b = 6
# a = 2 , 

B_Classes<- function(aclasses) {
  y<-floor((13.5-aclasses)/1.5)
  return(as.integer(y))
}

DurationCode<-function(A,B){
  DURATION_CODE<- paste0(c(rep("A",Ablocks),rep("B",Bblocks)),collapse = "")
  return(DURATION_CODE)
}

flavorlist<-data.frame(x = seq(1,13.5))
flavorlist$y<-B_Classes(flavorlist$x)
DurationCode(3,4)
flavorlist$DURATION_CODE<-DurationCode(flavorlist$x,flavorlist$y)






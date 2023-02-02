#Let's create a building with a small number of rooms and a schedule of the time blocks for each of those rooms

# let's first create some functions we'll need to populate our Building dataset
samplefromschedule<-function(string){
  choices<-RoomBlockPermutations(string)
  index<-sample(nrow(choices),size = 1)
  choice<-apply(choices, 1, paste, collapse = "")[index]
  return(choice)
}

B_Classes<- function(aclasses) {
  y<-floor((10.5-aclasses)/1.5)
  return(as.integer(y))
}

DurationCode<-function(Ablocks,Bblocks){
  DURATION_CODE<- paste0(c(rep("A",Ablocks),rep("B",Bblocks)),collapse = "")
  return(DURATION_CODE)
}

RoomBlockPermutations<-function(Block){
  
  
  characters<- strsplit(as.character(Block),split = "")
  numberofclasses<-length(characters[[1]])
  other<-length(characters[[1]])
  # return(is.numeric(numberofclasses))
  perms<-permutations(n=numberofclasses,r=other,characters[[1]],set = F)
  return(perms) .
  
}

#So let's build some data to debug some of what's going on.

BuildingA = data.frame("classroom" = as.character(seq(1:10)))
# we need to give each of these classrooms we need to select a number (less than 13) of 1 hour classes.
for(i in 1:length(BuildingA$classroom)){
  BuildingA$ACLASSES[i]<-sample(1:10,1,replace = T)
  BuildingA$BCLASSES[i]<-B_Classes(BuildingA$ACLASSES[i])
  BuildingA$BLOCKS[i]<-DurationCode(BuildingA$ACLASSES[i],BuildingA$BCLASSES[i])
  BuildingA$BLOCKORDER[i]<-samplefromschedule(BuildingA$BLOCKS[i])
}

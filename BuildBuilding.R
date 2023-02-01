# let's first create one building on a Campus
BuildingA = data.frame("classroom" = as.character(seq(1:10)))

for(i in 1:length(BuildingA$classroom)){
  
  mat<-RoomBlockPermutations(BuildingA$BLOCKCODE[i])
  BuildingA$RandPerm[i]<-samplefromschedule(mat)
}

samplefromschedule<-function(string){
  choices<-RoomBlockPermutations(string)
  choice<-choices[sample(nrow(choices),size = 1),]
  return(choice)
}

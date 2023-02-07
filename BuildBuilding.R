#Let's create a building with a small number of rooms and a schedule of the time blocks for each of those rooms
library(stats)
library(tidyverse)




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
  return(perms) 
  
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

#Make all the math enrollments indexed by student ID and give 
numbers = c(0980,1010,1200,1220,1230,1260)
probs0<-rexp(6,1)
probs<-probs0/sum(probs0)
mathprobs<-sort(probs,decreasing = T)
course_names<-paste("MATH",numbers,sep = "")

math_enrollments<-sample(course_names,2000,replace = T,prob = mathprobs)


student_ID<-paste0("S",1000:2999)


studentcourses<-data.frame("STUDENT_ID" = student_ID,
                           "COURSE" = math_enrollments,
                           "NUMBER_OF_COURSES" = sample(1:4,2000,replace = T))
# here's the students and all their math enrollments
#now let's give them a selection of classes to choose from populating more of the easier classes than the harder ones
#we'll do this by assigning a probability column to the course catalogue data frame.
coursecatalog<-expand.grid(c("ENG","HIST","PHYS","ENGR"),c("0980","1000","1010","1200","1220","1230","1260","2000","2550"))
coursecatalog$COURSECODE<-paste0(coursecatalog$Var1,coursecatalog$Var2)
df<-coursecatalog%>%
  select(COURSECODE)%>%
  bind_rows(math_offerings)%>%
  distinct(COURSECODE)%>%
  mutate(COURSE_NUMBER = as.numeric(gsub('[A-Z]',"",COURSECODE)),
         DEPARTMENT = gsub('[0:9]','',as.character(COURSECODE)),
         DURATION_CODE = ifelse(COURSE_NUMBER>1200,"B","A"))

#WHAT THE FUCK IS GOING ON HERE WITH GSUB

# we can populate the room schedules for building A by sampling from the course catalog, but we first need to add the math classes to the course catalog
# the only problem is we have 2000 math 980 students they all need to have at least math 980  on their schedules,
# but also have 2 or 3 other classes along with math 980
df1<-BuildingA%>%
  # group_by(classroom)%>%
  mutate(B1 = as.list(strsplit(BLOCKORDER,''))[[1]][1],
         B2 = as.list(strsplit(BLOCKORDER,''))[[1]][2],
         B3 = as.list(strsplit(BLOCKORDER,''))[[1]][3],
         B4 = as.list(strsplit(BLOCKORDER,''))[[1]][4],
         B5 = as.list(strsplit(BLOCKORDER,''))[[1]][5],
         B6 = as.list(strsplit(BLOCKORDER,''))[[1]][6],
         B7 = as.list(strsplit(BLOCKORDER,''))[[1]][7],
         B8 = as.list(strsplit(BLOCKORDER,''))[[1]][8],
         B9 = as.list(strsplit(BLOCKORDER,''))[[1]][9],
         B10 = as.list(strsplit(BLOCKORDER,''))[[1]][10])
sum(df1[,6:15]=="A",na.rm = T)
sum(df1[,6:15]=="B",na.rm = T)
# ok so we have sum(df1[,6:15]=="A",na.rm = T) A Blocks
# and sum(df1[,6:15]=="",na.rm = T) B Blocks to put courses in. we will sample from the course catalog but there needs
# to be some kind of rule to populate the easier courses with more students

# now we need to build out the schedules of all the students
studentcourses$COURSECODE1<-studentcourses$COURSE
studentcourses$COURSECODE2<-ifelse(studentcourses$NUMBER_OF_COURSES==1,)





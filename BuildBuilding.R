#Let's create a building with a small number of rooms and a schedule of the time blocks for each of those rooms
library(stats)
library(tidyverse)
library(dplyr)
library(gtools)




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
         B2 = as.list(strsplit(BLOCKORDER,''))[[2]][1],
         B3 = as.list(strsplit(BLOCKORDER,''))[[3]][1],
         B4 = as.list(strsplit(BLOCKORDER,''))[[4]][1],
         B5 = as.list(strsplit(BLOCKORDER,''))[[5]][1],
         B6 = as.list(strsplit(BLOCKORDER,''))[[6]][1],
         B7 = as.list(strsplit(BLOCKORDER,''))[[7]][1],
         B8 = as.list(strsplit(BLOCKORDER,''))[[8]][1],
         B9 = as.list(strsplit(BLOCKORDER,''))[[9]][1],
         B10 = as.list(strsplit(BLOCKORDER,''))[[10]][1])
sum(df1[,6:15]=="A",na.rm = T)
sum(df1[,6:15]=="B",na.rm = T)
# ok so we have sum(df1[,6:15]=="A",na.rm = T) A Blocks
# and sum(df1[,6:15]=="",na.rm = T) B Blocks to put courses in. we will sample from the course catalog but there needs
# to be some kind of rule to populate the easier courses with more students

# now we need to build out the schedules of all the students
studentcourses$COURSECODE1<-studentcourses$COURSE
studentcourses$COURSECODE2<-ifelse(studentcourses$NUMBER_OF_COURSES==1,)

# 
# 
# CHATGPT help

# Define the building's opening and closing times
start_time <- as.POSIXct("2023-03-01 08:00:00")
end_time <- as.POSIXct("2023-03-01 18:00:00")

# Define the time slots for each course
course_slots <- seq(from = start_time, to = end_time, by = 30*60) # every 30 minutes

# Define the course names and durations
course_names <- c("Intro to Programming", "Data Structures", "Algorithms")
course_durations <- c(2, 3, 2) # in hours


######

# Create a matrix to represent the schedule
num_slots <- length(course_slots)
num_courses <- length(course_names)
schedule <- matrix(NA, nrow = num_slots, ncol = num_courses)

# Fill the schedule with course names
for (i in 1:num_courses) {
  course_duration_slots <- course_durations[i] * 60 / 30
  available_slots <- which(is.na(schedule[, i]))
  num_available_slots <- length(available_slots)
  if (num_available_slots < course_duration_slots) {
    stop("Not enough available slots for course ", course_names[i])
  }
  start_slot <- sample(available_slots, 1)
  end_slot <- start_slot + course_duration_slots - 1
  schedule[start_slot:end_slot, i] <- course_names[i]
}

# Print the schedule
colnames(schedule) <- course_names
rownames(schedule) <- format(course_slots, "%H:%M")
print(schedule)

# Define the start and end times for each course block
start_times <- c("08:00", "09:30", "11:00", "12:30", "14:00", "15:30", "17:00", "18:30", "20:00")
end_times <- c("09:20", "10:50", "12:20", "13:50", "15:20", "16:50", "18:20", "19:50", "21:20")

# Create a data frame to store the course schedule for BuildingA
BuildingA_schedule <- data.frame(
  classroom = BuildingA$classroom,
  block_order = BuildingA$BLOCKORDER,
  start_time = character(length(nrow(BuildingA))),
  end_time = character(length(nrow(BuildingA))),
  course_name = character(length(nrow(BuildingA)))
)

# Loop through each row of BuildingA_schedule and fill in the start time, end time, and course name
for (i in seq_len(nrow(BuildingA_schedule))) {
  # Get the block order for the current row
  block_order <- BuildingA_schedule$block_order[i]
  
  # Get the start and end times for the current block
  start_time <- start_times[block_order]
  end_time <- end_times[block_order]
  
  # Get the course name for the current classroom and block
  classroom <- BuildingA_schedule$classroom[i]
  course_name <- paste0("Course", block_order, "-", classroom)
  
  # Update the course schedule data frame with the start time, end time, and course name
  BuildingA_schedule$start_time[i] <- start_time
  BuildingA_schedule$end_time[i] <- end_time
  BuildingA_schedule$course_name[i] <- course_name
}

# Print the course schedule for BuildingA
BuildingA_schedule

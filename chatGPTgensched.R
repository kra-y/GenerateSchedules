#KEITH"S CONTRIBUTION: creating th time block order data frame
# BuildBuilding2.R




#Make Course Catalog
library(stats)
library(dplyr)

# Create a data frame to store the course catalogue
# Create the course names
math_courses <- paste0("MATH", c("0980","1010","1200","1220","1260"))
eng_courses <- paste0("ENG", c("0980","1000","1010","1200","1220","1230","1260","2000","2550"))
hist_courses <- paste0("HIST", c("0980","1000","1010","1200","1220","1230","1260","2000","2550"))
phys_courses <- paste0("PHYS", c("0980","1000","1010","1200","1220","1230","1260","2000","2550"))
engr_courses <- paste0("ENGR", c("0980","1000","1010","1200","1220","1230","1260","2000","2550"))

# Combine the course names into a single vector
all_courses <- c(math_courses, eng_courses, hist_courses, phys_courses, engr_courses)

# Create a data frame with the course information
course_catalogue <- data.frame(
  course_name = all_courses,
  department = c(rep("MATH", length(math_courses)), rep("ENG", length(eng_courses)),
                 rep("HIST", length(hist_courses)), rep("PHYS", length(phys_courses)),
                 rep("ENGR", length(engr_courses))),
  level = c(c("0980","1010","1200","1220","1260"),rep(c("0980", "1000", "1010", "1200", "1220", "1230", "1260", "2000", "2550"),4)),
  stringsAsFactors = FALSE
)

# Print the first few rows of the course_catalogue data frame
head(course_catalogue)

course_catalogue <- course_catalogue %>%
  mutate(credit_hours = ifelse(level %in% c("0980","1000", "1010"), 2,
                               ifelse(level %in% c("1200", "1220", "2000"), 3,
                                      ifelse(level %in% c("1230","1260","2550"), 4, NA))),
         block_code = ifelse(level %in% c("0980","1000", "1010"), "A",
                             ifelse(level %in% c("1200", "1220", "2000"), "B",
                                    ifelse(level %in% c("1230","1260","2550"), "C", NA))),
         hours_per_session = ifelse(credit_hours==2,.83333,
                                    ifelse(credit_hours==3,1.5,
                                           ifelse(credit_hours==4,2,NA))))




##########################################################################################################################

# Now that we have the course catalogue, I need to build a building
# and schedule the courses that meet in the room
# throughout the day the day goes from 7:30AM and the last class gets out at 9PM.
# there about 65% of the schedule should consist of low level courses



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



selectfromcodes<-function(block){
  set<-course_catalogue%>%
    filter(block_code==block)
  index<-sample(nrow(set),1)
  course<-set[index,"course_name"]
  return(course)
}

classorder<-function(block_order){
  class_order<-{}
  for(i in 1:length(block_order)){
    # randomly assign a course whose block fits the block parameter
    class_order[i]<-selectfromcodes(block_order[i])
  }
  return(class_order)
    
  
}



# ChatGPT
# Create a data frame to store the room schedules
# Define the time slots for the day
room_schedules <- data.frame()

# Define time blocks
time_blocks <- seq(from = as.POSIXct("2023-03-01 07:00:00"), to = as.POSIXct("2023-03-01 21:00:00"), by = "10 min")

# Loop over each room
for (room in 1:10) {
  block_order<-sample(naiveschedule0$RANDOM_ORDER,10,replace = T)
  # convert the string "ABBCBA" to the right format
  block_order = strsplit(block_order,"")[[1]]
  #then create the class order
  class_order<-{}
  for(i in 1:length(block_order)){
    # randomly assign a course whose block fits the block parameter
    class_order[i]<-selectfromcodes(block_order[i])
    
  }
  # Loop over each time block
  for (i in 1:(length(block_order)) {
    # Filter course catalogue for courses that fit within this time block
    room_courses <- course_catalogue %>%
      filter(credit_hours %in% c(2, 3, 4),
             hours_per_session == as.numeric(difftime(time_blocks[i + 1], time_blocks[i], units = "mins"))) %>%
      group_by(course_name, level) %>%
      slice(1)
    # If there are courses that fit within this time block
    if (nrow(room_courses) > 0) {
      # Determine course start and end times
      start_time <- time_blocks[i]
      end_time <- time_blocks[i + 1]
      # Add room and section information
      room_courses$room <- room
      room_courses$section <- 0:(nrow(room_courses)-1)
      # Add start and end times
      room_courses$start_time <- start_time
      room_courses$end_time <- end_time
      # Add to room schedule
      room_schedules <- bind_rows(room_schedules, room_courses)
    }
  }
}

# Select columns and print first few rows
room_schedules <- room_schedules %>%
  select(course_name, level, room, start_time, end_time) %>%
  arrange(room, start_time)








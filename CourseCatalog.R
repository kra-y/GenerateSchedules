

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
                               ifelse(level %in% c("1200", "1220", "1260", "2000", "2550"), 3,
                                      ifelse(level == "1230", 4, NA))),
         hours_per_session = ifelse(credit_hours==2,.83333,
                                    ifelse(credit_hours==3,1.5,
                                           ifelse(credit_hours==4,2,NA))))




##########################################################################################################################

# Now that we have the course catalogue, I need to build a building
# and schedule the courses that meet in the room
# throughout the day the day goes from 7:30AM and the last class gets out at 9PM.
# there about 65% of the schedule should consist of low level courses


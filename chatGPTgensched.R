# ChatGPT
# Create a data frame to store the room schedules
# Define the time slots for the day
timeslots <- seq(from = as.POSIXct("2023-02-27 07:00:00"), to = as.POSIXct("2023-02-27 21:00:00"), by = 30*60)

# Create a data frame to store the schedule
schedule <- data.frame(course_name = character(0), start_time = character(0), end_time = character(0))

# Loop through each course in the course catalogue
for (i in seq_along(course_catalogue$course_name)) {
  
  # Get the course information
  course_name <- course_catalogue$course_name[i]
  credit_hours <- course_catalogue$credit_hours[i]
  hours_per_session <- course_catalogue$hours_per_session[i]
  
  # Find the first available time slot for the course
  available_slots <- which(diff(c(0, schedule$end_time)) >= hours_per_session*60*60)
  if (length(available_slots) == 0) {
    stop("No available time slots for course ", course_name)
  }
  start_slot <- available_slots[1]
  start_time <- format(timeslots[start_slot], "%Y-%m-%d %H:%M:%S")
  end_time <- format(timeslots[start_slot + hours_per_session*2 - 1], "%Y-%m-%d %H:%M:%S")
  
  # Add the course to the schedule
  schedule <- rbind(schedule, data.frame(course_name = course_name, start_time = start_time, end_time = end_time))
}

# Print the schedule
print(schedule)


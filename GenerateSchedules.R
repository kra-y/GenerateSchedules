library(tidyverse)
library(lubridate)
library(gtools)
rm(list = ls())
#generate unique IDs for students PIDM
pidm<-as.character(sprintf("%06d",0:999999))
#generate the list of semester codes TERM CODE
term.codes <- factor(sapply(2000:2019, paste0, c(20, 30, 40)), ordered = TRUE)
term.code.strings<-term.codes
levels(term.code.strings) <- 
  paste0(ifelse(substr(levels(term.codes), 5, 6) == "20", "Spring ", 
                ifelse(substr(levels(term.codes), 5, 6) == "30", "Summer ", "Fall ")),
         substr(levels(term.codes), 1, 4)) # recode term codes into pretty string string values
#generate all the subjects at the college: 
course.subjects<-c("Math", "English", "History","Writing","Engineering","Chemistry","Biology","Business","Physics","Economics","Music","Art")
#COURSE NUMBER
course.numbers<-sprintf("%04d",seq(900,2050,by = 15))
#CREATE LIST OF ALL THE COURSES AND RANDOMLY ASSIGN THE NUMBER OF CREDITS THEY'RE WORTH
course.attributes<-expand.grid("course.subject" = course.subjects,
                              "course.number" = course.numbers)%>%
  mutate(course = paste0(course.subject,"-",course.number),# specify the course name
         credits = sample(c(3,4),length(course.subjects)*length(course.numbers),replace = T),
         days.per.week = sample(c(2,3),length(course.subjects)*length(course.numbers),replace = T),
         n.sections = ceiling(2*rchisq(length(course.numbers)*length(course.subjects),0.0001,3)+1))%>%#randomly assign the number of sections, assume the distribution of number of sections offered is skewed right
  dplyr::select(course,
                credits,
                n.sections,
                days.per.week)%>%
  mutate(class.length = ifelse(credits==3 & days.per.week==2,
                               "90 minutes",
                               ifelse(credits==3 & days.per.week==3,
                                      "60 minutes",
                                      ifelse(credits==4 & days.per.week==2,
                                             "2 hours","80 minutes"))))

course.catalog<-course.attributes[rep(row.names(course.attributes),course.attributes$n.sections),c(1,5)]%>%
  group_by(course)%>%
  mutate(section = sprintf("%03d",row_number(course)))
#BEGIN TIME , END TIME, AND MEETING DAY 1
begin.times.1<-seq(from = as.POSIXct("2019-09-22 07:00:00 MDT"),
                  to = as.POSIXct("2019-09-22 21:00:00 MDT"),
                  by = "15 min")
end.times.1<-seq(from = as.POSIXct("2019-09-22 07:50:00 MDT"),
                to = as.POSIXct("2019-09-22 21:50:00 MDT"),
                by = "15 min")
meeting.days.1<-c("M","T","W","H","F")
#BEGIN TIME , END TIME, AND MEETING DAY 2
begin.times.2<-seq(from = as.POSIXct("2019-09-22 07:00:00 MDT"),
                  to = as.POSIXct("2019-09-22 21:00:00 MDT"),
                  by = "15 min")
end.times.2<-seq(from = as.POSIXct("2019-09-22 07:50:00 MDT"),
                to = as.POSIXct("2019-09-22 21:50:00 MDT"),
                by = "15 min")
meeting.days.2<-c("M","T","W","H","F")
#BEGIN TIME , END TIME, AND MEETING DAY 3
begin.times.3<-seq(from = as.POSIXct("2019-09-22 07:00:00 MDT"),
                  to = as.POSIXct("2019-09-22 21:00:00 MDT"),
                  by = "15 min")
end.times.3<-seq(from = as.POSIXct("2019-09-22 07:50:00 MDT"),
                to = as.POSIXct("2019-09-22 21:50:00 MDT"),
                by = "15 min")
meeting.days.3<-c("M","T","W","H","F")
#CAMPUS
campus<-LETTERS[1:10]

#DATA SET OF STUDENTS AND THEIR START AND END SEMESTER ENROLLMENTS

GenerateSchedules<-function(n.students = 10000){
d0<-data.frame("pidm" = sample(pidm,n.students,replace = F))
for (i in 1:nrow(d0)){
  start.term.index<-sample(1:length(term.codes),1)
  n.semesters.enrolled<-sample(1:(length(term.codes)-start.term.index),1)
  d0$first.term[i]<-as.character(term.codes[start.term.index])
  d0$last.term[i]<-as.character(term.codes[start.term.index+n.semesters.enrolled])
  d0$n.semesters.enrolled[i]<-n.semesters.enrolled
}
#DATA SET THAT COPIES EACH STUDENT THE NUMBER OF SEMESTERS THEY WERE ENROLLED SAMPLES NUMBER OF COURSES
d1<-d0[rep(row.names(d0), d0$n.semesters.enrolled),1:3]%>%
  mutate(first.term = factor(first.term, levels = levels(term.codes)),
         last.term = factor(last.term, levels = levels(term.codes)))%>%
  group_by(pidm)%>%
  mutate(term.code = term.codes[as.numeric(factor(first.term,levels = levels(term.codes)))+row_number(pidm)-1],
         n.student.term.courses = sample(c(1,2,3,4),length(pidm),replace = T))

#1 ROW PER STUDENT PER TERM PER COURSE SAMPLES COURSES
d2<-d1[rep(row.names(d1), d1$n.student.term.courses),c(1,4)]%>%
  group_by(pidm,term.code)%>%
  mutate(course.enrolled = sample(course.attributes$course,length(term.code),replace = T))%>%
  left_join(course.attributes,by = c("course.enrolled" = "course"))%>%
  ungroup()%>%
  mutate(class.length = ifelse(credits==3 & days.per.week==2,
                               90,
                               ifelse(credits==3 & days.per.week==3,
                                      60,
                                      ifelse(credits==4 & days.per.week==2,
                                             120,80))),
         meeting.day.1 = ifelse(days.per.week=="2","T",
                                "M"),
         meeting.day.2 = ifelse(days.per.week=="2","H",
                                "W"),
         meeting.day.3 = ifelse(days.per.week=="2","",
                                "Fr"))%>%
  ungroup()%>%
  dplyr::select(-c(n.sections,
                   days.per.week))%>%
  mutate(begin.time.1 = begin.times.1[sample(1:57,n(),replace = T)],
         end.time.1 = begin.time.1+minutes(class.length),
         begin.time.2 = begin.time.1,
         end.time.2 = end.time.1,
         begin.time.3 = begin.time.1,
         end.time.3 = end.time.1,
         campus = sample(campus,n(),replace = T))
d2
}

write.table(GenerateSchedules(20000),"/Users/karaneo/Documents/R/shinyapps/Scheduler/d.csv",sep = ',')





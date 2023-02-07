#Make Course Catalog
library(stats)
library(dplyr)
numbers = c("0980","1010","1200","1220","1260")
probs0<-rexp(6,1)
probs<-probs0/sum(probs0)
mathprobs<-sort(probs,decreasing = T)
course_names<-paste("MATH",numbers,sep = "")
course_names

math_enrollments<-data.frame(COURSECODE = sample(course_names,2000,replace = T,prob = mathprobs))
math_offerings<-math_enrollments%>%
  group_by(COURSECODE)%>%
  summarise(COURSECODE = first(COURSECODE))

student_ID<-paste0("S",1000:2999)


studentcourses<-data.frame("STUDENT_ID" = student_ID,
                 "COURSE" = math_enrollments,
                 "NUMBER_OF_COURSES" = sample(1:4,2000,replace = T))
coursecatalog<-expand.grid(c("ENG","HIST","PHYS","ENGR"),c("0980","1000","1010","1200","1220","1230","1260","2000","2550"))
coursecatalog$COURSECODE<-paste0(coursecatalog$Var1,coursecatalog$Var2)
coursecatalog<-coursecatalog%>%
  select(COURSECODE)%>%
  bind_rows(math_offerings)%>%
  mutate(COURSE_NUMBER = as.numeric(gsub('[A-Z]',"",COURSECODE)),
         DURATION_CODE = ifelse(COURSE_NUMBER>1200,"B","A"))


#we need to bring all this code over to a main script so it can all run in one organized place.
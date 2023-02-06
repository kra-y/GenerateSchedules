#Make Course Catalog
library(stats)
numbers = c(0980,1010,1200,1220,1230,1260)
probs0<-rexp(6,1)
probs<-probs0/sum(probs0)
mathprobs<-sort(probs,decreasing = T)
course_names<-paste("MATH",numbers,sep = "")

math_enrollments<-sample(course_names,2000,replace = T,prob = mathprobs)


student_ID<-paste0("S",1000:2999)


math<-data.frame("STUDENT_ID" = student_ID,
                 "COURSE" = math_enrollments)

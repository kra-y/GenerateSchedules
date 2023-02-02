#Make Course Catalog
library(stats)
numbers = c(980,1010,1200,1220,1230,1260)
probs0<-rexp(6,1)
probs<-probs0/sum(probs0)
mathprobs<-sort(probs,decreasing = T)
course_names<-paste("MATH",numbers,sep = "")

math_students<-sample(course_names,100,replace = T,prob = c(mathprobs))
x<-as.factor(math_students)
plot(x)

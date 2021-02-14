####### PACKAGES ######################################################################################################################################
list.of.packages<-c("dplyr","shiny","plotly","tables","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(shiny)
library(tidyverse)

library(plotly)
library(tables)
set.seed(922)
setwd("~/Documents/R/shinyapps/Scheduler")
d<-GenerateSchedules()%>%
  mutate(term.code.string = as.factor(paste0(ifelse(substr(term.code,5,6)=="20","Spring ",
                                                    ifelse(substr(term.code,5,6)=="30","Summer ","Fall ")),
                                             substr(term.code,1,4))))


####### Random Functions for Plots #######################################################################################################################
colfunc<-colorRampPalette(c("#00FBFB","#006363"))
min_year<-min(as.numeric(substr(d$term.code,1,4)))
max_year<-max(as.numeric(substr(d$term.code,1,4)))
semesters<-factor(sapply(min_year:max_year,paste0,c(20,30,40)),ordered = T)
levels(d$term.code.string)<-paste0(ifelse(substr(semesters,5,6)=="20","Spring ",
                                          ifelse(substr(semesters,5,6)=="30","Summer ","Fall ")),
                                   substr(semesters,1,4))
term.code.strings<-levels(d$term.code.string)
reason.courses = levels(as.factor(d$course.enrolled))
campuses = levels(as.factor(d$campus))

################### Scheduled Meetings Data ######################
posix.to.numtime<-function(x){
  as.numeric(gsub(":","",substr(x,12,16)))
}


myScheduledMeetingsData<-function(termcodestring,campusstring,reason.course){
  pre<-d%>%
    filter(term.code.string==termcodestring)%>%
    filter(campus==campusstring)%>%
    filter(course.enrolled==reason.course)%>%
    filter((is.na(begin.time.2) &
              is.na(end.time.2) &
              is.na(meeting.day.2)&
              is.na(begin.time.3) &
              is.na(end.time.3) &
              is.na(meeting.day.3))|
             paste0(meeting.day.2,begin.time.2,end.time.2)!=paste0(meeting.day.3,begin.time.3,end.time.3))%>% # should be left with only columns that have different values reported in the 9 different columns inclulded
    mutate(meeting.day.1 = as.character(meeting.day.1),
           begin.time.1 = posix.to.numtime(begin.time.1),
           end.time.1 = posix.to.numtime(end.time.1),
           meeting.day.2 = as.character(meeting.day.2),
           begin.time.2 = posix.to.numtime(begin.time.2),
           end.time.2 = posix.to.numtime(end.time.2),
           meeting.day.3 = as.character(meeting.day.3),
           begin.time.3 = posix.to.numtime(begin.time.3),
           end.time.3 = posix.to.numtime(end.time.3))%>%
    gather(key = "meeting.day.KEY",value = "meeting.day",c('meeting.day.1','meeting.day.2','meeting.day.3'))%>%
    filter(!is.na(meeting.day))%>%
    gather(key = "begin.time.KEY",value = "begin.time",c("begin.time.1","begin.time.2","begin.time.3"))%>%
    filter(!is.na(begin.time))%>%
    gather(key = "end.time.KEY",value = "END_TIME",c("end.time.1","end.time.2","end.time.3"))%>%
    filter(!is.na(END_TIME))%>%
    mutate(meeting.day = gsub('c("','',meeting.day,fixed = T),
           meeting.day = gsub('"','',meeting.day,fixed = T),
           meeting.day = gsub(')','',meeting.day,fixed = T),
           meeting.day = gsub(' ','',meeting.day,fixed = T))%>%
    separate_rows(meeting.day,sep = ',')%>%
    dplyr::select(-contains("KEY"))%>%
    mutate(weekday = factor(ifelse(meeting.day=="M","Monday",
                                   ifelse(meeting.day=="T","Tuesday",
                                          ifelse(meeting.day=="W","Wednesday",
                                                 ifelse(meeting.day == "H","Thursday",
                                                        ifelse(meeting.day=="Fr","Friday",
                                                               ifelse(meeting.day=="S","Saturday","Sunday")))))),
                            levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                            ordered = T))%>%
    dplyr::select(term.code,
                  campus:weekday)%>%
    group_by(weekday,
             begin.time,
             END_TIME)
  
  time.block<-factor(as.character(substr(gsub(":","",substr(seq(
    from=as.POSIXct("2012-1-1 6:00", tz="UTC"),
    to=as.POSIXct("2012-1-1 23:55", tz="UTC"),
    by="5 min"
  ),12,18)),1,4)),ordered = T) 
  
  data<-data.frame("Block" = as.numeric(as.character(time.block)),
                   "Sunday" = rep(NA,length(time.block)),
                   "Monday" = rep(NA,length(time.block)),
                   "Tuesday" = rep(NA,length(time.block)),
                   "Wednesday" = rep(NA,length(time.block)),
                   "Thursday" = rep(NA,length(time.block)),
                   "Friday" = rep(NA,length(time.block)),
                   "Saturday" = rep(NA,length(time.block)))
  
  for(i in 1:length(time.block)){
    data[i,"Sunday"]<-sum(pre$weekday=="Sunday" & pre$begin.time<=data$Block[i] & pre$END_TIME>=data$Block[i])
    data[i,"Monday"]<-sum(pre$weekday=="Monday" & pre$begin.time<=data$Block[i] & pre$END_TIME>=data$Block[i])
    data[i,"Tuesday"]<-sum(pre$weekday=="Tuesday" & pre$begin.time<=data$Block[i] & pre$END_TIME>=data$Block[i])
    data[i,"Wednesday"]<-sum(pre$weekday=="Wednesday" & pre$begin.time<=data$Block[i] & pre$END_TIME>=data$Block[i])
    data[i,"Thursday"]<-sum(pre$weekday=="Thursday" & pre$begin.time<=data$Block[i] & pre$END_TIME>=data$Block[i])
    data[i,"Friday"]<-sum(pre$weekday=="Friday" & pre$begin.time<=data$Block[i] & pre$END_TIME>=data$Block[i])
    data[i,"Saturday"]<-sum(pre$weekday=="Saturday" & pre$begin.time<=data$Block[i] & pre$END_TIME>=data$Block[i])
  }
  
  d1<-data%>%
    gather(key = "weekday", value = count,Sunday:Saturday)%>%
    mutate(Block = ifelse(nchar(Block)==3,paste0("0",Block),Block),
           Block = factor(Block,levels = time.block,ordered = T),
           weekday = factor(weekday,levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),ordered = T))%>%
    filter(weekday!="Sunday")
  
  d1$term<-termcodestring
  d1$campus<-campusstring
  d1$course<-reason.course
  d1
} 
myScheduledMeetingsPlot<-function(pd){
  term<-pd$term[1]
  campus<-pd$campus[1]
  course<-pd$course[1]
  ggplot(data = pd,aes(x = weekday,y = reorder(Block,desc(Block))))+
    geom_tile(aes(fill = count))+
    scale_fill_gradient(guide = guide_legend(title = paste0(course, " students\non Campus ",campus,"\nthroughout the day")),colfunc)+
    xlab(NULL)+
    ggtitle("Enrollments by Time Block",subtitle = paste0("Course Schedules for students taking\n",course," on the ",campus," campus\nDuring the ",
                                                          term," semester"))+
    scale_y_discrete(name = "Time of Day", breaks = factor(as.character(substr(gsub(":","",substr(seq(
      from=as.POSIXct("2012-1-1 6:00", tz="UTC"),
      to=as.POSIXct("2012-1-1 23:55", tz="UTC"),
      by="1 hour"),12,18)),1,4)),ordered = T),labels = factor(as.character(substr(gsub(":","",substr(seq(
        from=as.POSIXct("2012-1-1 6:00", tz="UTC"),
        to=as.POSIXct("2012-1-1 23:55", tz="UTC"),
        by="1 hour"),12,18)),1,4)),ordered = T))+
    theme(axis.ticks = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          plot.title = element_text(size = 20),
          axis.text.x = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(), 
          plot.background = element_blank(),
          plot.subtitle = element_text(size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14))
}
####### UI ##############################################################################################################################################

ui <- fluidPage(
  titlePanel(title = "Student Schedule Loads",
             windowTitle = "Generated Data"), 
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: #003865;
                    font-weight:100;
                    text-align:center;
                    vertical-align: middle;
                    }")),
    tags$link(rel="shortcut icon", href="favicon.ico")),
  # # Application title (Informative, but brief)
  fluidRow(
    column(
      3,
      wellPanel(
        h3("Filter"),
        selectizeInput("TERM_CODE_STRING",
                       label = "Term Code: ",
                       choices = term.code.strings,
                       selected = "Spring 2019"),
        selectizeInput("COURSE_STRING",
                       label = "Course: ",
                       choices = reason.courses,
                       selected = "Math-1230"),
        selectizeInput("CAMPUS",
                       label = "Campus: ",
                       choices = campuses,
                       selected = "A"),
        fluidRow(uiOutput("contact")) 
      )#wellPanel
    ),#column
    column(9,
           #      mainPanel(
           tabsetPanel(
             type = "tabs",
             tabPanel("Schedules",
                      fluidRow(
                        plotOutput(
                          "SCHEDULE",
                          width = "100%")) #plotOutput
             ),#tabPanel
             tabPanel("Info",
                      h4("This app displays the concentration of students enrolled in courses at the selected campus in the selected semester. For example, if the user selects Spring 2018 and campus B, the app overlays a transparency of every student's blocked-off weekly schedule at that campus. In other words, the lighter a block of time is on the plot, the busier the campus is.")
             )#tabPanel
           )#tabsetPanel
           #        )#mainPanel
    )#column
    
  )#fluidRow
  
    )#fluidPage

####### SERVER ##############################################################################################################  
server <- function(input, output,session) {
  
  schedule_prep<-reactive({
    req(input$TERM_CODE_STRING)
    req(input$COURSE_STRING)
    req(input$CAMPUS)
    myScheduledMeetingsData(input$TERM_CODE_STRING,input$CAMPUS,input$COURSE_STRING)
  })
  output$SCHEDULE<-renderPlot({
    validate(
      need(sum(schedule_prep()$count)>0,paste0("\nNo ",schedule_prep()$course[1], " students taking classes\non Campus ",
                                               schedule_prep()$campus[1],
                                               "\nin ",
                                               schedule_prep()$term[1]))
    )
    myScheduledMeetingsPlot(schedule_prep())
  },height = 600, width = 900)
  # Fill in your contact information
  url<- a("Keith R. Araneo-Yowell", href="mailto:keitharaneo@gmail.com")
  output$contact <- renderUI({
    tagList("For questions or improvement suggestions, please email ", url)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



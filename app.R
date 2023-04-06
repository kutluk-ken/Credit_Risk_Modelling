library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(party)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)
library(dashboardthemes)
library(tidyverse)
#urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
#install.packages(urlPackage, repos=NULL, type="source") 
library(randomForest)

data <- read.csv('Credit.csv', header = TRUE)
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Dashboard"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Input", tabName = "Input", icon = icon("dashboard")),
                            menuItem("Visualizations", icon = icon("th")),
                                     menuSubItem("BarPlot", tabName = "BarPlot")
                                     #menuSubItem("ScatterPlot", tabName = "ScatterPlot"),
                                     #menuSubItem("Histogram", tabName = "Histogram"),
                                     #menuSubItem("Model", tabName = "Model")),
                            #menuItem("Results", tabName = "Results", icon = icon("th"))
                        )
                    ),
                    dashboardBody(
                        shinyDashboardThemes(
                          theme = "purple_gradient"),
                        tabItems(
                          tabItem(
                                tabName = "Input",
                                h2("Input Details"),
                                fluidRow(
                                    column(4,
                                           selectInput("status",
                                                label = h4("Status"),
                                                choices = list("no checking account" = 1,
                                                               "... < 0 DM" =  2,
                                                               "0<= ... < 200 DM" =  3,
                                                               "... >= 200 DM / salary for at least 1 year" = 4),
                                                selected = 1 
                                                ),
                                            selectInput("duration",
                                                       label = h4("Duration"),
                                                       choices = list("0 - 12 months" = 1,
                                                                      "13 - 18 months" = 2,
                                                                      "19 - 24 months" =3,
                                                                      "> 24 months" = 4),
                                                       selected = 1
                                                ),
                                            selectInput("credit_history",
                                                       label = h4("Credit_history"),
                                                       choices = list("delay in paying off in the past" = 0,
                                                                      "critical account/other credits elsewhere" = 1,
                                                                      "no credits taken/all credits paid back duly" = 2,
                                                                      "existing credits paid back duly till now" = 3,
                                                                      "all credits at this bank paid back duly" = 4
                                                                      ),
                                                       selected = 0 
                                           ),
                                           selectInput("purpose",
                                                       label = h4("Purpose"),
                                                       choices = list("others" = 0,
                                                                      "car (new)" = 1,
                                                                      "car (used)" =2,
                                                                      "furniture/equipment" = 3,
                                                                      "radio/television" = 4,
                                                                      "domestic appliances" = 5,
                                                                      "repairs" = 6,
                                                                      "education" = 7,
                                                                      "vacation" = 8,
                                                                      "retraining" = 9,
                                                                      "business" = 10),
                                                       
                                                       selected = 1
                                           ),
                                           selectInput("amount",
                                                       label = h4("Amount"),
                                                       choices = list("Unknow: 0 - 1,000" = 1,
                                                                      "Unknow: 1,001 - 5,000" = 2,
                                                                      "Unknow: 5,001 - 10,000" =3,
                                                                      "Unknow: > 10,000" = 4),
                                                       selected = 1
                                           ),
                                           selectInput("savings",
                                                       label = h4("Savings"),
                                                       choices = list("unknown/no savings account" = 1,
                                                                      "... <  100 DM" = 2,
                                                                      "100 <= ... <  500 DM" =3,
                                                                      "500 <= ... < 1000 DM" = 4,
                                                                      "... >= 1000 DM" = 5),
                                                       selected = 1 
                                           ),
                                           selectInput("employment_duration",
                                                       label = h4("Employment_duration"),
                                                       choices = list("unemployed" = 1,
                                                                      "< 1 yr" = 2,
                                                                      "1 <= ... < 4 yrs" =3,
                                                                      "4 <= ... < 7 yrs" = 4,
                                                                      ">= 7 yrs" = 5),
                                                       selected = 1
                                           )
                                           
                                    ),
                                    column(4,
                                           selectInput("installment_rate",
                                                       label = h4("Installment_rate"),
                                                       choices = list(">= 35" = 1,
                                                                      "25 <= ... < 35" = 2,
                                                                      "20 <= ... < 25" =3,
                                                                      "< 20" = 4),
                                                       selected = 1
                                           ),
                                           
                                           selectInput("personal_status_sex",
                                                       label = h4("Personal_status_sex"),
                                                       choices = list("male : divorced/separated" = 1,
                                                                      "female : non-single or male : single" = 2,
                                                                      "male : married/widowed" =3,
                                                                      "female : single" = 4),
                                                       selected = 1
                                           ),
                                           selectInput("other_debtors",
                                                       label = h4("Other_debtors"),
                                                       choices = list("none" = 1,
                                                                      "co-applicant" = 2,
                                                                      "guarantor" = 3),
                                                       selected = 1 
                                           ),
                                           selectInput("present_residence",
                                                       label = h4("Present_residence"),
                                                       choices = list("< 1 yr" = 1,
                                                                      "1 <= ... < 4 yrs" = 2,
                                                                      "4 <= ... < 7 yrs" = 3,
                                                                      ">= 7 yrs" = 4),
                                                       selected = 1 
                                           ),
                                           selectInput("property",
                                                       label = h4("Property"),
                                                       choices = list("unknown / no property" = 1,
                                                                      "car or other" = 2,
                                                                      "building soc. savings agr./life insurance" = 3,
                                                                      "real estate" = 4),
                                                       selected = 1 
                                           ),
                                           selectInput("age",
                                                       label = h4("Age"),
                                                       choices = list("18 - 25" = 1,
                                                                      "26 - 40" = 2,
                                                                      "41 - 60" =3,
                                                                      "Over 60" = 4),
                                                       selected = 1 
                                           ),
                                           selectInput("other_installment_plans",
                                                       label = h4("Other_installment_plans"),
                                                       choices = list("bank" = 1,
                                                                      "stores" = 2,
                                                                      "none" =3),
                                                       selected = 1 
                                           )
                                           
                                           
                                    ),
                                    column(4,
                                           selectInput("housing",
                                                       label = h4("Housing"),
                                                       choices = list("for free" = 1,
                                                                      "rent" = 2,
                                                                      "own" =3),
                                                       selected = 1 
                                           ),
                                           selectInput("number_credits",
                                                       label = h4("Number_credits"),
                                                       choices = list("1" = 1,
                                                                      "2-3" = 2,
                                                                      "4-5" =3,
                                                                      ">= 6" =4),
                                                       selected = 1 
                                           ),
                                           selectInput("job",
                                                       label = h4("Job"),
                                                       choices = list("unemployed/unskilled - non-resident" = 1,
                                                                      "unskilled - resident" = 2,
                                                                      "skilled employee/official" =3,
                                                                      "manager/self-empl./highly qualif. employee" =4),
                                                       selected = 1 
                                           ),
                                           selectInput("people_liable",
                                                       label = h4("People_liable"),
                                                       choices = list("3 or more" = 1,
                                                                      "0 to 2" = 2),
                                                       selected = 1 
                                           ),
                                           selectInput("telephone",
                                                       label = h4("Telephone"),
                                                       choices = list("no" = 1,
                                                                      "yes (under customer name)" = 2),
                                                       selected = 1 
                                           ),
                                           selectInput("foreign_worker",
                                                       label = h4("Foreign_worker"),
                                                       choices = list("yes" = 1,
                                                                      "no" = 2),
                                                       selected = 1 
                                           )
                                           
                                    ),
                                    column(6, offset = 5,
                                           actionButton("action_Calc", label = "Evaluate" , size = "large")
                                    ),
                                    column(6, tabPanel("Output", 
                                                       p(h4("Credit prediction:")),
                                                       textOutput("decision", container = tags$h3)
                                    )
                                                       
                                    )
                                    
                                        )
                                    ),
                          tabItem(tabName = "BarPlot",
                                  h2("BarPlot"),
                                  fluidRow(
                                    column(4,
                                           selectInput("Cat",
                                                       label = h4("Categorical Variables"),
                                                       choices=colnames(data)
                                                       # choices = c("status"= 1,
                                                       #             "credit_history"=2,
                                                       #             "purpose" = 3 ,
                                                       #             "savings"=4,
                                                       #             "other_debtors"=5,
                                                       #             "housing"=6,
                                                       #             "other_installment_plans"=7,
                                                       #             "foreign_worker"=8),
                                                     )),
                                    hr(),
                                    helpText("Data: Credit Risk"),
                                    column(6,
                                           
                                           plotOutput("Plot1"))
 
                                    )
                                    
                                    
                                    # box(title = "Categorical Variables vs Credit risk",  width = 10,  background = "red", height=220,
                                    #     status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    #     selectInput(inputId = "one",label=h6(""),
                                    #                 choices = c("status"= 1,
                                    #                             "credit_history"=credit_history,
                                    #                             "purpose" = purpose ,
                                    #                             "savings"=savings,
                                    #                             "other_debtors"=other_debtors,
                                    #                             "housing"=housing,
                                    #                             "other_installment_plans"=other_installment_plans,
                                    #                             "foreign_worker"=foreign_worker),selected = status))
                                  )
                        
                                    
                                  
                                  )
                          )
                    

)




server <- function(input, output) {
  
  
  datainput = reactive({
    
    df1 <- read_csv("Credit.csv")
    dat <- read_csv("Credit.csv")
    
    nam_fahrmeirbook <- colnames(dat)
    
    
    nam_evtree <- c("status", "duration", "credit_history", "purpose", "amount", 
                    "savings", "employment_duration", "installment_rate",
                    "personal_status_sex", "other_debtors",
                    "present_residence", "property",
                    "age", "other_installment_plans",
                    "housing", "number_credits",
                    "job", "people_liable", "telephone", "foreign_worker",
                    "credit_risk")
    names(dat) <- nam_evtree
    
    ## make factors for all except the numeric variables
    ## make sure that even empty level of factor purpose = verw (dat[[4]]) is included
    for (i in setdiff(1:21, c(2,4,5,13)))
      dat[[i]] <- factor(dat[[i]])
    ## factor purpose
    dat[[4]] <- factor(dat[[4]], levels=as.character(0:10))
    
    ## assign level codes
    ## make intrinsically ordered factors into class ordered 
    levels(dat$credit_risk) <- c("bad", "good")
    levels(dat$status) = c("no checking account",
                           "... < 0 DM",
                           "0<= ... < 200 DM",
                           "... >= 200 DM / salary for at least 1 year")
    ## "critical account/other credits elsewhere" was
    ## "critical account/other credits existing (not at this bank)",
    levels(dat$credit_history) <- c(
      "delay in paying off in the past",
      "critical account/other credits elsewhere",
      "no credits taken/all credits paid back duly",
      "existing credits paid back duly till now",
      "all credits at this bank paid back duly")
    levels(dat$purpose) <- c(
      "others",
      "car (new)",
      "car (used)",
      "furniture/equipment",
      "radio/television",
      "domestic appliances",
      "repairs",
      "education", 
      "vacation",
      "retraining",
      "business")
    levels(dat$savings) <- c("unknown/no savings account",
                             "... <  100 DM", 
                             "100 <= ... <  500 DM",
                             "500 <= ... < 1000 DM", 
                             "... >= 1000 DM")
    levels(dat$employment_duration) <- 
      c(  "unemployed", 
          "< 1 yr", 
          "1 <= ... < 4 yrs",
          "4 <= ... < 7 yrs", 
          ">= 7 yrs")
    dat$installment_rate <- ordered(dat$installment_rate)
    levels(dat$installment_rate) <- c(">= 35", 
                                      "25 <= ... < 35",
                                      "20 <= ... < 25", 
                                      "< 20")
    levels(dat$other_debtors) <- c(
      "none",
      "co-applicant",
      "guarantor"
    )
    ## female : nonsingle was female : divorced/separated/married
    ##    widowed females are not mentioned in the code table
    levels(dat$personal_status_sex) <- c(
      "male : divorced/separated",
      "female : non-single or male : single",
      "male : married/widowed",
      "female : single")
    dat$present_residence <- ordered(dat$present_residence)
    levels(dat$present_residence) <- c("< 1 yr", 
                                       "1 <= ... < 4 yrs", 
                                       "4 <= ... < 7 yrs", 
                                       ">= 7 yrs")
    ## "building soc. savings agr./life insurance", 
    ##    was "building society savings agreement/life insurance"
    levels(dat$property) <- c(
      "unknown / no property",
      "car or other",
      "building soc. savings agr./life insurance", 
      "real estate"
    )
    levels(dat$other_installment_plans) <- c(
      "bank",
      "stores",
      "none"
    )
    levels(dat$housing) <- c("for free", "rent", "own")
    dat$number_credits <- ordered(dat$number_credits)
    levels(dat$number_credits) <- c("1", "2-3", "4-5", ">= 6")
    ## manager/self-empl./highly qualif. employee  was
    ##   management/self-employed/highly qualified employee/officer
    levels(dat$job) <- c(
      "unemployed/unskilled - non-resident",
      "unskilled - resident",
      "skilled employee/official",
      "manager/self-empl./highly qualif. employee"
    )
    levels(dat$people_liable) <- c("3 or more", "0 to 2")
    levels(dat$telephone) <- c("no", "yes (under customer name)")
    levels(dat$foreign_worker) <- c("yes", "no")
    dat
  })
  
  
  
  data <- read.csv('Credit.csv', header = TRUE)
  data$duration <- cut(data$duration, c(0,12,18,24,Inf), labels = c(1:4))
  data$amount <- cut(data$amount, c(0,1000,5000,10000,Inf), labels = c(1:4))
  data$age <- cut(data$age, c(18,25,40,60,Inf), labels = c(1:4))
  #credit[[4]] <- factor(credit[[4]], levels=as.character(0:10))
  #credit[[2]] <- factor(credit[[2]])
  #credit[[5]] <- factor(credit[[5]])
  #credit[[4]] <- factor(credit[[4]], levels=as.character(0:10))
  for(i in 1:21) 
    data[, i] <- as.factor(data[, i])
  
  set.seed(1005740600)
  #partition <- createDataPartition(data$credit_risk, p=0.7, list=FALSE)
  
  #train <- data[partition,]
  #set.seed(1005740600)
  RF <-  randomForest(credit_risk ~ ., data=data)
  inputDF <- data[-(2:1000),]

  # Return accept/reject decision
   values <- reactiveValues()
   observe({
     input$action_Calc
     # inputDF[1,]$status <- input$status
     # inputDF[1,]$duration <- input$duration
     # inputDF[1,]$credit_history <- input$credit_history
     # inputDF[1,]$purpose <- input$purpose
     # inputDF[1,]$amount <- input$amount
     # inputDF[1,]$savings <- input$savings
     # inputDF[1,]$employment_duration <- input$employment_duration
     # inputDF[1,]$installment_rate <- input$installment_rate
     # inputDF[1,]$other_debtors <- input$other_debtors
     # inputDF[1,]$personal_status_sex <- input$personal_status_sex
     # inputDF[1,]$present_residence <- input$present_residence
     # inputDF[1,]$property <- input$property
     # inputDF[1,]$age <- input$age
     # inputDF[1,]$other_installment_plans <- input$other_installment_plans
     # inputDF[1,]$housing <- input$housing
     # inputDF[1,]$number_credits <- input$number_credits
     # inputDF[1,]$job <- input$job
     # inputDF[1,]$people_liable <- input$people_liable
     # inputDF[1,]$telephone <- input$telephone
     # inputDF[1,]$foreign_worker <- input$foreign_worker

     inputDF[1,-21] <- isolate(list(input$status, input$duration,
                                input$credit_history, input$purpose,
                                input$amount, input$savings,
                                input$employment_duration, input$installment_rate,
                                input$personal_status_sex, input$other_debtors,
                                input$present_residence,input$property,input$age,
                                input$other_installment_plans,input$housing,
                                input$number_credits, input$job,
                                input$people_liable, input$telephone,
                                input$foreign_worker))
     decision <- predict(RF, inputDF, type = 'prob')
      if(decision[2] > decision[1]){
        decision = 'GOOD'
      } else {
        decision = 'BAD'
      }
     values$decision <- decision
   })
  
   # Display decision
  
   output$decision <- renderText({
     if(input$action_Calc == 0) ""
     else
       out <- paste(values$decision)
  
   })
   
    output$Plot1<-renderPlot({
      
      ggplot(datainput(),aes(input$Cat,..count..))+
      geom_bar(aes(fill = credit_risk), position = "dodge")+
      theme(axis.text.x = element_text(angle =45, hjust = 1))
      })
}

shinyApp(ui, server)


    

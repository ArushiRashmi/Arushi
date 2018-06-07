library(shiny)
library(ROCR)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(tree)
library(e1071)
library(xtable)
library(randomForest)
library(shinythemes)
shinyUI(
  fluidPage(theme=shinytheme("journal"),
            titlePanel(title=h3("Anomaly Intrusion Detection Model",align="center")),
            shinythemes::themeSelector(),
    
      sidebarLayout(
        sidebarPanel(
          actionButton("naive",label="Naive Bayes"),
          br(),
          actionButton("svm",label = "Support Vector Machine"),
          br(),
          actionButton("dtree",label = "Decision Tree"),
          br(),
          actionButton("mlr",label = "Multiple Logistic Regression"),
          br()
        
        ),
        mainPanel(
        tabsetPanel(type = "tab",
                     tabPanel("Summary",verbatimTextOutput("summary")),
                     tabPanel("Structure",verbatimTextOutput("structure")),
                     tabPanel("Plot",plotOutput("plot")),
                     tabPanel("Accuracy",verbatimTextOutput("accuracy"))
         
        )
      )
    )
  )
)

library(shiny)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(tree)
library(randomForest)
library(e1071)
library(xtable)
library(caret)
library(pROC)
library(ROCR)
library(nnet)
library(rminer)
library(ggplot2)
setwd("G:/apnaproject")
final=read.csv("final.csv",header = TRUE)
shinyServer(function(input,output)
  {
 set.seed(1300)
  observe(
    {
      r=as.numeric(0.55)
      ind=sample(2,nrow(final),replace = TRUE,prob=c(r,1-r))
      trainset=final[ind==1,]
      testset=final[ind==2,]
      observeEvent(input$naive,{
          NaiveB=naiveBayes(label ~.,data = trainset)
          nbpred=predict(NaiveB, testset,type='class')
          dbn=as.numeric(nbpred)
          xyq=prediction(dbn,testset$label)
          per=performance(xyq,"tpr","fpr")
          
            output$plot<-renderPlot(
              {plot(per)
      })
            output$summary<-renderPrint(
              {
                summary(final)
              }
            )
            output$structure<-renderPrint(
              {
                structure(final)
              }
            )
            output$accuracy<-renderPrint(
              {
                NaiveB=naiveBayes(label ~.,data = trainset)
                nbpred=predict(NaiveB, testset,type='class')
                confusionMatrix(nbpred,testset$label)
              }
            )
          
        })
        
      
      observeEvent(input$svm,{
        tcr=trainControl(method = "repeatedcv",number=5,repeats = 2)
        svml=train(label ~.,data=trainset,method="svmLinear",
                   trControl=tcr,preProcess=c("center","scale"),
                   tuneLength=5)
        tes=predict(svml,newdata = testset)
        dbn3=as.numeric(tes)
        xyq3=prediction(dbn3,testset$label)
        per3=performance(xyq3,"tpr","fpr")
        
        output$plot<-renderPlot(
          {plot(per3)
          })
        output$summary<-renderPrint(
          {
            summary(final)
          }
        )
        output$structure<-renderPrint(
          {
            structure(final)
          }
        )
        output$accuracy<-renderPrint(
          {
            svml=train(label ~.,data=trainset,method="svmLinear",
                       trControl=tcr,preProcess=c("center","scale"),
                       tuneLength=3)
            tes=predict(svml,newdata = testset)
            confusionMatrix(tes,testset$label)
          }
        )
        
      })
        
      
            observeEvent(input$dtree,{
        trc=trainControl(method="repeatedcv",number=4,repeats = 2)
        dtree=train(label~.,data=trainset,method="rpart",
                    parms=list(split="information"),
                    trControl=trc,tuneLength=3)
        testpred=predict(dtree,newdata = testset)
        dbn2=as.numeric(testpred)
        xyq2=prediction(dbn2,testset$label)
        per2=performance(xyq2,"tpr","fpr")
        
        output$plot<-renderPlot(
          {plot(per2)
          })
        output$summary<-renderPrint(
          {
            summary(final)
          }
        )
        output$structure<-renderPrint(
          {
            structure(final)
          }
        )
        output$accuracy<-renderPrint(
          {
            dtree=train(label~.,data=trainset,method="rpart",
                        parms=list(split="information"),
                        trControl=trc,tuneLength=3)
            testpred=predict(dtree,newdata = testset)
            confusionMatrix(testpred,testset$label)
          }
        )
        
      })
      
      
      
            observeEvent(input$mlr,{
        final$label=relevel(final$label,ref = "B")
        mlr=multinom(final$label~.,data = final)
        
        p2=predict(mlr, testset)
        dbn4=as.numeric(p2)
        xyq4=prediction(dbn4,testset$label)
        per4=performance(xyq4,"tpr","fpr")
        
        output$plot<-renderPlot(
          {plot(per4)
          })
        output$summary<-renderPrint(
          {
            summary(final)
          }
        )
        output$structure<-renderPrint(
          {
            structure(final)
          }
        )
        output$accuracy<-renderPrint(
          {
            mlr=multinom(final$label~.,data = final)
            p2=predict(mlr, testset)
            confusionMatrix(p2,testset$label)
            
          }
        )
        
      })
}
)
  }
)

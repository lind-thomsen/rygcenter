#' Calculates ODI score and adds the value in a new columns 
#' 
#' udregner:
#'        ODI_sumscore: summen af alle spørgsmål
#'        ODI_missing: antal missing svar
#'        
#' @param data a spinedata dataset
#' @keywords SpineData 
#' @export


odi<-function(data){
  library(dplyr)
  # check for RM questions in data and are they 
  variables<-variable.map()%>%filter(group=="ODI")
  # calculate summaries: RM_sumscore, RM_proportion, RM_missing
  odi<-by(variables,variables$questionnaire,function(questions){
    #only calculate if all questions are present
    ## old code
    # if(sum(colnames(data) %in% questions$variable)==10){
    #   sumscore<-t(apply(data,1,function(data){
    #     c(sum(as.numeric(data[questions$variable]),na.rm = T),sum(is.na(data[questions$variable])))
    #   }))
    #   colnames(sumscore)<-c(paste0("ODI.","sumscore.",questions[1,1]),paste0("ODI.","missing.",questions[1,1]))
    #   sumscore
    # }
    ## 
    n<-sum(colnames(data) %in% questions$variable)  
    #only calculate if all questions are present
    if(sum(colnames(data) %in% questions$variable)==10){ 
      sumscore<-t(apply(data,1,function(data){
        missing<-sum(is.na(data[questions$variable]))
        sumscore<-sum(as.numeric(data[questions$variable]),na.rm = T)
        prop.score<-NA
        if(missing<10){ #answered at least 1 question
          if(missing==0){
            prop.score<-sumscore/50
          }
          else if(missing==1){
            prop.score<-sumscore/45
          } 
          else if(missing>1){
            prop.score<-(sumscore+(sumscore/(10-missing))*missing)/50
          }
        }
        
        c(sumscore,missing,prop.score)
      }))
      colnames(sumscore)<-c(paste0("ODI.sumscore.",questions[1,1]),paste0("ODI.missing.",questions[1,1]),paste0("ODI.propscore.",questions[1,1]))
      
      sumscore
    }
  })
  # add to data
  for(i in 1:length(odi)){
    if(!is.null(odi[[i]])){
      data<-data%>% bind_cols(as.data.frame(odi[[i]]))
    }
  }
  data
}

#' return data for a defined questionnaire used in SpineData.
#' 
#' @param data a data.frame with your data 
#' @param questionnaire.name the data you want e.g. Roland-Morris, ODI, NDI,... etc
#' @param questionnaire.time A character vector with one or more of: "baseline" (dafault), "fu6mp", "fu12mp","fu3mp"
#' @keywords SpineData 
#' @export

questionnaire<-function(data,questionnaire.name,questionnaire.time=c("baseline")){
  questionnaire.time=c("baseline","fu6mp")
  if(questionnaire %in% c("RM","Roland-Morris")){
    variable.names<-variable.map %>% filter(variable=="Roland-Morris" & questionnaire %in% questionnaire.time) %>% select(lbp)%>%simp
  }
}

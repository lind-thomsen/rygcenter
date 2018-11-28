#' Pseudoanonymize by removing columns used for direct identification
#'
#' From spinedata data it removes: "firstName","lastName","cprNumber,postcode , longitude, longitude
#' it saves these identifiers together with patientID, which is kept in the dataset
#' For now assumes that patientID exists
#' 
#' It the chainfile exists the data will be merge before it is saved again, no dublicates allowed
#' 
#' @param data the dataset to anonymize
#' @param datasource SpineData or SurveyXact
#' @param patient.chainfile A .Rdata file for cpr, name and email
#' @param episode.chainfile A .Rdata file, with postcode, and other things that change bewteen episodes
#' @param randomId if true, a random number will be used instead of patientId which will be deleted, the chainfile will not be used
#' 
#' @export


pseudoanonymize<-function(data,datasource="SpineData",patient.chainfile="B:/chainfiles/patients_spinedata_mhmh.Rdata",episode.chainfile="B:/chainfiles/episodes_spinedata_mhmh.Rdata",randomId=F){
  if(datasource=="SpineData"){
    pid.cols<-c("cprNumber","firstName","lastName","email")
    eid.cols<-c("postcode","municipality", "postcode..BLP","postcode..12mP","postcode..6mP","longitude","latitude")
  }
  pid.cols.exists<-which( colnames(data) %in% pid.cols)
  eid.cols.exists<-which( colnames(data) %in% eid.cols)
  
  data.chain.patient<-data %>% 
    select(one_of("patientID"),pid.cols.exists)%>%
    distinct() 
  data.chain.episode<-data %>% 
    select(one_of("patientID","episodeId"),eid.cols.exists)%>%
    distinct() 
  
  if(file.exists(patient.chainfile)){
    #henter: data.chain.saved
    load(patient.chainfile)
    patient.chain.saved<-patient.chain.saved%>%
      anti_join(data.chain.patient,by=c("patientID")) %>%
      bind_rows(data.chain.patient)
      save(patient.chain.saved,file=patient.chainfile)
  }else{
    patient.chain.saved<-data.chain.patient
    save(patient.chain.saved,file=patient.chainfile)
  }
  
  if(file.exists(episode.chainfile)){
    #henter: data.chain.saved
    load(episode.chainfile)
    episode.chain.saved<-episode.chain.saved%>%
      anti_join(data.chain.episode,by=c("patientID","episodeId")) %>%
      bind_rows(data.chain.episode)
    save(episode.chain.saved,file=episode.chainfile)
  }else{
    episode.chain.saved<-data.chain.episode
    save(episode.chain.saved,file=episode.chainfile)
  }
  
  
  data%>%select(-c(pid.cols.exists,eid.cols.exists))
}
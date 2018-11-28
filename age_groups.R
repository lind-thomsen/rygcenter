#' a function to create age groups
#' 
#' At the moment it uses <20 as the lowest group and then e.g. 20-24, 25-29 for grp.size=5
#' it tries to set the maximum age as close to 
#' The new age group variable is called 'age.grp'
#' 
#' @param data A data.frame
#' @param age.col the name of the column with the age value
#' @param grp.size the age inteval to group by, lowest group will be <20, max grp will be 80+, 5 is default
#' 
#' @export

age.groups <- function(data, age.col,grp.size=5){
  max.grp=floor(80/grp.size)
  min.grp=floor(20/grp.size)-1
  grp.levels<-min.grp:max.grp
  grp.labels<-character(length=length(grp.levels))
  grp.labels[1]<-"<20"
  for(i in 2:(length(grp.labels)-1)){
    grp.labels[i]<-paste0(20+(i-2)*grp.size,"-",20+(i-1)*grp.size-1,collapse="")
  }
  grp.labels[length(grp.labels)]<-paste0(20+(length(grp.labels)-2)*grp.size ,"+")
  agegrps<-data %>%
    mutate(age.grp=floor(get(age.col)/grp.size))%>%
    mutate(age.grp=ifelse(age.grp<min.grp,min.grp,age.grp))%>% 
    mutate(age.grp=ifelse(age.grp>max.grp,max.grp,age.grp))%>%
    mutate(age.grp=factor(age.grp,levels = min.grp:max.grp,grp.labels))
  
  return(agegrps)
}

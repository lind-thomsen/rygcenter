#' A SpineData function
#' 
#' calculates the birthday based on danish cpr-number
#' removes '-' if present
#' baseret på https://www.cpr.dk/media/9345/personnummeret-i-cpr.pdf
#' I dette dokument bruges alle de 4 sidste cifre til bestemmelse, men det kan oversættes til kun det første
#' og dermed kan tilsyneladende også bruges til midlertidige/erstatnings CPR numre der ellers indeholder bogstaver.
#' 
#' @param cpr a list or character vector of cpr-numbers
#' @keywords SpineData cpr
#' @export

cpr.birthdate<-function(cpr){
  #cpr=data.1$cprNumber
  #browser()
    result<-simplify2array(
    lapply(cpr,function(cpr1){   #
      #test: cpr1="010203-456"
      cpr1<-gsub("-","",cpr1)
      birth<-"1911-11-11"
    if(nchar(cpr1)!=10){
      cpr1="1111111111" #ukendt CPR
    }  
      
    
    if(nchar(cpr1)==10){
    dato=substr(cpr1,1,4)
    year=as.numeric(substr(cpr1,5,6))
    #last4<-as.numeric(substr(cpr1,7,10))
    control<-as.numeric(substr(cpr1,7,7))
    century<-"19"
    #erstatnings cpr-nummer
    if(!is.na(control)){ # hvis control ikke er et tal så springer vi resten over og går videre med 1900 tallet
    if(year<=36 && control==4){ #last4>=4000 && last4<5000)  {
      century="20"
    }else
    if(year<=57 && control>=5 && control<9){ #last4>=5000 && last4<9000)  {
      century="20"
    }else
    if(year>57 && control>=5 && control<9){ #&& last4>=5000 && last4<9000)  {
      century="18"
    } else
    if(year<=36 && control >=9){ #last4>=9000 )  {
      century="20"
    }
    }
    birth<-paste(c(century,substr(cpr1,5,6),"-",substr(dato,3,4),"-",substr(dato,1,2)),sep="",collapse = "")
    }
      
    return(birth)
      
    
    })
    )
    #browser()
    return(result)
}
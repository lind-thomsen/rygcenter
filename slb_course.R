#' Baseret på et SLB datasæt defineres et forløb på hver linie
#' 
#' Definerer et forløb som alle kontakter med det samme LPR_Contact_start/LPR_Contact_End par, 
#' Hvert forløb får en linie i det færdige datasæt, hvis muligt tilføjes aktionsdiagnose for første besøg
#' aktionsdiagnose for sidste besøg og antallet af besøg
#' KOmplette forløb (courses) har en afsluttet_til som ikke er -1 eller LPR_Contact_End defineret (LPR_Contact_End er ikke sikker nok i sig selv)
#' 
#' Funktionen antager at dato felterne LPR_Contact_start, LPR_Contact_End og Startdato alle er i POSIXct eller Date format
#' 
#' @param data A SLB dataset
#' @export

slb.course<-function(data){
  forloeb<-data%>%
    arrange(Cpr,Startdato)%>%
    group_by(Cpr,LPR_Contact_Start,LPR_Contact_End) %>%
    summarise(contacts=n(),
              first_visit=first(Startdato),
              first_aktdiag=first(kont_aktdiag),
              ContactType=first(ContactType),
              last_visit=last(Startdato),
              last_aktdiag=last(kont_aktdiag),
              Afsluttet=ifelse(!is.na(first(LPR_Contact_End))|last(Afslut_maade)!="-1",1,0),
              Afslut_maade=last(Afslut_maade),
              Stamafdeling=first(Stamafdeling),
              Komnr=first(Komnr)) 
  
  return(forloeb)
  
}

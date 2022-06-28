library(jsonlite)            	
library(countrycode)        	
library(zipcode)              
library(stringr)            	
library(dplyr)			          
library(RCurl)			          

f1_min <- 0
f1_max <- 166

f2_min <- 1
f2_max <- 5

f1 <- f1_min
f2 <- f2_min 

current_folder <- "/MAG-OAGv1/"

p_id <- list()
p_title <- list()
p_year <- numeric() 
p_doc_type <- list()

p_authors_affil <- list()       #list of orgs
p_n_o_Authors <- numeric()      #n_o_Authors
p_n_o_Affiliations <- numeric() #n_o_Affiliations

flag_all_NA <- logical()

special_string_endings <- c(',','.',' ','(',')',';')

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))}

substrLeft = function(text, num_char) {
  substr(text, 1, num_char)
}

istheUS <- function(name,string){
  data(zipcode)
  if (is.na(name)) {return(FALSE)} else
  {
  if ((length(state.abb[which(state.name == name)])==1)|(name %in% state.abb) | (name=='USA') | (name =='U.S.A') | (name =='U.S.A.') |(substrRight(my_string,13)=='United States'))  
    { 
    return(TRUE)} else
  {
    if (grepl(' ',name))
    {
      last_space_position <- gregexpr(' ',name)
      last_space_position_value <- last_space_position[[1]][length(last_space_position[[1]])]
      z_code <- trimws(substrRight(name, nchar(name)-last_space_position_value))
    }
    else
    {z_code <- name}
    
    zip_code <- clean.zipcodes(z_code)
    if (is.na(match(zip_code,zipcode$zip)))
    {return(FALSE)}
    else
    {
      s_abb <- zipcode$state[match(zip_code,zipcode$zip)]
      city_name <- zipcode$city[match(zip_code,zipcode$zip)]
      state_name <- state.name[grep(s_abb, state.abb)]
      
      if (length(state_name)==0) 
      {(grepl(paste(s_abb,' ',z_code),name)) | grepl(city_name,string)}
      else
      {(grepl(paste(s_abb,' ',z_code),name)) | grepl(city_name,string) | grepl(state_name,string)}
    }
  }
  }
}

istheUK <- function(name){
  UK_list <- c('UK', 'U.K', 'U.K.', 'England', 'ENGLAND', 'Scotland', 'SCOTLAND','Wales', 'WALES', 'Northern Ireland', 'NORTHERN IRELAND','Great Britain','GREAT BRITAIN')
  if (is.na(name)) {return(FALSE)}
  else {return((name %in% UK_list) | (substrRight(name,15)=='United Kingdoms'))}
}

isChina_Taiwan <- function(name, string){
  if (is.na(string))  { return('None')} 
  else {
    if (((substrRight(name,3)=='ROC') |(substrRight(name,5)=='R.O.C') | (substrRight(name,5)=='R.O.C.')) & (grepl('Taiwan',string))) {return ('TWN')}         
    else {
      if ((substrRight(name,11)=='Taiwan, ROC')|(substrRight(name,13)=='Taiwan, R.O.C') | (substrRight(name,14)=='Taiwan, R.O.C.'))
      { return ('TWN')}
      else { 
        if ((substrRight(name,5)==', PRC') | (substrRight(name,7)==', P.R.C') | (substrRight(name,9)==' PR China')|(substrRight(name,11)==' P.R. China')) {return ('CHN')}
        else {return ('None')} 
      }         
    }      
  }  
}

for (s in f1_min:f1_max) 
{
  for (t in f2_min:f2_max)
  {
    
    total_papers <- 0
    total_coAuthored_papers <- 0 
    total_coAuthored_papers_International <- 0 
    
    total_coAuthored_Affiliations <- 0  
    total_matched_Affiliations <- 0
    
    Year <- list()
    Country1 <- list()
    Country2 <- list()
    International <- list()
    Weight <- list()
    Title_list <- list()
    Doc_type <- list()

    current_file <- paste(current_folder,"mag_papers_",toString(f1),"_",toString(f2),".txt",sep="")
    bilateral_file <- paste("MAG_bilateral_",toString(f1),"_",toString(f2),".csv",sep="")
    new_file <- paste("mag_bilateral_",toString(f1),"_",toString(f2),".csv",sep="")   
    
    print(current_file)
    
    j <- 1
    
    con = file(current_file, "r")
    while ( TRUE ) 
    {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      
      flag_all_NA[j] <- TRUE
      
      paper <- NULL 
      
      paper <- jsonlite::fromJSON(line)
  
      p_title[j] <- paper$title
      p_year[j] <- paper$year
      p_doc_type[j] <- paper$doc_type
      p_authors_affil[j]<-"" 
      p_n_o_Authors[j] <- length(paper$authors$name)
      
      if (p_n_o_Authors[j]<2) 
      {
        p_n_o_Affiliations[j] <- 0
        p_authors_affil[j] <-"" 
      }
      else
      {
        p_n_o_Affiliations[j] <- length(paper$authors$org)
        
        if (p_n_o_Affiliations[j]==0)
        {
          p_authors_affil[j] <-"" 
        }
        else {
          for (i in 1:p_n_o_Affiliations[j])
          {
            my_string <-paper$authors$org[i]
            
            #some special cases followed
            if (grepl('vcard_country-name=',my_string))
            {
              position <- gregexpr('vcard_country-name=',my_string)
              position_value <- position[[length(position)]][1]
              right_part <- trimws(substrRight(my_string, nchar(my_string)-position_value-18))
              position <- gregexpr(',',right_part)
              position_value <- position[[1]][1]
              country_name <- trimws(substrLeft(right_part, position_value-1))
            }
            else
            {
              if ((grepl('email:',my_string,ignore.case=TRUE)) | (grepl('e-mail:',my_string,ignore.case=TRUE)) | (grepl('emails:',my_string,ignore.case=TRUE)) |(grepl('e-mails:',my_string,ignore.case=TRUE)))
              {
                position_value1 <- 0
                position_value2 <- 0
                position_value3 <- 0
                position_value4 <- 0
                if (grepl('email:',my_string,ignore.case=TRUE)) 
                  {position1 <- gregexpr('email:',my_string,ignore.case=TRUE)
                   position_value1 <- position1[[1]][length(position1[[1]])]}
                if (grepl('e-mail:',my_string,ignore.case=TRUE)) 
                  {position2 <- gregexpr('e-mail:',my_string,ignore.case=TRUE)
                   position_value2 <- position2[[1]][length(position2[[1]])]}
                if (grepl('emails:',my_string,ignore.case=TRUE)) 
                  {position3 <- gregexpr('emails:',my_string,ignore.case=TRUE)
                   position_value3 <- position3[[1]][length(position3[[1]])]}
                if (grepl('e-mails:',my_string,ignore.case=TRUE)) 
                  {position4 <- gregexpr('e-mails:',my_string,ignore.case=TRUE)
                   position_value4 <- position4[[1]][length(position4[[1]])]}
                position_value <- max(position_value1,position_value2,position_value3,position_value4)
                left_part <- trimws(substrLeft(my_string,position_value-1))
                last_char <- substrRight(left_part,1)
                while (last_char %in% special_string_endings)
                {
                  left_part <- substrLeft(left_part, nchar(left_part)-1)
                  last_char <- substrRight(left_part,1)
                }
                position <- gregexpr(',',left_part)
                position_value <- position[[1]][length(position[[1]])]
                country_name <- trimws(substrRight(left_part, nchar(left_part)-position_value))
              }
              else {   
                my_string <- gsub("#TAB#","",my_string) 
                my_string <- gsub(", EU","",my_string) 
                my_string <- gsub("#R#","",my_string) 
                my_string <- gsub("#N#","",my_string)
                last_char <- substrRight(my_string,1)
                while (last_char %in% special_string_endings)
                {
                  my_string <- substrLeft(my_string, nchar(my_string)-1)
                  last_char <- substrRight(my_string,1)
                }
                
                position <-gregexpr(",",my_string)
                position_value <- position[[1]][length(position[[1]])]
                country_name <- trimws(substrRight(my_string, nchar(my_string)-position_value))
              }
            }
            
            if (!is.na(countrycode(country_name, 'iso3c', 'country.name'))){
              country_code <- as.character(country_name)
            } else { country_code <- countrycode(country_name, 'country.name', 'iso3c')}
            
            if (is.na(country_code))
            {
              if (istheUS(country_name,my_string)) #identifying US states
              {
                country_code <- 'USA'
              } else
              {
                if (istheUK(country_name)) #identifying UK component parts
                {
                  country_code <- 'GBR'
                }
                else
                {
                  China_Taiwan <- isChina_Taiwan(country_name,my_string) #identifying China or Taiwan
                  if (!(China_Taiwan=='None') ){
                    if (China_Taiwan=='CHN') {country_code <- 'CHN'}
                    if (China_Taiwan=='TWN') {country_code <- 'TWN'}
                  }
                  else
                  {
                  
                  my_query <- paste('https://query.wikidata.org/sparql?format=json&query=',RCurl::curlEscape(paste('PREFIX
                                                                                                                   schema: <http://schema.org/> PREFIX wdt:
                                                                                                                   <http://www.wikidata.org/prop/direct/> SELECT ?countryLabel WHERE
                                                                                                                   {<https://en.wikipedia.org/wiki/',gsub(' ','_',gsub("\\|","",gsub("\\\\","",country_name))),'> schema:about
                                                                                                                   ?datalink. ?datalink wdt:P17 ?country. SERVICE wikibase:label {
                                                                                                                   bd:serviceParam wikibase:language "en" .}}',sep='')),sep='')
                  mod2 <- try({
                    country_label <- fromJSON(url(my_query))
                  }, TRUE)
                  
                  if(isTRUE(class(mod2)=="try-error")) 
                  { 
                    #alarm
                  } 
                  else { 
                    if(length(country_label$results$bindings$countryLabel$value) >0){
                      country_name <- country_label$results$bindings$countryLabel$value[1]
                      country_code <- countrycode(country_name, 'country.name', 'iso3c')
                    }
                  } 
                } 
              }
            }
          }
            
            
            if (!is.na(country_code)) { flag_all_NA[j] <- FALSE }
            if (i==1)
            {
              p_authors_affil[j] <- country_code
            }
            else
            {
              p_authors_affil[j] <- paste (p_authors_affil[j],"#",country_code) 
            }
          }
        }
      }
      p_authors_affil[j] <- str_split( p_authors_affil[j], " # ")
      
      j <- j+1  
    }
    
    close(con)
    
    full_df <- data.frame(p_year, cbind(p_doc_type), cbind(p_n_o_Authors), cbind(p_n_o_Affiliations),cbind(p_authors_affil), cbind(flag_all_NA), cbind(p_title), stringsAsFactors=FALSE)
    coauthored_df <- distinct(subset(full_df, p_n_o_Authors>1, select = c("p_year","p_n_o_Authors","p_n_o_Affiliations","p_authors_affil","flag_all_NA","p_title")))
    
    total_papers <- nrow(full_df)
    total_coAuthored_papers <- nrow(coauthored_df)
    total_coAuthored_Affiliations <-sum(coauthored_df$p_n_o_Affiliations)
    
    Sys.time()
    
    k <- 1
    for (l in 1: nrow(full_df))
    {
      year <- full_df$p_year[l]
      doc_type <- full_df$p_doc_type[l]
      n_o_Authors <- full_df$p_n_o_Authors[l]
      n_o_Affiliations <- full_df$p_n_o_Affiliations[l]
      country_list <- full_df$p_authors_affil[[l]]
      
      unique_country_list <- unique(country_list)
      
      n_o_Countries <- length(country_list) 
      n_o_unique_Countries <- length(unique_country_list)
      
      if (n_o_unique_Countries==0)
      {
        Year [k] <- year
        Doc_type[k] <- doc_type
        Authors[k] <- n_o_Authors
        Affiliations[k] <- n_o_Affiliations
        N_o_Records[k] <- 1
        Country[k] <- 'NA'
        write.table(data.frame(Year [k], Doc_type[k], Authors[k], Affiliations[k], N_o_Records[k], Country[k], stringsAsFactors = F), file = summary_file, sep=",",append = T,row.names = F, col.names = F)                
        k <-k+1        
      }
      else
      {
        for (m in 1:(n_o_unique_Countries))
        {
          Year [k] <- year
          Doc_type[k] <- doc_type
          Authors[k] <- n_o_Authors
          Affiliations[k] <- n_o_Affiliations
          N_o_Records[k] <- n_o_unique_Countries
          Country[k] <- unique_country_list[m]
          write.table(data.frame(Year [k], Doc_type[k], Authors[k], Affiliations[k], N_o_Records[k], Country[k], stringsAsFactors = F), file = summary_file, sep=",",append = T,row.names = F, col.names = F)                
          k <-k+1
        }          
      }
    }
    
    write.table(data.frame(current_file, j-1, total_papers, total_coAuthored_papers,total_coAuthored_papers_International,  total_coAuthored_Affiliations, total_matched_Affiliations, nrow(relationship_df ), nrow(bilateral_df),k-1, stringsAsFactors = F), file = for_test_matched_numbers, sep=",",append = T,row.names = F, col.names = F)
    
    f2 <- f2+1
    if (f2>f2_max)
    {
    f2 <- 1
      f1 <- f1+1
      if (f1>f1_max) {break}
    }
    if ((f1==166) &(f2==2)) {break}
  }
}

Sys.time()

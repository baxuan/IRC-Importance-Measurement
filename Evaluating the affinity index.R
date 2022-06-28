relcomp <- function(a, b) {
  
  comp <- vector()
  
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  
  return(comp)
}


Period <- c('51-80','81-00','01-20')
Count_type <- c('Whole_count','Fractional_count')
for (x in 1:length(Period))
{
  print(Period[x])
  filename <- paste('RC_All_',Period[x],'.csv', sep='')

  RC <- read.csv(filename,header = TRUE)
  
  for (y in 1:length(Count_type))
  {
    print(Count_type[y])
    Bilateral_RC <- RC[RC$Country1!=RC$Country2,]
    
    if (Count_type[y]=="Whole_count")
    {
      Aggregated_RC <- aggregate(Bilateral_RC$Whole_count, by=list(Country=Bilateral_RC$Country1,Partner=Bilateral_RC$Country2), FUN=sum)  
    }else
    {
      Aggregated_RC <- aggregate(Bilateral_RC$Fractional_count, by=list(Country=Bilateral_RC$Country1,Partner=Bilateral_RC$Country2), FUN=sum)  
    }
    
    colnames(Aggregated_RC) <- c("Country","Partner","Count")
    
    Country_list <- sort(unique(c(Aggregated_RC$Country,Aggregated_RC$Partner)))
    Country_count <- length(Country_list)
    
    CANZUK_list <- c('AUS', 'CAN', 'GBR', 'NZL')
    
    nonCANZUK_list <- relcomp(Country_list,CANZUK_list )
    
    Country_RC_count <- data.frame()
    for (a_country in Country_list)
    {
      RC_count <- sum(Aggregated_RC[Aggregated_RC$Country==a_country,3])+sum(Aggregated_RC[Aggregated_RC$Partner==a_country,3])
      Country_RC_count <- rbind(Country_RC_count,RC_count)
    }
    
    Country_RC_list <- cbind(Country_list, Country_RC_count)
    colnames(Country_RC_list) <- c("Country_name", "RC_count")

    ##########################
    # Calculating AFI values #
    ##########################
    
    AFI_Total <- data.frame()
    
    #Group1: CANZUK countries as CANZUK's partners
    AFI_Group1 <- data.frame()
    for (m in 1:length(CANZUK_list))
    {
      for (n in 1:length(CANZUK_list))
      {
        if (CANZUK_list[m]!=CANZUK_list[n])
        {
          Country <- CANZUK_list[m]
          Partner <- CANZUK_list[n]
          RC_count <- 0
          if (length(as.numeric(Aggregated_RC[(Aggregated_RC$Country == Country) & (Aggregated_RC$Partner == Partner),]$Count))>0)
          {
            RC_count <- RC_count + as.numeric(Aggregated_RC[(Aggregated_RC$Country == Country) & (Aggregated_RC$Partner == Partner),]$Count)
          }
          if (length(as.numeric(Aggregated_RC[(Aggregated_RC$Country == Partner) & (Aggregated_RC$Partner == Country),]$Count))>0)
          {
            RC_count <- RC_count + as.numeric(Aggregated_RC[(Aggregated_RC$Country == Partner) & (Aggregated_RC$Partner == Country),]$Count)
          }
          if (length(as.numeric(RC_count/Country_RC_list[Country_RC_list$Country_name==Country,]$RC_count))>0)
          {
            AFI_value <- RC_count/Country_RC_list[Country_RC_list$Country_name==Country,]$RC_count 
          } else
          {
            AFI_value <- 0
          }
          
          AFI_row <- cbind(Country, Partner, AFI_value)
          AFI_Group1 <- rbind(AFI_Group1,AFI_row)
          AFI_Total <- rbind(AFI_Total,AFI_row)
        }
      }
    }

    #Group2: nonCANZUK countries as CANZUK's partners
    AFI_Group2 <- data.frame()
    for (m in 1:length(CANZUK_list))
    {
      for (n in 1:length(nonCANZUK_list))
      {
          Country <- CANZUK_list[m]
          Partner <- nonCANZUK_list[n]
          RC_count <- 0
          if (length(as.numeric(Aggregated_RC[(Aggregated_RC$Country == Country) & (Aggregated_RC$Partner == Partner),]$Count))>0)
          {
            RC_count <- RC_count + as.numeric(Aggregated_RC[(Aggregated_RC$Country == Country) & (Aggregated_RC$Partner == Partner),]$Count)
          }
          if (length(as.numeric(Aggregated_RC[(Aggregated_RC$Country == Partner) & (Aggregated_RC$Partner == Country),]$Count))>0)
          {
            RC_count <- RC_count + as.numeric(Aggregated_RC[(Aggregated_RC$Country == Partner) & (Aggregated_RC$Partner == Country),]$Count)
          }
          AFI_value <- RC_count/Country_RC_list[Country_RC_list$Country_name==Country,]$RC_count 
          AFI_row <- cbind(Country, Partner, AFI_value)
          AFI_Group2 <- rbind(AFI_Group2,AFI_row)
          AFI_Total <- rbind(AFI_Total,AFI_row)          
      }
    }
    AFI_Group1 <- AFI_Group1[AFI_Group1$AFI_value>0,]
    AFI_Group2 <- AFI_Group2[AFI_Group2$AFI_value>0,]
    AFI_Total <- AFI_Total[AFI_Total$AFI_value>0,]
    
    Group1_median_ratio <- median(as.numeric(AFI_Group1$AFI_value))/median(as.numeric(AFI_Total$AFI_value))
    Group1_SD_ratio <- sd(AFI_Group1$AFI_value)/sd(AFI_Total$AFI_value)
    Group2_median_ratio <- median(as.numeric(AFI_Group2$AFI_value))/median(as.numeric(AFI_Total$AFI_value))
    Group2_SD_ratio <- sd(AFI_Group2$AFI_value)/sd(AFI_Total$AFI_value)
    
    a_AFI_ratio <- cbind(paste('Period: ',Period[x]),Count_type[y], Group1_median_ratio,Group1_SD_ratio,Group2_median_ratio,Group2_SD_ratio)
    write.table(a_AFI_ratio, file='AFI_ratios.csv', append = TRUE, row.names = FALSE, col.names = FALSE, sep=',')
  }
 
}

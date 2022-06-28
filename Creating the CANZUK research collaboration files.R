data_folder <- "/summary_files/"

list_file <- list.files(data_folder)
RC_total <- NULL
RC_df <- NULL

for (file in list_file)
{
  print(file)
  df <- read.csv(paste(data_folder,file,sep=''), header=FALSE, sep=',')
  collaborative_country_df <- df[df$V5>1,]
  n_row <- nrow(collaborative_country_df)
  count <- 1 
  while (count<n_row)
  {
    n_o_unique_Countries <- collaborative_country_df[count,5]
    
    checking_flag <- TRUE
    for (i in 1: (n_o_unique_Countries-1))
    {
      checking_flag <- checking_flag & (collaborative_country_df[count+i-1,1]==collaborative_country_df[count+i,1])
    }
    if (checking_flag==TRUE)
    {
      unique_uppercase_Countries <- unique(toupper(collaborative_country_df$V6[count:(count+n_o_unique_Countries-1)]))
      unique_uppercase_Countries <- unique_uppercase_Countries[!is.na(unique_uppercase_Countries)]
      n_o_unique_uppercase_Countries <-length(unique_uppercase_Countries)
      
      if (n_o_unique_uppercase_Countries>1)
      {
        whole_count <- 1 
        fractional_count <- 1/n_o_unique_uppercase_Countries
        
        if (n_o_unique_uppercase_Countries==2)
        {
          RC <- c(collaborative_country_df[count,1], unique_uppercase_Countries[1], unique_uppercase_Countries[2], whole_count, fractional_count)
          RC_total <- rbind(RC_total,RC)
        }
        else
        {
          for (i in 1:(n_o_unique_uppercase_Countries-1))
          {
            for (j in (i+1):(n_o_unique_uppercase_Countries))
            {
              RC <- c(collaborative_country_df[count,1],unique_uppercase_Countries[i], unique_uppercase_Countries[j], whole_count, fractional_count)
              RC_total <- rbind(RC_total,RC)
            }
          }
        }
      }
    }
    else
    {
      print('Error')
      print(count)
    }
    count <- count + n_o_unique_Countries
  }
}

RC_df <- as.data.frame(RC_total)
colnames(RC_df) <- c("Year","Country1","Country2","Whole_count","Fractional_count")

All_df <- RC_df[!is.na(RC_df$Country1) & !is.na(RC_df$Country2),]

write.csv(All_df,paste("RC_All.csv", sep=''), row.names = FALSE)

All_51_80 <- All_df[(as.numeric(All_df$Year)>1950 & as.numeric(All_df$Year)<=1980),]
write.csv(All_51_80,"RC_All_51-80.csv", row.names = FALSE)

All_81_00 <- All_df[(as.numeric(All_df$Year)>1980 & as.numeric(All_df$Year)<=2000),]
write.csv(All_81_00,"RC_All_81-00.csv", row.names = FALSE)

All_01_20 <- All_df[(as.numeric(All_df$Year)>2000 & as.numeric(All_df$Year)<=2020),]
write.csv(All_01_20,"RC_All_01-20.csv", row.names = FALSE)



CANZUK<- All_df[(All_df$Country1 %in% c('CAN','AUS', 'NZL', 'GBR'))|(All_df$Country2 %in% c('CAN','AUS', 'NZL', 'GBR')),]

CANZUK2 <- CANZUK[order(CANZUK$Year),]
for (i in 1:nrow(CANZUK2))
{
  if (CANZUK2[i,2]>CANZUK2[i,3])
  {
    temp <- CANZUK2[i,2]
    CANZUK2[i,2] <- CANZUK2[i,3]
    CANZUK2[i,3] <- temp
  }
}
write.csv(CANZUK2,"RC_CANZUK.csv", row.names = FALSE)

CANZUK_51_80 <- CANZUK2[(as.numeric(CANZUK2$Year)>1950 & as.numeric(CANZUK2$Year)<=1980),]
write.csv(CANZUK_51_80,"RC_CANZUK_51-80.csv", row.names = FALSE)

CANZUK_81_00 <- CANZUK2[(as.numeric(CANZUK2$Year)>1980 & as.numeric(CANZUK2$Year)<=2000),]
write.csv(CANZUK_81_00,"RC_CANZUK_81-00.csv", row.names = FALSE)

CANZUK_01_20 <- CANZUK2[(as.numeric(CANZUK2$Year)>2000 & as.numeric(CANZUK2$Year)<=2020),]
write.csv(CANZUK_01_20,"RC_CANZUK_01-20.csv", row.names = FALSE)


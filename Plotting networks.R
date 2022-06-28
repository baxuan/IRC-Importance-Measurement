CANZUK_list <- c('AUS', 'CAN', 'GBR', 'NZL')

Period <- c('51-80','81-00','01-20')
Count_type <- c('Whole_count','Fractional_count')

for (x in 1:length(Period))
{
  print(Period[x])
  filename <- paste('RC_All_',Period[x],'.csv', sep='')
  
  RC <- read.csv(filename,header = TRUE)
  
  Bilateral_All_RC <- RC[RC$Country1!=RC$Country2,]
  Bilateral_CANZUK_RC <- Bilateral_All_RC[((Bilateral_All_RC$Country1 %in% CANZUK_list) | (Bilateral_All_RC$Country2 %in% CANZUK_list)),]
  
  
  ##################
  # Whole Counting #

  Aggregated_CANZUK_RC <- aggregate(Bilateral_CANZUK_RC$Whole_count, by=list(Country1=Bilateral_CANZUK_RC$Country1,Country2=Bilateral_CANZUK_RC$Country2), FUN=sum)
  colnames(Aggregated_CANZUK_RC) <- c("Country1","Country2","Whole_count_total")
  Med_CANZUK_RC <- median(Aggregated_CANZUK_RC$Whole_count_total)
  Aggregated_CANZUK_RC <- Aggregated_CANZUK_RC[Aggregated_CANZUK_RC$Whole_count_total >= Med_CANZUK_RC,]
  
  Country_list <- sort(unique(c(Aggregated_CANZUK_RC$Country1,Aggregated_CANZUK_RC$Country2)))
  Country_count <- length(Country_list)
  
  Country_RC_count <- data.frame()
  for (country in Country_list)
  {
    RC_count <- sum(Aggregated_CANZUK_RC[Aggregated_CANZUK_RC$Country1==country,3])+sum(Aggregated_CANZUK_RC[Aggregated_CANZUK_RC$Country2==country,3])
    Country_RC_count <- rbind(Country_RC_count,RC_count)
  }
  
  Country_RC_list <- cbind(Country_list, Country_RC_count)
  colnames(Country_RC_list) <- c("Country_name", "RC_count")
  
  Med_Country <- median(Country_RC_list$RC_count)
  
  notCANZUK_RC_count <- Country_RC_list[!(Country_RC_list$Country_name %in% CANZUK_list),]
  top5_notCANZUK <- head(notCANZUK_RC_count[order(notCANZUK_RC_count$RC_count,decreasing = TRUE),],5)
  
  cutoff <- head(sort(notCANZUK_RC_count$RC_count, decreasing = TRUE),15)[15]
  
  Country_list_filtered <- Country_list 
  for (i in 1: length(Country_RC_list$Country_name))
  {
    if (!(Country_RC_list$Country_name[i]  %in% CANZUK_list) & (Country_RC_list$RC_count[i]<cutoff))
    {
      Country_list_filtered[i] <- ""
    }
  }
  
  df <- as.data.frame(matrix(0, ncol = Country_count, nrow = Country_count))
  row.names(df) <- Country_RC_list$Country_name
  colnames(df) <- Country_RC_list$Country_name
  
  for (k in 1:nrow(Aggregated_CANZUK_RC))
  {
    for (i in 1:Country_count)
    {
      for (j in 1:Country_count)
      {
        if ((Aggregated_CANZUK_RC[k,1]==Country_RC_list$Country_name[i]) & (Aggregated_CANZUK_RC[k,2]==Country_RC_list$Country_name[j]) & (i!=j))
        {
          df[i,j] <- df[i,j] + Aggregated_CANZUK_RC[k,3]
          df[j,i] <- df[j,i] + Aggregated_CANZUK_RC[k,3]
        }
      }
    }
  }
  
  dm <- data.matrix(df)
  
  RC_statnet = as.network(dm, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
  RC_statnet 
  
  RC_country_col <- ifelse(colnames(dm) %in% CANZUK_list, "blue", "gray")
  RC_label_col <- ifelse(Country_RC_list$Country_name %in% CANZUK_list, "blue", ifelse(Country_RC_list$RC_count>=top5_notCANZUK$RC_count[5], "black", "gray40"))
  RC_label_cex <- ifelse(Country_RC_list$Country_name %in% CANZUK_list, 0.7, ifelse(Country_RC_list$RC_count>=top5_notCANZUK$RC_count[5], 0.7, 0.6))
  
  temp_Vertex_cex <- log(as.double(unlist(Country_RC_list$RC_count/Med_Country))/10)
  Vertex_cex <- ifelse(temp_Vertex_cex<0.5, 0.1, temp_Vertex_cex)
  
  Edge.lwd <- log(get.edge.value(RC_statnet, "edge.lwd")/median(get.edge.value(RC_statnet, "edge.lwd")))
  
  plot.network(RC_statnet, vertex.cex = Vertex_cex ,  vertex.col = RC_country_col, edge.col = "gray90", edge.lwd = Edge.lwd, label = Country_list_filtered, label.col = RC_label_col, label.cex = RC_label_cex, label.pad = 0, label.pos = 0)
  
  
  
  ######################
  # Fractional Counting #
  print('Fractional Counting')
  
  Aggregated_CANZUK_RC <- aggregate(Bilateral_CANZUK_RC$Fractional_count, by=list(Country1=Bilateral_CANZUK_RC$Country1,Country2=Bilateral_CANZUK_RC$Country2), FUN=sum)
  colnames(Aggregated_CANZUK_RC) <- c("Country1","Country2","Fractional_count_total")
  Med_CANZUK_RC <- median(Aggregated_CANZUK_RC$Fractional_count_total)
  Aggregated_CANZUK_RC <- Aggregated_CANZUK_RC[Aggregated_CANZUK_RC$Fractional_count_total >= Med_CANZUK_RC,]
  
  Country_list <- sort(unique(c(Aggregated_CANZUK_RC$Country1,Aggregated_CANZUK_RC$Country2)))
  Country_count <- length(Country_list)
  
  Country_RC_count <- data.frame()
  for (country in Country_list)
  {
    RC_count <- sum(Aggregated_CANZUK_RC[Aggregated_CANZUK_RC$Country1==country,3])+sum(Aggregated_CANZUK_RC[Aggregated_CANZUK_RC$Country2==country,3])
    Country_RC_count <- rbind(Country_RC_count,RC_count)
  }
  
  Country_RC_list <- cbind(Country_list, Country_RC_count)
  colnames(Country_RC_list) <- c("Country_name", "RC_count")
  
  Med_Country <- median(Country_RC_list$RC_count)
  
  notCANZUK_RC_count <- Country_RC_list[!(Country_RC_list$Country_name %in% CANZUK_list),]
  top5_notCANZUK <- head(notCANZUK_RC_count[order(notCANZUK_RC_count$RC_count,decreasing = TRUE),],5)
  
  cutoff <- head(sort(notCANZUK_RC_count$RC_count, decreasing = TRUE),15)[15]
  
  Country_list_filtered <- Country_list 
  for (i in 1: length(Country_RC_list$Country_name))
  {
    if (!(Country_RC_list$Country_name[i]  %in% CANZUK_list) & (Country_RC_list$RC_count[i]<cutoff))
    {
      Country_list_filtered[i] <- ""
    }
  }
  
  df <- as.data.frame(matrix(0, ncol = Country_count, nrow = Country_count))
  row.names(df) <- Country_RC_list$Country_name
  colnames(df) <- Country_RC_list$Country_name
  
  for (k in 1:nrow(Aggregated_CANZUK_RC))
  {
    for (i in 1:Country_count)
    {
      for (j in 1:Country_count)
      {
        if ((Aggregated_CANZUK_RC[k,1]==Country_RC_list$Country_name[i]) & (Aggregated_CANZUK_RC[k,2]==Country_RC_list$Country_name[j]) & (i!=j))
        {
          df[i,j] <- df[i,j] + Aggregated_CANZUK_RC[k,3]
          df[j,i] <- df[j,i] + Aggregated_CANZUK_RC[k,3]
        }
      }
    }
  }
  
  dm <- data.matrix(df)
  
  RC_statnet = as.network(dm, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
  RC_statnet 
  
  RC_country_col <- ifelse(colnames(dm) %in% CANZUK_list, "blue", "gray")
  RC_label_col <- ifelse(Country_RC_list$Country_name %in% CANZUK_list, "blue", ifelse(Country_RC_list$RC_count>=top5_notCANZUK$RC_count[5], "black", "gray40"))
  RC_label_cex <- ifelse(Country_RC_list$Country_name %in% CANZUK_list, 0.7, ifelse(Country_RC_list$RC_count>=top5_notCANZUK$RC_count[5], 0.7, 0.6))
  
  temp_Vertex_cex <- log(as.double(unlist(Country_RC_list$RC_count/Med_Country))/10)
  Vertex_cex <- ifelse(temp_Vertex_cex<0.5, 0.1, temp_Vertex_cex)
  
  Edge.lwd <- log(get.edge.value(RC_statnet, "edge.lwd")/median(get.edge.value(RC_statnet, "edge.lwd")))
  
  plot.network(RC_statnet, vertex.cex = Vertex_cex ,  vertex.col = RC_country_col, edge.col = "gray90", edge.lwd = Edge.lwd, label = Country_list_filtered, label.col = RC_label_col, label.cex = RC_label_cex, label.pad = 0, label.pos = 0)
  
}


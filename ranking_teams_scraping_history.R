uefa_country_ranking_teams <- data.frame("country","team",0,"bla",0)
colnames(uefa_country_ranking_teams) <- c("country","team","scored_points","status","year")

years <- 2025:1956
#years <- c("2023","2022","2021","2020","2019")

for (y in years) {
  
  if (y > 2017) {
  url <- paste0("https://kassiesa.net/uefa/data/method5/ccoef",y,".html")
  } else if (y > 2008) {
  url <- paste0("https://kassiesa.net/uefa/data/method4/ccoef",y,".html")
  } else if (y > 2003) {
  url <- paste0("https://kassiesa.net/uefa/data/method3/ccoef",y,".html")
  } else if (y > 1998) {  
  url <- paste0("https://kassiesa.net/uefa/data/method2/ccoef",y,".html")
  } else {
  url <- paste0("https://kassiesa.net/uefa/data/method1/ccoef",y,".html")
  }  
  
  webpage <- read_html(url)
  uefa_table <- html_text(html_nodes(webpage,"td"))

  r <- 1
  
  repeat {
    
    if (grepl("\n",uefa_table[r]) == TRUE) {
      
      country <- gsub(" teams.*","",uefa_table[r])
      country <- gsub("\n","",country)  
      country <- substr(country,1,nchar(country)-1)    
      teams_count <- gsub("teams.*","",uefa_table[r])
      teams_count <- as.numeric(trimws(gsub(".*\n","",teams_count)))
      
    }
    
    if (grepl("CL|EL|ECL|UC|CWC|CO",uefa_table[r]) == TRUE) { 
      
      team <- uefa_table[r-1]
      team_points <- as.numeric(uefa_table[r+8])/teams_count
      status <- uefa_table[r+9]
      
      new_data <- data.frame(country,team,team_points,status,y)
      colnames(new_data) <- c("country","team","scored_points","status","year")
      
      uefa_country_ranking_teams <- rbind(uefa_country_ranking_teams,new_data)
      print(new_data)
      
    }    
    r <- r+1
    
    if (r == length(uefa_table)) {
      break  
    }  
    
  }
  
}

#Adaptations
uefa_country_ranking_teams <- uefa_country_ranking_teams[-1,]
uefa_country_ranking_teams <- uefa_country_ranking_teams[nchar(uefa_country_ranking_teams$team) > 3,]
uefa_country_ranking_teams$status[uefa_country_ranking_teams$status == ""] <- "out"

#Adaption Teamnames
uefa_country_ranking_teams$team <- gsub("Dinamo Kiev","Dynamo Kyiv",uefa_country_ranking_teams$team)
uefa_country_ranking_teams$team <- gsub("Dinamo Kyiv","Dynamo Kyiv",uefa_country_ranking_teams$team)
uefa_country_ranking_teams$team <- gsub("Glasgow Rangers","Rangers",uefa_country_ranking_teams$team)
uefa_country_ranking_teams$team <- gsub("Olympique Lyon","Olympique Lyonnais",uefa_country_ranking_teams$team)
uefa_country_ranking_teams$team <- gsub("Red Star Belgrade","Crvena zvezda",uefa_country_ranking_teams$team)
uefa_country_ranking_teams$team <- gsub("Servette FC Genève","Servette FC",uefa_country_ranking_teams$team)
uefa_country_ranking_teams$team <- gsub("Dinamo Zagreb","GNK Dinamo",uefa_country_ranking_teams$team)
uefa_country_ranking_teams$team <- gsub("LASK Linz","LASK",uefa_country_ranking_teams$team)


write.csv(uefa_country_ranking_teams,"Output/ranking_teams_history.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


#Tidy table for all seasons
points_team <- uefa_country_ranking_teams %>%
  group_by(team) %>%
  summarise(overall_points_team = sum(scored_points),country=country)

points_country <- uefa_country_ranking_teams %>%
  group_by(country) %>%
  summarise(overall_points_country = sum(scored_points))

#Adapt Russia
points_country$overall_points_country[47] <- points_country$overall_points_country[47]+4.333

complete_table <- unique(left_join(points_team,points_country))

complete_table <- complete_table[order(complete_table$team),]
#complete_table <- complete_table[-c(3:4,6:10),]

complete_table$percentage <- complete_table$overall_points_team/complete_table$overall_points_country


#Wappen
flags <- read_excel("Data/flags.xlsx", col_names = FALSE)
colnames(flags) <- c("flag","country")

complete_table <- merge(complete_table,flags,all.x = TRUE)
#complete_table$country <- paste0(complete_table$flag,complete_table$country)
complete_table <- complete_table[order(complete_table$flag),]

#Text
complete_table$text <- paste0(complete_table$country,": ",round(complete_table$overall_points_country,3)," points")

complete_table <- complete_table %>%
  distinct(team, .keep_all=TRUE) %>%
  arrange(country)

write.csv(complete_table,"Output/ranking_teams_overview_history.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

#Success rate
participations <- data.frame(table(uefa_country_ranking_teams$team))
colnames(participations) <- c("team","participations")
success_rate <- merge(complete_table,participations)
success_rate$average_points <- success_rate$overall_points_team/success_rate$participations
success_rate <- success_rate %>%
  filter(participations >= 10) %>%
  select(flag,country,team,participations,average_points)

write.csv(success_rate,"Output/success_teams_history.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


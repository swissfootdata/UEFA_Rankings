ranking_map_data <- data.frame("country",0,0,"title_datawrapper_overall","text_datawrapper_overall","title_datawrapper_season","text_datawrapper_season")
colnames(ranking_map_data) <- c("country","overall_points","points_season","title_datawrapper_overall","text_datawrapper_overall","title_datawrapper_season","text_datawrapper_season")

for (c in 1:nrow(uefa_country_ranking_full)) {

country_name <- gsub(".*:","",uefa_country_ranking_full$country[c])
teams_overall <- complete_table %>%
  filter(country == country_name) %>%
  arrange(desc(overall_points_team))

teams_season <- complete_table_season %>%
  filter(country == country_name) %>%
  arrange(desc(overall_points_team))

#teams_season$status <- gsub("not out","&#x2714;&#xFE0F;",teams_season$status)
#teams_season$status <- gsub("out","&#x274C;",teams_season$status)

text_datawrapper_overall <- ""
for (t in 1:nrow(teams_overall)) {
text_datawrapper_overall <- paste0(text_datawrapper_overall,teams_overall$team[t],": <b>",
                                   round(teams_overall$overall_points_team[t],3),
                                   "</b> (",
                                   round(teams_overall$percentage[t]*100,1),"%)<br>")
}  

text_datawrapper_season <- ""
for (t in 1:nrow(teams_season)) {
  text_datawrapper_season <- paste0(text_datawrapper_season,teams_season$team[t],": <b>",
                                     round(teams_season$overall_points_team[t],3),
                                     "</b> (",
                                     round(teams_season$percentage[t]*100,1),"%, ",teams_season$status[t],
                                     ")<br>")
}  

new_data <- data.frame(country_name,uefa_country_ranking_full[c,5],uefa_country_ranking_full[c,4],
                       teams_overall$text[1],text_datawrapper_overall,
                       teams_season$text[1],text_datawrapper_season)
colnames(new_data) <- c("country","overall_points","points_season","title_datawrapper_overall","text_datawrapper_overall","title_datawrapper_season","text_datawrapper_season")

ranking_map_data <- rbind(ranking_map_data,new_data)

print(new_data)

}

ranking_map_data <- ranking_map_data[-1,]

#Adaptation
ranking_map_data$country <- gsub("Russia","Russian Federation",ranking_map_data$country)
ranking_map_data$country <- gsub("Slovakia","Slovak Republic",ranking_map_data$country)
ranking_map_data$country <- gsub("Bosnia-Herzegovina","Bosnia and Herzegovina",ranking_map_data$country)
ranking_map_data$country <- gsub("Faroe Islands","Faeroe Is.",ranking_map_data$country)

ranking_map_data$title_datawrapper_season[which(grepl("Russian",ranking_map_data$country))] <-
  "Russia: excluded"

ranking_map_data$text_datawrapper_season[which(grepl("Russian",ranking_map_data$country))] <-
  "Due to the war in Ukraine, the Russian Federation got excluded from all UEFA competitions. They recieved 4.333 points for the country ranking, which is the lowest number they have earned in any of the last five seasons."

ranking_map_data$points_season[which(grepl("Russian",ranking_map_data$country))] <- "0"


ranking_map_data$text_datawrapper_season <- gsub("NaN%","0%",ranking_map_data$text_datawrapper_season)

write.csv(ranking_map_data,"Output/ranking_map_data.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

###Team maps
team_coordinates <- read_excel("Data/coordinates_clubs.xlsx", col_names = TRUE)

team_data_with_coordinates <- merge(complete_table,team_coordinates,all.x=TRUE)

team_data_with_coordinates$overall_points_team <- round(team_data_with_coordinates$overall_points_team,3)
team_data_with_coordinates$overall_points_country <- round(team_data_with_coordinates$overall_points_country,3)
team_data_with_coordinates$percentage <- round(team_data_with_coordinates$percentage*100,1)

write.csv(team_data_with_coordinates,"Output/ranking_map_data_teams.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


###Team maps
team_data_with_coordinates_season <- merge(complete_table_season,team_coordinates,all.x=TRUE)

team_data_with_coordinates_season$overall_points_team <- round(team_data_with_coordinates_season$overall_points_team,3)
team_data_with_coordinates_season$overall_points_country <- round(team_data_with_coordinates_season$overall_points_country,3)
team_data_with_coordinates_season$percentage <- round(team_data_with_coordinates_season$percentage*100,1)

write.csv(team_data_with_coordinates_season,"Output/ranking_map_data_teams_season.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

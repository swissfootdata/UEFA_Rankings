current_points <- data.frame("team",0,0)
colnames(current_points) <- c("team","match_points","bonus_points")

url <- "https://kassiesa.net/uefa/data/method5/tcoef2024.html"
webpage <- read_html(url)
team_points <- html_text(html_nodes(webpage,"td"))

repeat {
  
  team <- team_points[1:10]
  team_selection <- team_points[c(3,7,9)]
  names(team_selection) <- c("team","match_points","bonus_points")
  current_points <- rbind(current_points,team_selection)
  team_points <- team_points[-(1:10)]
  
  if (length(team_points) < 10) {
    break
  }
  
}


current_points <- current_points[-1,]
current_points$team <- gsub("Glasgow Rangers","Rangers",current_points$team)
current_points$team <- gsub("Servette FC Genève","Servette FC",current_points$team)

current_points <- current_points %>%
  mutate(match_points = as.numeric(match_points),
         bonus_points = as.numeric(bonus_points),
         points_overall = match_points+bonus_points,
         date = Sys.Date())

#Get old data and add
old_data <- read.csv("https://raw.githubusercontent.com/swissfootdata/UEFA_Rankings/master/Output/club_owner_data.csv", encoding = "UTF-8")

current_points <- rbind(current_points,old_data)
write.csv(current_points,"Output/club_owner_data.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

print("New club owner data scraped")

#Get match Data
teams <- read_xlsx("./data/marktwerte_UEFA_competitions.xlsx")
teams$logo <- paste0('<img src="https://tmssl.akamaized.net/images/wappen/head/',teams$logo,'.png" height="40"></img>')
teams$value_table <- paste0('<p style="color:darkgreen">',format(teams$value*1000000,big.mark = ","),' €</p>')
teams$club_list <- paste0(teams$club," (",teams$nation,"): ",format(teams$value,nsmall =2),' Mio. €')
teams <- teams %>%
  filter(!is.na(club))

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

#Team in or out
status <- webpage %>%
  html_nodes("td") %>%
  html_attr("class")%>%
  na.omit()

status <- status[seq(1,length(status),2)]
status <- ifelse(status == "aleft blue","&#x2714;&#xFE0F;","&#x274C;")

current_points <- current_points %>%
  mutate(match_points = as.numeric(match_points),
         bonus_points = as.numeric(bonus_points),
         points_overall = match_points+bonus_points,
         date = Sys.Date(),
         status = status)

#Merge with team data
current_points <- teams %>%
  left_join(current_points,by=c("club"="team"))


#Get old data and add
old_data <- read.csv("Output/club_owner_data.csv", encoding = "UTF-8")
old_data$date <- as.Date(old_data$date)
colnames(old_data)[1] <- "id"

old_data <- old_data %>%
  filter(date <= Sys.Date()-3)

all_points <- rbind(current_points,old_data)

#Save data
write.csv(all_points,"Output/club_owner_data.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

#Get gain from last week
previous_date <- max(old_data[old_data$date <= Sys.Date()-3,]$date)

#Get points last week
points_last_week <- old_data %>%
  filter(date == previous_date) %>%
  select(club,match_points,bonus_points,points_overall) %>%
  rename(match_points_last_week = match_points,
         bonus_points_last_week = bonus_points,
         points_overall_last_week = points_overall)

current_points <- current_points %>%
  left_join(points_last_week) %>%
  mutate(gain_match_points = match_points-match_points_last_week,
         gain_bonus_points = bonus_points-bonus_points_last_week,
         gain_points_overall = points_overall -points_overall_last_week)

#Get Owner Data
owner_data <- read_xlsx("./Data/owner_data.xlsx")
owner_data$full_name <- paste0(owner_data$first_name," ",owner_data$last_name)

owner_data_overview <- data.frame("player_id","Owner",0,0,0,0,
                                  0,"Motto","Twitter","Favorite Club")
colnames(owner_data_overview) <- c("player_id","Owner","Points Overall","Match Points","Bonus Points","Points Gained This Week",
                                   "Money Spent in Mio. €","Motto","Twitter","Favorite Club")
for (o in 1:nrow(owner_data)) {
  owner_portfolio <- current_points %>%
    filter(current_points$id == owner_data$team1_id[o] |
             current_points$id == owner_data$team2_id[o] |
             current_points$id == owner_data$team3_id[o] |
             current_points$id == owner_data$team4_id[o] |
             current_points$id == owner_data$team5_id[o] |
             current_points$id == owner_data$team6_id[o] |
             current_points$id == owner_data$team7_id[o] |
             current_points$id == owner_data$team8_id[o] |
             current_points$id == owner_data$team9_id[o] |
             current_points$id == owner_data$team10_id[o])
  
  new_entry <- data.frame(owner_data$player_id[o],
                          owner_data$full_name[o],
                          sum(owner_portfolio$points_overall),
                          sum(owner_portfolio$match_points),
                          sum(owner_portfolio$bonus_points),
                          sum(owner_portfolio$gain_points_overall),
                          owner_data$value_overall[o],
                          owner_data$motto[o],
                          owner_data$twitter[o],
                          owner_data$favorite_club[o])
  colnames(new_entry) <- c("player_id","Owner","Points Overall","Match Points","Bonus Points","Points Gained This Week",
                           "Money Spent in Mio. €","Motto","Twitter","Favorite Club")
  
  owner_data_overview <- rbind(owner_data_overview,new_entry)
}    

owner_data_overview <- owner_data_overview[-1,]

owner_data_overview <- owner_data_overview %>%
  arrange(desc(`Points Overall`),
          `Money Spent in Mio. €`,
          Owner) %>%
  mutate(`Points Overall` = paste0("<b>",`Points Overall`,"</b>"))

#Save Owner Overview
write.csv(owner_data_overview,"Output/owner_overview.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

success_clubs <- current_points %>%
  select(logo,
         club,
         nation,
         match_points,
         bonus_points,
         points_overall,
         status) %>%
  arrange(desc(points_overall),
          club) %>%
  mutate(points_overall = paste0("<b>",points_overall,"</b>"))

write.csv(success_clubs,"Output/success_clubs.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


efficiency_clubs <- current_points %>%
  mutate(mio_per_point = round(value/points_overall,2)) %>%
  select(logo,
         club,
         nation,
         value_table,
         points_overall,
         mio_per_point,
         status) %>%
  arrange(mio_per_point,
          value_table) %>%
  mutate(mio_per_point = paste0("<b>",mio_per_point,"</b>"))
efficiency_clubs$mio_per_point <- gsub("Inf","-",efficiency_clubs$mio_per_point)

write.csv(efficiency_clubs, "Output/efficiency_clubs.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

cat(toString(Sys.time()),file="Output/last_update.txt")

print("New club owner data scraped")


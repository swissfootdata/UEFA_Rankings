library(rvest)
library(stringr)
library(XML)
library(RSelenium)
library(dplyr)

setwd("C:/Users/simon/Onedrive/Fussballdaten/uefa_ranking")

uefa_country_ranking_teams <- data.frame("country","team",0)
colnames(uefa_country_ranking_teams) <- c("country","team","scored_points")

years <- c("2022","2021","2020","2019","2018")

for (y in years) {
  
url <- paste0("https://kassiesa.net/uefa/data/method5/ccoef",y,".html")
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

if (grepl("CL|EL|ECL",uefa_table[r]) == TRUE) { 

team <- uefa_table[r-1]
team_points <- as.numeric(uefa_table[r+8])/teams_count

new_data <- data.frame(country,team,team_points)
colnames(new_data) <- c("country","team","scored_points")

uefa_country_ranking_teams <- rbind(uefa_country_ranking_teams,new_data)
print(new_data)
  
}    
r <- r+1

if (r == length(uefa_table)) {
break  
}  

}

}

uefa_country_ranking_teams <- uefa_country_ranking_teams[-1,]
write.csv(uefa_country_ranking_teams,"Output/uefa_country_ranking_teams.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

#Tidy table
points_team <- uefa_country_ranking_teams %>%
  group_by(team) %>%
  summarise(overall_points_team = sum(scored_points),country=country)

points_country <- uefa_country_ranking_teams %>%
  group_by(country) %>%
  summarise(overall_points_country = sum(scored_points))


complete_table <- unique(left_join(points_team,points_country))

complete_table <- complete_table[order(complete_table$team),]
complete_table <- complete_table[-c(3:4,6:10),]

complete_table$percentage <- complete_table$overall_points_team/complete_table$overall_points_country


#Wappen
library(readxl)
flags <- read_excel("flags.xlsx", col_names = FALSE)
colnames(flags) <- c("flag","country")

complete_table <- merge(complete_table,flags,all.x = TRUE)
#complete_table$country <- paste0(complete_table$flag,complete_table$country)
complete_table <- complete_table[order(-complete_table$overall_points_country),]

#Text
complete_table$text <- paste0(complete_table$country,": ",round(complete_table$overall_points_country,3)," points")




write.csv(complete_table,"Output/overview_teams_ranking.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


#Commit and update Datawrapper
library(git2r)
library(DatawRappr)

gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}

gitstatus <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git status"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitadd <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add --all"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitpush <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitpull <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git pull"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}


#Make Commit
token <- read.csv("C:/Users/simon/OneDrive/Github_Token/token.txt",header=FALSE)[1,1]

git2r::config(user.name = "awp-finanznachrichten",user.email = "sw@awp.ch")
invisible(git2r::cred_token(token))
gitadd()
gitcommit()
gitpush()


#Update Datawrapper
datawrapper_auth("fYNHJEgLlCPgMC8hO0Bxm7j3SG2cOGCPnIJRc5RCVc72zYBFaMxGYIOY081zYaeq", overwrite = TRUE)
dw_edit_chart("qw90m", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("qw90m")

dw_edit_chart("up5me", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("up5me")

dw_edit_chart("v88MJ", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("v88MJ")


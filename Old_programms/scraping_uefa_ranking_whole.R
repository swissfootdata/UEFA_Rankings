library(rvest)
library(stringr)
library(XML)
library(RSelenium)


setwd("C:/Users/simon/Onedrive/Fussballdaten/uefa_ranking")

#Browser öffnen
driver <- RSelenium::rsDriver(port= 4568L, browser = "firefox")

remote_driver <- driver[["client"]]
remote_driver$navigate("https://kassiesa.net/uefa/data/method5/crank2022.html")

output <- remote_driver$findElement(using="class",value="countrygroup")

text_all <- output$getElementText()

#Close browser
remote_driver$close()

#Close server
driver[["server"]]$stop()

#Stop Java-Process
system("taskkill /F /IM java.exe")


text_all <- gsub("\n"," ",text_all)
text_all <- gsub("/ ","/",text_all)

#Replace Countries
text_all <- gsub("Czech Republic","Czech_Republic",text_all)
text_all <- gsub("Northern Ireland","Northern_Ireland",text_all)
text_all <- gsub("Faroe Island","Faroe_Island",text_all)
text_all <- gsub("North Macedonia","North_Macedonia",text_all)
text_all <- gsub("San Marino","San_Marino",text_all)
text_all <- strsplit(text_all," ")[[1]]

uefa_table <- text_all[-c(1:9)]

#url <- "https://kassiesa.net/uefa/data/method5/crank2022.html"
#webpage <- read_html(url)

#uefa_table <- html_text(html_nodes(webpage,"td"))


#uefa_table <- uefa_table[uefa_table!=""]
#uefa_table <- uefa_table[-(496:498)]

uefa_country_ranking_full <- data.frame(1,2,3,4,5,6,7,8,9)
names(uefa_country_ranking_full) <- c("rank","country","17/18","18/19","19/20","20/21","21/22","overall","teams")

repeat {

country <- uefa_table[1:9]
names(country) <- c("rank","country","17/18","18/19","19/20","20/21","21/22","overall","teams")
uefa_country_ranking_full <- rbind(uefa_country_ranking_full,country)
uefa_table <- uefa_table[-(1:9)]

if (length(uefa_table) < 9) {
  
  break
}

}


uefa_country_ranking_full <- uefa_country_ranking_full[-1,]
uefa_country_ranking_full$teams <- gsub(" ","",uefa_country_ranking_full$teams)
uefa_country_ranking_full$country <- gsub("_"," ",uefa_country_ranking_full$country)

for (i in 1:nrow(uefa_country_ranking_full)) {
  
  if (nchar(uefa_country_ranking_full$teams[i]) == 1) {
    
    uefa_country_ranking_full$teams[i] <- paste0("0/",uefa_country_ranking_full$teams[i])
    
  }
  
}

#Calculate gap
uefa_country_ranking_full$gap <- 0

for (u in 2:nrow(uefa_country_ranking_full)) {

uefa_country_ranking_full$gap[u] <- as.numeric(uefa_country_ranking_full$overall[u]) -
  as.numeric(uefa_country_ranking_full$overall[u-1])

}


#Add flags

flags <- read_excel("flags.xlsx", col_names = FALSE)
colnames(flags) <- c("flag","country")

uefa_country_ranking_full <- merge(uefa_country_ranking_full,flags,all.x = TRUE)

uefa_country_ranking_full$country <- paste0(uefa_country_ranking_full$flag,uefa_country_ranking_full$country)


#Points gained
old_data_ranking_full <- read.csv("https://raw.githubusercontent.com/awp-finanznachrichten/uefa_ranking/master/Output/uefa_country_ranking_full.csv",encoding = "UTF-8")
old_data_ranking_full <- old_data_ranking_full[,1:3]
colnames(old_data_ranking_full) <- c("rank_old","country","current_points_old")


#Save old ranking with date
save(old_data_ranking_full,file=paste0("Old_data/",Sys.Date(),"_old_data_ranking_full.rdata"))

#Load
if (weekdays(Sys.Date()) == "Mittwoch") {
load(paste0("Old_data/",Sys.Date()-1,"_old_data_ranking_full.rdata"))

}

if (weekdays(Sys.Date()) == "Donnerstag") {
  load(paste0("Old_data/",Sys.Date()-2,"_old_data_ranking_full.rdata"))
  
}

if (weekdays(Sys.Date()) == "Freitag") {
  load(paste0("Old_data/",Sys.Date()-3,"_old_data_ranking_full.rdata"))
  
}



uefa_country_ranking_full <- merge(uefa_country_ranking_full,old_data_ranking_full)

uefa_country_ranking_full$gained <- as.numeric(uefa_country_ranking_full$overall)-as.numeric(uefa_country_ranking_full$current_points_old)


#Compare with last rank
uefa_country_ranking_full$rank <- paste0(uefa_country_ranking_full$rank,".",
                                    "(",gsub("[(].*","",uefa_country_ranking_full$rank_old),")") #Punkt entfernen

uefa_country_ranking_full$overall <- as.numeric(uefa_country_ranking_full$overall)


#Tidy it
uefa_country_ranking_full <- uefa_country_ranking_full[order(-uefa_country_ranking_full$overall),]

uefa_country_ranking_full <- uefa_country_ranking_full[,c(2,1,8,14,10,9)]
colnames(uefa_country_ranking_full) <- c("rank","country","current points","points gained",
                                    "gap to team ahead","teams remaining")

print(uefa_country_ranking_full)


write.csv(uefa_country_ranking_full,"Output/uefa_country_ranking_full.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


###Make Access List
access_list <- read_excel("access_list.xlsx", col_names = TRUE)

access_list$Country <- uefa_country_ranking_full$country

access_list <- access_list[,c(10,2:9)]

write.csv(access_list,"Output/access_list.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

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
dw_edit_chart("6dhKQ", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("6dhKQ")

#Update Datawrapper
dw_edit_chart("UFlq7", intro=paste0("Based on the current standing in the UEFA country ranking. Last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("UFlq7")


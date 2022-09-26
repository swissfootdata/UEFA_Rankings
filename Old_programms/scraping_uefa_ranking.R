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


uefa_country_ranking <- data.frame(1,2,3,4,5,6,7,8,9)
names(uefa_country_ranking) <- c("rank","country","17/18","18/19","19/20","20/21","21/22","overall","teams")
repeat {

country <- uefa_table[1:9]
names(country) <- c("rank","country","17/18","18/19","19/20","20/21","21/22","overall","teams")
uefa_country_ranking <- rbind(uefa_country_ranking,country)
uefa_table <- uefa_table[-(1:9)]

if (length(uefa_table) < 9) {
  
  break
}

}

uefa_country_ranking <- uefa_country_ranking[-1,]
uefa_country_ranking$teams <- gsub(" ","",uefa_country_ranking$teams)
uefa_country_ranking$country <- gsub("_"," ",uefa_country_ranking$country)

for (i in 1:nrow(uefa_country_ranking)) {
  
  if (nchar(uefa_country_ranking$teams[i]) == 1) {
    
    uefa_country_ranking$teams[i] <- paste0("0/",uefa_country_ranking$teams[i])
    
  }
  
}


#colnames(uefa_country_ranking) <- c("")

###Top 7
uefa_country_ranking_top7 <- uefa_country_ranking[1:8,] 

#Calculate gap
uefa_country_ranking_top7$gap_4 <- as.numeric(uefa_country_ranking_top7$overall) -
  as.numeric(uefa_country_ranking_top7$overall[4])
uefa_country_ranking_top7$gap_8 <- as.numeric(uefa_country_ranking_top7$overall) -
  as.numeric(uefa_country_ranking_top7$overall[8])

uefa_country_ranking_top7 <- uefa_country_ranking_top7[-8,]

#Wappen
library(readxl)
flags <- read_excel("flags.xlsx", col_names = FALSE)
colnames(flags) <- c("flag","country")

uefa_country_ranking_top7 <- merge(uefa_country_ranking_top7,flags,all.x = TRUE)

uefa_country_ranking_top7$country <- paste0(uefa_country_ranking_top7$flag,uefa_country_ranking_top7$country)


#Points gained
old_data_ranking_top7 <- read.csv("https://raw.githubusercontent.com/awp-finanznachrichten/uefa_ranking/master/Output/uefa_country_ranking_top7.csv",encoding = "UTF-8")
old_data_ranking_top7 <- old_data_ranking_top7[,1:3]
colnames(old_data_ranking_top7) <- c("rank_old","country","current_points_old")

#Save old ranking with date
save(old_data_ranking_top7,file=paste0("Old_data/",Sys.Date(),"_old_data_ranking_top7.rdata"))

#Load
if (weekdays(Sys.Date()) == "Mittwoch") {
  load(paste0("Old_data/",Sys.Date()-1,"_old_data_ranking_top7.rdata"))
  
}

if (weekdays(Sys.Date()) == "Donnerstag") {
  load(paste0("Old_data/",Sys.Date()-2,"_old_data_ranking_top7.rdata"))
  
}

if (weekdays(Sys.Date()) == "Freitag") {
  load(paste0("Old_data/",Sys.Date()-3,"_old_data_ranking_top7.rdata"))
  
}


uefa_country_ranking_top7 <- merge(uefa_country_ranking_top7,old_data_ranking_top7)

uefa_country_ranking_top7$gained <- as.numeric(uefa_country_ranking_top7$overall)-as.numeric(uefa_country_ranking_top7$current_points_old)


#Compare with last rank
uefa_country_ranking_top7$rank <- paste0(uefa_country_ranking_top7$rank,".",
                                    "(",gsub("[(].*","",uefa_country_ranking_top7$rank_old),")") #Punkt entfernen

#Tidy it
uefa_country_ranking_top7$overall <- as.numeric(uefa_country_ranking_top7$overall)
uefa_country_ranking_top7 <- uefa_country_ranking_top7[order(-uefa_country_ranking_top7$overall),]


uefa_country_ranking_top7 <- uefa_country_ranking_top7[,c(2,1,8,15,10,11,9)]
colnames(uefa_country_ranking_top7) <- c("rank","country","current points","points gained",
                                    "gap to 4th place","gap to 8th place","teams remaining")


write.csv(uefa_country_ranking_top7,"Output/uefa_country_ranking_top7.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")


###Fight for Top 11 and Top 15

#uefa_country_ranking$teams <- sub("\\/.*", "", uefa_country_ranking$teams)
uefa_country_ranking <- uefa_country_ranking[8:23,]

#Calculate gap
uefa_country_ranking$gap_11 <- as.numeric(uefa_country_ranking$overall) -
  as.numeric(uefa_country_ranking$overall[4])
uefa_country_ranking$gap_15 <- as.numeric(uefa_country_ranking$overall) -
  as.numeric(uefa_country_ranking$overall[8])

#Wappen
uefa_country_ranking <- merge(uefa_country_ranking,flags,all.x = TRUE)
uefa_country_ranking$country <- paste0(uefa_country_ranking$flag,uefa_country_ranking$country)


#Points gained
old_data_ranking <- read.csv("https://raw.githubusercontent.com/awp-finanznachrichten/uefa_ranking/master/Output/uefa_country_ranking.csv",encoding = "UTF-8")
old_data_ranking <- old_data_ranking[,1:3]
colnames(old_data_ranking) <- c("rank_old","country","current_points_old")

#Save old ranking with date
save(old_data_ranking,file=paste0("Old_data/",Sys.Date(),"_old_data_ranking.rdata"))

#Load
if (weekdays(Sys.Date()) == "Mittwoch") {
load(paste0("Old_data/",Sys.Date()-1,"_old_data_ranking.rdata"))

}

if (weekdays(Sys.Date()) == "Donnerstag") {
  load(paste0("Old_data/",Sys.Date()-2,"_old_data_ranking.rdata"))
  
}

if (weekdays(Sys.Date()) == "Freitag") {
  load(paste0("Old_data/",Sys.Date()-3,"_old_data_ranking.rdata"))
  
}


uefa_country_ranking <- merge(uefa_country_ranking,old_data_ranking)

uefa_country_ranking$gained <- as.numeric(uefa_country_ranking$overall)-as.numeric(uefa_country_ranking$current_points_old)


#Compare with last rank
uefa_country_ranking$rank <- paste0(uefa_country_ranking$rank,".",
                                    "(",gsub("[(].*","",uefa_country_ranking$rank_old),")") #Punkt entfernen

#Tidy it
uefa_country_ranking$overall <- as.numeric(uefa_country_ranking$overall)
uefa_country_ranking <- uefa_country_ranking[order(-uefa_country_ranking$overall),]


uefa_country_ranking <- uefa_country_ranking[,c(2,1,8,15,10,11,9)]
colnames(uefa_country_ranking) <- c("rank","country","current points","points gained",
                                    "gap to 11th place","gap to 15th place","teams remaining")

print(uefa_country_ranking)

write.csv(uefa_country_ranking,"Output/uefa_country_ranking.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

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
dw_edit_chart("J6Mna", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("J6Mna")

dw_edit_chart("xlWOs", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("xlWOs")



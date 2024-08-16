library(rvest)
library(stringr)
library(XML)
library(RSelenium)
library(git2r)
library(DatawRappr)
library(readxl)
library(dplyr)
library(readr)
library(httr)

#setwd("C:/Users/simon/Onedrive/Fussballdaten/uefa_ranking")
setwd("C:/Users/Administrator/Desktop/UEFA_Rankings")

#Load Token
#token <- read.csv("C:/Users/simon/OneDrive/Github_Token/token.txt",header=FALSE)[1,1]
token <- read.csv("C:/Users/Administrator/Desktop/Github_Token/token.txt",header=FALSE)[1,1]

#Load functions
source("ranking_funktionen.R",encoding = "UTF-8")

#repeat {
#Sys.sleep(10)

#Stop Geckodriver
try(system("taskkill /F /IM geckodriver.exe"))

#Stop Java-Process
try(system("taskkill /F /IM java.exe"))

#Check Update Time
driver <- RSelenium::rsDriver(port= 4570L, browser = "firefox",chromever = NULL, phantomver = NULL)
remote_driver <- driver[["client"]]
  
current_date <- Sys.Date()
current_day <- as.numeric(format(Sys.Date(),"%d"))

remote_driver$navigate("https://kassiesa.net/uefa/data/method5/crank2025.html")
output <- remote_driver$findElement(using="class",value="flex-container")
text_datum <- output$getElementText()[[1]]
text_datum <- strsplit(text_datum,"\n")[[1]][3]

#day <- gsub(".*Last update:","",text_datum)
#day <- parse_number(day)

#Close browser
remote_driver$close()

#Close server
driver[["server"]]$stop()

#Stop Geckodriver
try(system("taskkill /F /IM geckodriver.exe"))

#Stop Java-Process
try(system("taskkill /F /IM java.exe"))

#Letztes Update laden
last_update <- read.delim("last_update.txt",header=FALSE)

monday_check <- weekdays(Sys.Date()) == "Montag" & as.numeric(format(Sys.time(),"%H")) == 6

if (last_update != text_datum ||
    monday_check == TRUE) {
print("Aktuelle Daten gefunden")

#Update Ranking Data
source("ranking_scraping_new.R",encoding = "UTF-8")
  
#Stop Geckodriver
try(system("taskkill /F /IM geckodriver.exe"))
  
#Stop Java-Process
try(system("taskkill /F /IM java.exe"))  

#Update Team Data
source("ranking_teams_scraping.R",encoding = "UTF-8")

#Update Maps
source("create_maps.R",encoding = "UTF-8")
  
#Update Datawrapper
source("update_datawrapper.R",encoding = "UTF-8")
  
git2r::config(user.name = "swissfootdata",user.email = "swissfootballdata@bluewin.ch")
invisible(git2r::cred_token(token))
gitpull()
gitadd()
gitcommit()
gitpush()  
  
#Update Club Owner Data
#source("update_club_owner_data.R",encoding = "UTF-8")
    
#Store Last update
cat(text_datum,file="last_update.txt")

#git2r::config(user.name = "swissfootdata",user.email = "swissfootballdata@bluewin.ch")
#invisible(git2r::cred_token(token))
#gitpull()
#gitadd()
#gitcommit()
#gitpush()

} else {
print("Noch keine aktuellen Daten gefunden")  
}  


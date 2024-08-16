r <- GET("https://kassiesa.net/uefa/data/method5/crank2025.html")
content <- content(r)
text_all <- content %>%
  html_elements(".countrygroup") %>%
  html_text()

text_all <- gsub("\n"," ",text_all)
text_all <- gsub("/ ","/",text_all)

#Replace Countries
text_all <- gsub("Czech Republic","Czech_Republic",text_all)
text_all <- gsub("Northern Ireland","Northern_Ireland",text_all)
text_all <- gsub("Faroe Island","Faroe_Island",text_all)
text_all <- gsub("North Macedonia","North_Macedonia",text_all)
text_all <- gsub("San Marino","San_Marino",text_all)
text_all <- strsplit(text_all," ")[[1]]
text_all <- text_all[text_all != ""]

uefa_table <- text_all[-c(1:9)]

uefa_country_ranking_full <- data.frame(1,2,3,4,5,6,7,8,9)
names(uefa_country_ranking_full) <- c("rank","country","20/21","21/22","22/23","23/24","24/25","overall","teams")
repeat {
  
  country <- uefa_table[1:9]
  names(country) <- c("rank","country","20/21","21/22","22/23","23/24","24/25","overall","teams")
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

#Add flags
flags <- read_excel("Data/flags.xlsx", col_names = FALSE)
colnames(flags) <- c("flag","country")

uefa_country_ranking_full <- merge(uefa_country_ranking_full,flags,all.x = TRUE)

uefa_country_ranking_full$country <- paste0(uefa_country_ranking_full$flag,uefa_country_ranking_full$country)


#Points gained
old_data_ranking_full <- read.csv("https://raw.githubusercontent.com/swissfootdata/UEFA_Rankings/master/Output/uefa_ranking.csv",encoding = "UTF-8")
old_data_ranking_full <- old_data_ranking_full[,c(1,2,5)]
colnames(old_data_ranking_full) <- c("rank_old","country","current_points_old")

#Adapt if past midnight
if (as.numeric(format(Sys.time(),"%H")) < 2) {
current_date <- current_date-1
}

#Save old ranking with date
save(old_data_ranking_full,file=paste0("Old_data/",current_date,"_old_data_ranking_full.rdata"))

#Load
if (weekdays(current_date) == "Dienstag") {
  load(paste0("Old_data/",current_date-1,"_old_data_ranking_full.rdata"))
  
}

if (weekdays(current_date) == "Mittwoch") {
  load(paste0("Old_data/",current_date-2,"_old_data_ranking_full.rdata"))
  
}

if (weekdays(current_date) == "Donnerstag") {
  load(paste0("Old_data/",current_date-3,"_old_data_ranking_full.rdata"))
  
}

if (weekdays(current_date) == "Freitag") {
  load(paste0("Old_data/",current_date-4,"_old_data_ranking_full.rdata"))
}

if (weekdays(current_date) == "Samstag") {
  load(paste0("Old_data/",current_date-5,"_old_data_ranking_full.rdata"))
}

if (weekdays(current_date) == "Sonntag") {
  load(paste0("Old_data/",current_date-6,"_old_data_ranking_full.rdata"))
}

uefa_country_ranking_full <- merge(uefa_country_ranking_full,old_data_ranking_full)
uefa_country_ranking_full$gained <- as.numeric(uefa_country_ranking_full$overall)-as.numeric(uefa_country_ranking_full$current_points_old)


#Compare with last rank
uefa_country_ranking_full$rank <- paste0(uefa_country_ranking_full$rank,".",
                                         "(",gsub("[(].*","",uefa_country_ranking_full$rank_old),")") #Punkt entfernen

uefa_country_ranking_full$overall <- as.numeric(uefa_country_ranking_full$overall)



#Tidy it
uefa_country_ranking_full <- uefa_country_ranking_full[order(-uefa_country_ranking_full$overall),]


uefa_country_ranking_full <- uefa_country_ranking_full[,c(2,1,13,7,8,9)]
colnames(uefa_country_ranking_full) <- c("rank","country","points gained","points season 24/25","points overall","teams remaining")

write.csv(uefa_country_ranking_full,"Output/uefa_ranking.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

print(uefa_country_ranking_full)

###Make Access List
access_list <- read_excel("Data/access_list.xlsx", col_names = TRUE)
access_list$Country <- uefa_country_ranking_full$country
access_list <- access_list[,c(10,2:9)]

#Liechtenstein Cleanup
access_list$Champion[which(grepl("Liechtenstein",access_list$Country))] <- NA
access_list$`Team 2`[which(grepl("Liechtenstein",access_list$Country))] <- NA
access_list$`Team 3`[which(grepl("Liechtenstein",access_list$Country))] <- NA
access_list$Teams[which(grepl("Liechtenstein",access_list$Country))] <- 1

write.csv(access_list,"Output/ranking_access_list.csv", na = "", row.names = FALSE, fileEncoding = "UTF-8")

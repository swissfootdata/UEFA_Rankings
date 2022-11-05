#Update Datawrapper
datawrapper_auth("fYNHJEgLlCPgMC8hO0Bxm7j3SG2cOGCPnIJRc5RCVc72zYBFaMxGYIOY081zYaeq", overwrite = TRUE)

#Ranking Live Standings
dw_data_to_chart(uefa_country_ranking_full,"VViWE")

###Adapt countries with no teams to red
#Which countries have no teams left?
check_no_teams <- which(grepl("0",uefa_country_ranking_full$`teams remaining`))-1

#Get metadata and adapt
chart_metadata <- dw_retrieve_chart_metadata("VViWE")
adapted_list <- chart_metadata[["content"]][["metadata"]][["visualize"]]

#Determine if row is red (no teams left) or white
for (r in 0:54) {
row_name <- paste0("row-",r)

if (sum(r == check_no_teams) == 1) {
adapted_list$rows[[row_name]]$style$background <- "#c71e1d"
} else {  
adapted_list$rows[[row_name]]$style$background <- "#FFFFFF"
}
}

dw_edit_chart("VViWE", 
              intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")),
              visualize = adapted_list
              )
dw_publish_chart("VViWE")

#Most successful team current season
dw_data_to_chart(complete_table_season,"awxBw")
dw_edit_chart("awxBw", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("awxBw")

#Teams overview current season
dw_data_to_chart(complete_table_season_adapt,"M08Ux")
dw_edit_chart("M08Ux", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("M08Ux")

#Power map current season
dw_data_to_chart(ranking_map_data,"oSHCF")
dw_edit_chart("oSHCF", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("oSHCF")

#Power map current season: Teams overview
dw_data_to_chart(team_data_with_coordinates_season,"Oh2nr")
dw_edit_chart("Oh2nr", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("Oh2nr")

#Teams overview last five seasons
dw_data_to_chart(complete_table,"v88MJ")
dw_edit_chart("v88MJ", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("v88MJ")

#Most successful teams in the last five seasons
dw_data_to_chart(complete_table,"qw90m")
dw_edit_chart("qw90m", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("qw90m")

#Power map last five seasons
dw_data_to_chart(ranking_map_data,"xFGfR")
dw_edit_chart("xFGfR", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("xFGfR")

#Power map last five seasons: Teams overview
dw_data_to_chart(team_data_with_coordinates,"zXf6G")
dw_edit_chart("zXf6G", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("zXf6G")

#Teams with largest share of overall points in their country
dw_data_to_chart(complete_table,"up5me")
dw_edit_chart("up5me", intro=paste0("last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("up5me")


#Access List
dw_edit_chart("f8BVO", intro=paste0("Based on the current standing in the UEFA country ranking. Last update: ",format(Sys.time(),"%d.%m.%Y %H:%M Uhr")))
dw_publish_chart("f8BVO")


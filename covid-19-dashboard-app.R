# Load libraries
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(scales)
library(ggiraph)
library(stringr)
library(reshape2)
library(gridExtra)
library(readxl)
library(broom)
library(RColorBrewer)

# Define plotting theme and aesthetics
my_theme = function () { 
  theme_bw() + 
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position = "bottom",
          strip.background = element_rect(fill = 'white', colour = 'white'))
}

# Extract legend from ggplot object
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Define vector of colors of length n
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Define nations and group colors
nations = c("England", "Scotland", "Wales", "Northern Ireland")
group_colors = gg_color_hue(length(nations))
names(group_colors) = nations

#--------------------------------------------------
# Cases by date
#--------------------------------------------------

# Load data on daily and cumulative cases by specimen date and nation
url = "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22%7D&format=csv"
daily_cases_by_nation = fread(url)
daily_cases_by_nation$date = as.Date(daily_cases_by_nation$date)
daily_cases_by_nation$areaName = reorder(daily_cases_by_nation$areaName, daily_cases_by_nation$newCasesBySpecimenDate)

plot_cases_timeline = function(date_range){
  
  df = daily_cases_by_nation[daily_cases_by_nation$date >= min(date_range) & daily_cases_by_nation$date <= max(date_range), ]
  
  # Plot the cases over time (line for cumulative cases and bars for daily cases)
  plot1 = ggplot() + 
     geom_bar_interactive(data = df, aes(x = date, y = newCasesBySpecimenDate, fill = areaName,
                                         tooltip = paste0(date, ": ", newCasesBySpecimenDate, " new cases in ", areaName)), 
                          stat = "identity", position = "stack") +
    labs(x = NULL, y = "Daily new cases", fill = NULL, caption = "") + 
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    ggtitle(str_wrap("Daily new COVID-19 cases by specimen date and UK nation", 25)) + 
    my_theme() +
    scale_fill_manual(values = group_colors) + 
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  
  # Plot the cases over time (line for cumulative cases and bars for daily cases)
  plot2 = ggplot() + 
     geom_bar_interactive(data = df, aes(x = date, y = cumCasesBySpecimenDate, fill = areaName,
                                           tooltip = paste0(date, ": ", cumCasesBySpecimenDate, " cumulative cases in ", areaName)), 
                            stat = "identity", position = "stack") +
    labs(x = NULL, y = "Cumulative cases", fill = NULL, 
         caption = "Source: Author's analysis of data obtained from Public Health England") + 
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    ggtitle(str_wrap("Cumulative COVID-19 cases by specimen date and UK nation", 25)) + 
    my_theme() +
    scale_fill_manual(values = group_colors) + 
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  
  # Combine the plots into one with shared legend
  leg = g_legend(plot1)
  plot = grid.arrange(arrangeGrob(plot1 + theme(legend.position = "none"),
                                  plot2 + theme(legend.position = "none"),
                                  ncol = 2), leg, heights = c(10, 1))
  
  return(plot)
}


#--------------------------------------------------
# Deaths by date
#--------------------------------------------------

# Deaths by date of death
url = "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newDeaths28DaysByDeathDate%22:%22newDeaths28DaysByDeathDate%22,%22cumDeaths28DaysByDeathDate%22:%22cumDeaths28DaysByDeathDate%22%7D&format=csv"
daily_deaths_by_nation = fread(url)
daily_deaths_by_nation$date = as.Date(daily_deaths_by_nation$date)
daily_deaths_by_nation$areaName = reorder(daily_deaths_by_nation$areaName, daily_deaths_by_nation$newDeaths28DaysByDeathDate)

plot_deaths_timeline = function(date_range){
  
  df = daily_deaths_by_nation[daily_deaths_by_nation$date >= min(date_range) & daily_deaths_by_nation$date <= max(date_range), ]
  
  # Plot the deaths over time (line for cumulative cases and bars for daily cases)
  plot1 = ggplot() + 
    geom_bar_interactive(data = df, aes(x = date, y = newDeaths28DaysByDeathDate, fill = areaName,
                                        tooltip = paste0(date, ": ", newDeaths28DaysByDeathDate, " new deaths in ", areaName)), 
                         stat = "identity", position = "stack") +
    labs(x = NULL, y = "Daily new deaths", fill = NULL, caption = "") + 
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    ggtitle(str_wrap("Daily new deaths within 28 days of positive COVID-19 test by death date and UK nation", 25)) + 
    my_theme() +
    scale_fill_manual(values = group_colors) + 
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  
  # Plot the deaths over time (line for cumulative cases and bars for daily cases)
  plot2 = ggplot() + 
    geom_bar_interactive(data = df, aes(x = date, y = cumDeaths28DaysByDeathDate, fill = areaName,
                                          tooltip = paste0(date, ": ", cumDeaths28DaysByDeathDate, " cumulative deaths in", areaName)), 
                           stat = "identity", position = "stack") +
    labs(x = NULL, y = "Cumulative deaths", fill = NULL, 
         caption = "Source: Author's analysis of data obtained from Public Health England") + 
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    ggtitle(str_wrap("Cumulative deaths within 28 days of positive COVID-19 test by death date and UK nation", 25)) + 
    my_theme() +
    scale_fill_manual(values = group_colors) + 
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  
  # Combine the plots into one with shared legend
  leg = g_legend(plot1)
  plot = grid.arrange(arrangeGrob(plot1 + theme(legend.position = "none"),
                                  plot2 + theme(legend.position = "none"),
                                  ncol = 2), leg, heights = c(10, 1))
  
  return(plot)
  
}

#--------------------------------------------------
# Hospitalizations by date
#--------------------------------------------------

# Patients admitted to hospital
url = "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newAdmissions%22:%22newAdmissions%22,%22cumAdmissions%22:%22cumAdmissions%22%7D&format=csv"
daily_hospital_admits_by_nation = fread(url)
daily_hospital_admits_by_nation$date = as.Date(daily_hospital_admits_by_nation$date)
daily_hospital_admits_by_nation$areaName = reorder(daily_hospital_admits_by_nation$areaName, daily_hospital_admits_by_nation$newAdmissions)

plot_hospital_timeline = function(date_range){
  
  df = daily_hospital_admits_by_nation[daily_hospital_admits_by_nation$date >= min(date_range) & daily_hospital_admits_by_nation$date <= max(date_range), ]
  
  # Plot the deaths over time (line for cumulative cases and bars for daily cases)
  plot1 = ggplot() + 
    geom_bar_interactive(data = df, aes(x = date, y = newAdmissions, fill = areaName,
                                        tooltip = paste0(date, ": ", newAdmissions, " new admissions in ", areaName)), 
                         stat = "identity", position = "stack") +
    labs(x = NULL, y = "Daily new admissions", fill = NULL, caption = "") + 
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    ggtitle(str_wrap("Daily COVID-19 hospital admissions by date and UK nation", 25)) + 
    my_theme() +
    scale_fill_manual(values = group_colors) + 
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  
  # Plot the deaths over time (line for cumulative cases and bars for daily cases)
  plot2 = ggplot() + 
    geom_bar_interactive(data = df, aes(x = date, y = cumAdmissions, fill = areaName, 
                                          tooltip = paste0(date, ": ", cumAdmissions, " cumulative admissions in ", areaName)), 
                           stat = "identity", position = "stack") +
    labs(x = NULL, y = "Cumulative admissions", fill = NULL, 
         caption = "Source: Author's analysis of data obtained from Public Health England") + 
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    ggtitle(str_wrap("Cumulative COVID-19 hospital admissions by date and UK nation", 25)) + 
    my_theme() +
    scale_fill_manual(values = group_colors) + 
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  
  # Combine the plots into one with shared legend
  leg = g_legend(plot1)
  plot = grid.arrange(arrangeGrob(plot1 + theme(legend.position = "none"),
                                  plot2 + theme(legend.position = "none"),
                                  ncol = 2), leg, heights = c(10, 1))
  
  return(plot)
  
}
#--------------------------------------------------
# Tests by date
#--------------------------------------------------

# PCR and antibody tests
url = "https://api.coronavirus.data.gov.uk/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22plannedPCRCapacityByPublishDate%22:%22plannedPCRCapacityByPublishDate%22,%22plannedAntibodyCapacityByPublishDate%22:%22plannedAntibodyCapacityByPublishDate%22,%22newPCRTestsByPublishDate%22:%22newPCRTestsByPublishDate%22,%22cumPCRTestsByPublishDate%22:%22cumPCRTestsByPublishDate%22,%22newAntibodyTestsByPublishDate%22:%22newAntibodyTestsByPublishDate%22,%22cumAntibodyTestsByPublishDate%22:%22cumAntibodyTestsByPublishDate%22%7D&format=csv"
test = fread(url)
test$date = as.Date(test$date)
test$cumAntibodyTestsByPublishDate = as.integer(test$cumAntibodyTestsByPublishDate)
test_cum = test[, c("areaName", "date", "cumPCRTestsByPublishDate", "cumAntibodyTestsByPublishDate")]
test_new = test[, c("areaName", "date", "newPCRTestsByPublishDate", "newAntibodyTestsByPublishDate")]
melt_cum = melt(test_cum, id.vars = c("areaName", "date"))
melt_new = melt(test_new, id.vars = c("areaName", "date"))
melt_new$variable = gsub("TestsByPublishDate", "", gsub("new", "", melt_new$variable))
melt_cum$variable = gsub("TestsByPublishDate", "", gsub("cum", "", melt_cum$variable))

plot_tests_timeline = function(date_range){
  
  df_new = melt_new[melt_new$date >= min(date_range) & melt_new$date <= max(date_range), ]
  df_cum = melt_cum[melt_cum$date >= min(date_range) & melt_cum$date <= max(date_range), ]
  
  plot1 = ggplot() + 
    geom_bar_interactive(data = df_new, aes(x = date, y = value, fill = variable,
                                            tooltip = paste0(date, ": ", value, " new tests (", variable, ")")), 
                         stat = "identity", position = "stack") +
    labs(x = NULL, y = "Daily new tests", fill = "Test type", color = "Test type", shape = "Test type", caption = "") + 
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    ggtitle(str_wrap("Daily COVID-19 tests in the United Kingdom by date and test type", 25)) + 
    my_theme() + 
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  
  plot2 = ggplot() + 
    geom_bar_interactive(data = df_cum, aes(x = date, y = value, fill = variable, 
                                              tooltip = paste0(date, ": ", value, " cumulative tests (", variable, ")")), 
                           stat = "identity", position = "stack") +
    labs(x = NULL, y = "Cumulative tests", fill = "Test type", 
         caption = "Source: Author's analysis of data obtained from Public Health England") + 
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    ggtitle(str_wrap("Cumulative COVID-19 tests in the United Kingdom by date and test type", 25)) + 
    my_theme() + 
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  
  # Combine the plots into one with shared legend
  leg = g_legend(plot1)
  plot = grid.arrange(arrangeGrob(plot1 + theme(legend.position = "none"),
                                  plot2 + theme(legend.position = "none"),
                                  ncol = 2), leg, heights = c(10, 1))
  
  return(plot)
  
}

#--------------------------------------------------
# Map of cases by region
#--------------------------------------------------

url_start = "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=region;areaName="
url_end = "&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22%7D&format=csv"
url_regions = c("East%2520Midlands", "East%2520of%2520England", "London", "North%2520East", "North%2520West", "South%2520East",
                "South%2520West", "West%2520Midlands", "Yorkshire%2520and%2520The%2520Humber")
urls = paste0(url_start, url_regions, url_end)
cases_by_region = rbind.fill(lapply(as.list(urls), fread))

if (dir.exists("uk-geo-data") == FALSE) { dir.create("uk-geo-data") }
download.file("https://opendata.arcgis.com/datasets/4fcca2a47fed4bfaa1793015a18537ac_4.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D", destfile = "uk-geo-data/uk-geo-regions.zip")
unzip("uk-geo-data/uk-geo-regions.zip", exdir = "uk-geo-data")
uk_geo_region = rgdal::readOGR(dsn = "uk-geo-data", "Regions_December_2017_Ultra_Generalised_Clipped_Boundaries_in_England")
mapdata_region <- tidy(uk_geo_region, region = "rgn17nm")
# matchdf = read_excel('matchdata.xlsx', sheet = "region")
# mapdata_region["region"] = matchdf$corona_data[match(mapdata_region$id, matchdf$geo_data)]
mapdata_region = merge(mapdata_region, cases_by_region, by.x = "id", by.y = "areaName")

plot_cases_region_map = function(current_date){
  
  mapdata = mapdata_region[mapdata_region$date == current_date, ]
  
  plot1 = ggplot() + 
    geom_polygon_interactive(data = mapdata, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = newCasesBySpecimenDate, group = group, 
                                 tooltip = sprintf("%s<br/>%s", id, newCasesBySpecimenDate))) + 
    scale_fill_gradientn(colours = rev(brewer.pal(5, "RdYlBu")), na.value = 'white') + 
    labs(fill = "New cases", color = NULL, x = NULL, y = NULL, caption = "") + 
    ggtitle(str_wrap(paste0("Number of new COVID-19 cases by England region on ", current_date), 25)) + 
    my_theme() + 
    coord_fixed() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.width = unit(0.7, "cm"))
  
  plot2 = ggplot() + 
    geom_polygon_interactive(data = mapdata, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = cumCasesBySpecimenDate, group = group, 
                                 tooltip = sprintf("%s<br/>%s", id, cumCasesBySpecimenDate))) + 
    scale_fill_gradientn(colours = rev(brewer.pal(5, "RdYlBu")), na.value = 'white', labels = label_scientific()) + 
    labs(fill = "Cumulative cases", color = NULL, x = NULL, y = NULL,
         caption = "Source: Author's analysis of data obtained from Public Health England") + 
    ggtitle(str_wrap(paste0("Number of cumulative COVID-19 cases by England region on ", current_date), 25)) + 
    my_theme() + 
    coord_fixed() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.width = unit(0.75, "cm"))
  
  plot = grid.arrange(plot1, plot2, ncol = 2)
  
  return(plot)
}

#------------------------------------------------------------------------------------
# Shiny app
#------------------------------------------------------------------------------------

ui = shinyUI(
  
  navbarPage("COVID-19 in the United Kingdom",
             
             tabPanel("Timeline of new and cumulative cases by date and UK nation",
                      sidebarPanel(
                        dateRangeInput("daterange", "Supply a date range for the timeline:",
                                       start = min(as.character(daily_cases_by_nation$date)),
                                       end   = max(as.character(daily_cases_by_nation$date))),
                        
                        p("Don't forget to hover over the plots with your mouse to see the values!"),
                        br(),
                        p("All data used for this dashboard are from Public Health England (PHE). 
                        I am by no means trying to make any implications whatsoever about 
                        anything related to COVID-19 and do not take responsibility for any 
                        misinterpretations; I am merely visually displaying data published by PHE.")
                      ),
                      mainPanel(girafeOutput("timeline_cases"))
             ),
             
             tabPanel("Timeline of new and cumulative deaths by date and UK nation",
                      sidebarPanel(
                        dateRangeInput("daterange", "Supply a date range for the timeline:",
                                       start = min(as.character(daily_deaths_by_nation$date)),
                                       end   = max(as.character(daily_deaths_by_nation$date))),
                        
                        p("Don't forget to hover over the plots with your mouse to see the values!"),
                        br(),
                        p("All data used for this dashboard are from Public Health England (PHE). 
                        I am by no means trying to make any implications whatsoever about 
                        anything related to COVID-19 and do not take responsibility for any 
                        misinterpretations; I am merely visually displaying data published by PHE.")
                      ),
                      mainPanel(girafeOutput("timeline_deaths"))
             ),
             
             tabPanel("Timeline of new and cumulative hospital admissions by date and UK nation",
                      sidebarPanel(
                        dateRangeInput("daterange", "Supply a date range for the timeline:",
                                       start = min(as.character(daily_hospital_admits_by_nation$date)),
                                       end   = max(as.character(daily_hospital_admits_by_nation$date))),
                        
                        p("Don't forget to hover over the plots with your mouse to see the values!"),
                        br(),
                        p("All data used for this dashboard are from Public Health England (PHE). 
                        I am by no means trying to make any implications whatsoever about 
                        anything related to COVID-19 and do not take responsibility for any 
                        misinterpretations; I am merely visually displaying data published by PHE.")
                      ),
                      mainPanel(girafeOutput("timeline_hospital"))
             ),
             
             tabPanel("Timeline of new and cumulative tests by date and test type",
                      sidebarPanel(
                        dateRangeInput("daterange", "Supply a date range for the timeline:",
                                       start = min(as.character(test$date)),
                                       end   = max(as.character(test$date))),
                        
                        p("Don't forget to hover over the plots with your mouse to see the values!"),
                        br(),
                        p("All data used for this dashboard are from Public Health England (PHE). 
                        I am by no means trying to make any implications whatsoever about 
                        anything related to COVID-19 and do not take responsibility for any 
                        misinterpretations; I am merely visually displaying data published by PHE.")
                      ),
                      mainPanel(girafeOutput("timeline_tests"))
             ),
             
             tabPanel("Map of new and cumulative cases in England by region",
                      sidebarPanel(
                        dateInput("date", "Supply a date for the map:", value = (Sys.Date() - 5)),
                        
                        p("Don't forget to hover over the plots with your mouse to see the values!"),
                        br(),
                        p("All data used for this dashboard are from Public Health England (PHE). 
                        I am by no means trying to make any implications whatsoever about 
                        anything related to COVID-19 and do not take responsibility for any 
                        misinterpretations; I am merely visually displaying data published by PHE.")
                      ),
                      mainPanel(girafeOutput("map_region"))
             )
             
  )
  
)

server = function(input, output) {
  
  output$timeline_tests <- renderGirafe({
    
    girafe(print(plot_tests_timeline(input$daterange)))
    
  })
  
  output$timeline_cases <- renderGirafe({
    
    girafe(print(plot_cases_timeline(input$daterange)))
    
  })
  
  output$timeline_deaths <- renderGirafe({
    
    girafe(print(plot_deaths_timeline(input$daterange)))
    
  })
  
  output$timeline_hospital <- renderGirafe({
    
    girafe(print(plot_hospital_timeline(input$daterange)))
    
  })
  
  output$timeline_tests <- renderGirafe({
    
    girafe(print(plot_tests_timeline(input$daterange)))
    
  })
  
  output$map_region <- renderGirafe({
    
    girafe(print(plot_cases_region_map(input$date)))
    
  })
  
  
}

shinyApp(ui = ui, server = server)



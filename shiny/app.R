library(shiny)
library(tidyverse)
library(shinydashboard)
library(viridis)
library(usmap)
library(maps)
library(rjson)
library(plotly)

unemploy = read_csv("Unemployment_renamed.csv") 
clean_unemploy =  
  dplyr::rename(unemploy, state = series_id) %>% 
  mutate(label = fct_inorder(label))
state = clean_unemploy %>% distinct(state) %>% pull()

gun = read_csv("mass shootings(all years).csv") %>% 
  janitor::clean_names() %>% 
  separate(incident_date, c("date", "month", "year"), sep = "-" ) %>% 
  mutate(year = as.numeric(year) + 2000) %>% 
  filter(year != 2017)

gun_state = gun %>%
  group_by(year, state) %>% 
  summarise(number_of_incident = n(), 
            sum_killed = sum(number_killed), 
            sum_injured = sum(number_injured)) %>% 
  replace(is.na(.), "District of Columbia") 

url <- 'https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_20m.json'
states <- fromJSON(file=url)


ui = navbarPage("US Overview",
                tabPanel("Mass Shootings Map",
                        fillPage(
                           tags$style(type = "text/css", "#usplot {height: calc(100vh - 80px) !important;}"),
                         sidebarPanel(
                           sliderInput(
                             "year_select", 
                             "Select year: ",
                             min = 2018, max = 2021, value = 2020), 
                           h6("The Mass Shootings Map allows users to toggle between different years pre- and post- Covid-19 (2018 - 2021), displaying the changes in frequency of mass shootings over time across states in the U.S. "), 
                           h6("States with missing data are shown in blank. "),
                           h6("The annual total numbers of killed and injured victims can be seen by hovering the cursor over specific state.")
                         ),
                         mainPanel(plotlyOutput("usplot")))), 
 
 
    tabPanel("Unemployment Rate Trends",
             fillPage(
               tags$style(type = "text/css", "#lineplot {height: calc(100vh - 150px) !important;}"),
                sidebarPanel(selectizeInput(
                   inputId = "state_select", 
                   label = "Select or type in state(s) of interest: ", 
                   choices = state, 
                   selected = "New York",
                   multiple = TRUE
                 ), 
                 h6("The Unemployment Rate Trends plot allows users to select states of interest, demonstrating the changes in unemployment rate over time in the U.S. from pre-covid (2017) to post-covid period (2021). "), 
                 h6("Multiple states can be selected at the same time for comparison. ")
                 ),
             mainPanel(plotlyOutput("lineplot")))
))


server = function(input,output){
    
    output$lineplot = renderPlotly({
    clean_unemploy %>% 
    filter(state %in% input$state_select) %>% 
        plot_ly(x = ~label, y = ~value, color = ~state, type = 'scatter', mode = 'lines') %>% 
        layout(
          title = "Unemployment Rate by Month from 2017-2021",
          xaxis = list(title = 'Time'),
          yaxis = list(title = 'Unemployment rate'))
  })
    
    
    output$usplot = renderPlotly({
    gun_state %>% 
      filter(year == input$year_select) %>% 
      mutate(text_label = str_c("Number of killed: ", sum_killed, '\nNumber of injured: ', sum_injured)) %>%
      plot_ly() %>% 
      add_trace(
        type = "choroplethmapbox",
        featureidkey = "properties.NAME", 
        geojson = states,
        locations= ~state,
        z=~number_of_incident,
        colorscale="Viridis", 
        # add text
        text = ~text_label,
        marker = list(line = list(
          width = 0),
          opacity = 0.75
        )
      ) %>% 
      layout(
        mapbox = list(
          style = "carto-positron",
          zoom = 2.3,
          center = list(lon = -95.71, lat = 37.09)), 
        title = "US Map of Mass Shooting",
        legend = list(x = 100, y = 0.5)) %>%   
      colorbar(title = "# Mass Shooting")
  })
}

shinyApp(ui, server)
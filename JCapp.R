

# load libraries
require(shinythemes)
require(shiny)
require(leaflet)
require(rgdal)
require(tidyverse)
require(spdplyr)
require(magrittr)
require(rgeos)
require(RColorBrewer)
require(httr)
require(stringi)
require(reshape2)
require(shinydashboard)
require(geosphere)
require(viridis)
require(plotly)
require(tools)

# set working directory
# setwd("~/Documents/Projects/Energy/")
setwd("C:/Users/jimmy/OneDrive/Documents/GitHub/Energy")
# setwd("C:/Users/lab/Documents/GitHub/Energy")

# create provincial data
source("create_data.R")

# create input vectors
years = 2005:2018
areas = prov_map@data$NAME
indicators = c("input","output","price")
commodities = c("wood","total_petroleum_products","light_fuel_oil","total_heavy_fuel_oil",
                "diesel","total_coal","natural_gas","uranium","methane","propane")

# create input lists
list_of_areas = lapply(paste0("<p style='color:black;font-family:Verdana,Geneva,sans-serif;font-weight:bold;font-size: 14px'>",
                                   areas,"</p>"),HTML)

list_of_indicators = lapply(paste0("<p style='color:black;font-family:Verdana,Geneva,sans-serif;font-weight:bold;font-size: 14px'>",
                                   indicators,"</p>"),HTML)

list_of_commodities = lapply(paste0("<p style='color:black;font-family:Verdana,Geneva,sans-serif;font-weight:bold;font-size: 14px'>",
                                    toTitleCase(gsub('_', ' ', commodities)),"</p>"),HTML)

# create user interface
ui <- dashboardPage(
  
  dashboardHeader(title = div(icon("fighter-jet"),icon("seedling"),icon("charging-station"))),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    tags$head(tags$style(HTML('
    
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #55579A;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #55579A;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #55579A;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #FFFFFF;
                              color: black;
                              }

                              '))),
    fluidRow(
      box(sliderInput("year",h6(''), min=2005, max=2018, value=c(2005,2018)),width = 3),
      box(selectInput("indicator", label = HTML('<FONT color="#55579A"><FONT size="4pt">Indicator'),
                 choices = indicators, selected='price'),width = 3),
      box(selectInput("commodity", label = HTML('<FONT color="#55579A"><FONT size="4pt">Commodity'),
                      choices = commodities, selected = 'wood'),width = 3),
      box(selectInput("province", label = HTML('<FONT color="#55579A"><FONT size="4pt">Province'),
                      choices = areas, selected='Ontario'),width = 3)),
    fluidRow(
      box(status = "primary",leafletOutput("plot"),width=12),
      box(title = "bubblebubble",status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("bubble"),width= 6),
      box(title = "InputOutput",status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("linegraph"),width = 6),
      infoBoxOutput("fuelselected")
      
    ))
)

# create server
server <- function(input, output) {
  
   output$plot <- renderLeaflet({
    
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>%
      setView(lng = -100.4, lat = 53, zoom = 4) %>%
      addProviderTiles(providers$Stamen.Watercolor,
                       options = providerTileOptions(opacity = 1))
    
  })
  
  selected_com_ind <- reactive({
    
    min_year = min(input$year)
    max_year = max(input$year)
    min_selection = str_c(input$commodity,'_',input$indicator,'_',min_year)
    max_selection = str_c(input$commodity,'_',input$indicator,'_',max_year)
    
    prov_map@data %<>% 
      mutate(selection=rowMeans(select(.,min_selection:max_selection),na.rm=TRUE)) %>%
      mutate(scaled_selection=scale(selection))
    
    prov_map
    
  })
  
  selected_prov_input <- reactive({

    min_year = min(input$year)
    max_year = max(input$year)
    selected_province = input$province

    subject_matter_2 %>%
      filter(province == selected_province & year %in% min_year:max_year) %>%
      mutate(year_opacity = ((year-1999)^3)/6859)
      # group_by(commodity) %>%
      # summarize(ins = round(mean(input),0), out = round(mean(output),0), price = round(mean(price,na.rm=TRUE),0))

  })
  
  output$bubble <- renderPlotly({
    
    selected_prov_input() %>%
      plot_ly(type = 'scatter', mode = 'markers', x = ~input, y = ~output, size = ~price, color = ~commodity,
              colors = viridis_pal(option = "D")(10),
              marker = list(sizeref=0.1), hoverinfo = 'text', 
              text=~paste('<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
                          '<b>Price: </b>$',format(price,big.mark=",",scientific=FALSE),'<br>',
                          '<b>Input: </b>',format(input,big.mark=",",scientific=FALSE),'Tonsslashkilolitres','<br>',
                          '<b>Output: </b>',format(output,big.mark=",",scientific=FALSE),'MgHours')) %>%
      layout(showlegend=FALSE)
    
  })
  
  observe({
    
    ccs_popup <- paste0('<strong>',selected_com_ind()$NAME,'</strong>',
                        "<br><strong>",input$commodity,": </strong>",
                        selected_com_ind()$selection, " t/kL/m[^3]")
    
    leafletProxy("plot") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = selected_com_ind(),
                  fillColor = ~colorBin("Reds", selection, 5)(selection),
                  color = "#BDBDC3",
                  fillOpacity = 0.5,
                  weight = 1) %>%
      addCircles(data=selected_com_ind(), lng = ~x, lat = ~y,
                 #color = ~colorBin("Blues", selection, 8)(selection),
                 fillOpacity = 0.6,
                 popup = ccs_popup,
                 weight = ~scaled_selection*50)
    
  })
  
  linegraph_reactive <- reactive({
    
    selected_province = input$province
    selected_commodity = input$commodity
    min_year = min(input$year)
    max_year = max(input$year)
    
    subject_matter_2 %>% filter(province == selected_province,
                                commodity %in% selected_commodity,
                                year %in% min_year:max_year)
  })
  
  output$linegraph <- renderPlotly({
    
    plot_ly(linegraph_reactive()) %>%
      add_trace(x= ~year, y= ~input,type = 'scatter', mode = 'lines', name= 'Input',
                marker = list(color = '#55579A'),
                hoverinfo = "text",
                text = ~paste(input,' t')) %>%
      add_trace(x= ~year, y = ~output, type = 'scatter', mode = 'lines', name = 'Output', yaxis = 'y2',
                line = list(color = '#4db8ff'),
                hoverinfo = "text",
                text = ~paste(output,'GWh')) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(range=c(0,~max(input)),side = 'left', title = 'Input (metric tonnes)', showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(range=c(0,~max(output)),side = 'right', overlaying = "y", title = 'Output in MWh', showgrid = FALSE, zeroline = FALSE))
    
  })
  
  output$fuelselected <- renderInfoBox({
    
    infoBox("Current fuel selected",paste0(input$commodity), icon("charging-station"), color = "blue")
    
  })
  
}

# run app
shinyApp(ui, server)



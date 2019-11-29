

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
# setwd("C:/Users/jimmy/OneDrive/Documents/GitHub/Energy")
setwd("C:/Users/lab/Documents/GitHub/Energy")

# create provincial data
source("create_data.R")

# create input vectors
years = 2005:2018
areas = prov_map@data$NAME
indicators = c("input","output","price",'emission')
commodities = c("wood","light_fuel_oil","heavy_fuel_oil","diesel","total_coal",
                "natural_gas","uranium","methane","propane")

# create input lists
list_of_areas = lapply(paste0("<p style='color:black;font-family:Verdana,Geneva,sans-serif;font-weight:bold;font-size: 14px'>",
                                   areas,"</p>"),HTML)

list_of_indicators = lapply(paste0("<p style='color:black;font-family:Verdana,Geneva,sans-serif;font-weight:bold;font-size: 14px'>",
                                   indicators,"</p>"),HTML)

list_of_commodities = lapply(paste0("<p style='color:black;font-family:Verdana,Geneva,sans-serif;font-weight:bold;font-size: 14px'>",
                                    toTitleCase(gsub('_', ' ', commodities)),"</p>"),HTML)

commodity_labels = toTitleCase(gsub('_', ' ', commodities))
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
      
      box(sliderInput("year",h6(''), 2005, 2018, value=c(2005,2018),sep = ""),width = 3),
      actionButton("instructions","Display instructions"),
      textOutput("show")
      ),
    
    fluidRow(
      box(status = "primary",leafletOutput("plot", height= '600px'),width="100%")),
    fluidRow(box(column(6,selectInput("province", label = HTML('<FONT color="#55579A"><FONT size="4pt">Province'),
                             choices = areas, selected='Ontario')),
             column(3,selectInput("commodity", label = HTML('<FONT color="#55579A"><FONT size="4pt">Compare'),
                                   choices = setNames(commodities,commodity_labels), selected = 'wood')),
             column(3,selectInput("commodity2", label = HTML('<FONT color="#55579A"><FONT size="4pt">with'),
                             choices = setNames(commodities,commodity_labels), selected = 'wood')), width = 12)),
    fluidRow(
      
      box(title = "bubblebubble",status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("bubble"),width= 6),
      box(title = "bitchtits",status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("bitchtits"),width= 6)
      
      ),
    
    fluidRow(
      
      column(title = "InputOutput",status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("linegraph"),width = 6),
      column(title = "InputOutput",status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("linegraph2"),width = 6)
      
    )
      
    ))


# create server
server <- function(input, output) {
  
  observeEvent(input$instruction_yes, {
    
    output$show <- renderText({
   
      paste0("Test set of instructions")
    })
    
  })
  
   
  output$plot <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 6,
      attributionControl=FALSE)) %>%
      setView(lng = -100.4, lat = 60, zoom = 4) %>%
      addProviderTiles(providers$Hydda.Base,
                       options = providerTileOptions(opacity = 0.8))
    
  })
  
  selected_com_ind <- reactive({
    
    min_year = min(input$year)
    max_year = max(input$year)
    min_emissions = str_c(input$commodity,'_emission_',min_year)
    max_emissions = str_c(input$commodity,'_emission_',max_year)
    min_outputs = str_c(input$commodity,'_output_',min_year)
    max_outputs = str_c(input$commodity,'_output_',max_year)
    
    prov_map@data %<>% 
      mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
             outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0))
    
    prov_map
    
  })
  
  observe({
    
    prov_popup <- paste0('<strong>',selected_com_ind()$NAME,', ',
                         input$commodity,"</strong> <br>",
                         '<strong>Emissions: </strong>',selected_com_ind()$emissions, " tonnes",
                         '<br><strong>Outputs: </strong>',selected_com_ind()$outputs, " MWh")
    
    leafletProxy("plot") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = selected_com_ind(),
                  fillColor = ~colorBin("Reds", emissions, 5)(emissions),
                  color = "#BDBDC3",
                  fillOpacity = 0.2,
                  weight = 1) %>%
      addCircles(data=selected_com_ind(), lng = ~x, lat = ~y,
                 fillOpacity = 0.6,
                 color = 'blue',
                 popup = prov_popup,
                 weight = ~outputs/10000)
    
  })
  
  selected_prov_input <- reactive({

    min_year = min(input$year)
    max_year = max(input$year)
    selected_province = input$province

    subject_matter_2 %>%
      filter(province == selected_province & year %in% min_year:max_year) %>%
      mutate(year_opacity = ((year-1999)^3)/6859)
  })
  
  output$bubble <- renderPlotly({
    
    efficiency_selected() %>%
      plot_ly(type = 'scatter', mode = 'markers', x = ~input, y = ~output, size = ~price, color = ~commodity,
              colors = viridis_pal(option = "D")(10),
              marker = list(sizeref=0.1, line = list(width = 0.5, color = '#FFFFFF')), hoverinfo = 'text', 
              text=~paste('<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
                          '<b>Price: </b>$',format(price,big.mark=",",scientific=FALSE),'<br>',
                          '<b>Input: </b>',format(input,big.mark=",",scientific=FALSE),'Tonsslashkilolitres','<br>',
                          '<b>Output: </b>',format(output,big.mark=",",scientific=FALSE),'MgHours')) %>%
      layout(showlegend=FALSE)
    
  })
  
  linegraph_reactive <- reactive({
    
    selected_province = input$province
    selected_commodity = input$commodity
    min_year = min(input$year)
    max_year = max(input$year)
    
    subject_matter_2 %>% filter(province == selected_province,
                                commodity %in% selected_commodity,
                                year %in% min_year:max_year) %>%
      mutate(efficiency=output/input)
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
  
  output$linegraph2 <- renderPlotly({
    
    plot_ly(linegraph_reactive()) %>%
      add_trace(x= ~year, y= ~price,type = 'scatter', mode = 'lines', name= 'Price',
                marker = list(color = '#55579A'),
                hoverinfo = "text",
                text = ~paste(price,' t')) %>%
      add_trace(x= ~year, y = ~efficiency, type = 'scatter', mode = 'lines', name = 'Efficiency', yaxis = 'y2',
                line = list(color = '#4db8ff'),
                hoverinfo = "text",
                text = ~paste(efficiency,'GWh')) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(range=c(0,~max(price)),side = 'left', title = 'Price (metric tonnes)', showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(range=c(0,~max(efficiency)),side = 'right', overlaying = "y", title = 'Efficiency in MWh', showgrid = FALSE, zeroline = FALSE))
    
  })
  
  
  efficiency_selected <- reactive({
    
    min_year = min(input$year)
    max_year = max(input$year)
    selected_province = input$province
    
    subject_matter_2 %>%
      filter(province == selected_province & year %in% min_year:max_year) %>%
      mutate(year_opacity = ((year-1999)^3)/6859,
             efficiency=output/input)
    
  })
  
  output$bitchtits <- renderPlotly({

    efficiency_selected() %>%
      plot_ly(type = 'scatter', mode = 'markers', x = ~efficiency, y = ~price, size = ~emission, color = ~commodity,
              colors = viridis_pal(option = "D")(10),
              marker = list(sizeref=0.1, line = list(width = 0.5, color = '#FFFFFF')), hoverinfo = 'text',
              text=~paste('<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
                          '<b>Price: </b>$',format(price,big.mark=",",scientific=FALSE),'<br>')) %>%
      layout(showlegend=FALSE)

  })
  
  
}

# run app
shinyApp(ui, server)

# https://stackoverflow.com/questions/37472915/r-shiny-how-to-generate-this-layout-with-nested-rows-in-column-2

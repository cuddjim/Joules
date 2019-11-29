

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
setwd("~/Documents/Projects/KnownSideEffects/")
# setwd("C:/Users/jimmy/OneDrive/Documents/GitHub/KnownSideEffects")
# setwd("C:/Users/lab/Documents/GitHub/KnownSideEffects")

# create provincial data
source("create_data.R")

# create input vectors
years = 2005:2018
areas = prov_map@data$NAME
indicators = c("input","output","price",'emission')
commodities = c("wood","light_fuel_oil","heavy_fuel_oil","diesel","total_coal",
                "natural_gas","uranium","methane","propane")
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
      actionButton("instructions","Display user guide"),
      textOutput("show")
      
      ),
    
    fluidRow(
      
      box(status = "info",leafletOutput("plot", height= '600px'),width="100%")
      
      ),
    
    fluidRow(
      
      box(
        
        column(6,selectInput("province", label = HTML('<FONT color="#55579A"><FONT size="4pt">Province'),
                               choices = areas, selected='Ontario')),
        column(3,selectInput("commodity1", label = HTML('<FONT color="#55579A"><FONT size="4pt">Compare'),
                               choices = setNames(commodities,commodity_labels), selected = 'wood')),
        column(3,selectInput("commodity2", label = HTML('<FONT color="#55579A"><FONT size="4pt">with'),
                               choices = setNames(commodities,commodity_labels), selected = 'wood')),
        column(status = "primary",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("bubble"),width= 6),
        column(status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("linegraph"),width = 6),
        column(status = "primary",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("bubble_2"),width= 6),
        column(status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("linegraph2"),width = 6),width=12)
      
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
    min_emissions = str_c(input$commodity1,'_emission_',min_year)
    max_emissions = str_c(input$commodity1,'_emission_',max_year)
    min_outputs = str_c(input$commodity1,'_output_',min_year)
    max_outputs = str_c(input$commodity1,'_output_',max_year)
    
    prov_map@data %<>% 
      mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
             outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0))
    
    prov_map
    
  })
  
  observe({
    
    prov_popup <- paste0('<strong>',selected_com_ind()$NAME,', ',
                         input$commodity1,"</strong> <br>",
                         '<strong>Emissions: </strong>',selected_com_ind()$emissions, " tonnes",
                         '<br><strong>Outputs: </strong>',selected_com_ind()$outputs, " MWh")
    
    color_pal <- colorNumeric(palette = "RdYlBu", 
                              domain = selected_com_ind()$emissions, reverse = TRUE)
    
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
                 weight = ~outputs/10000) %>% 
      addLegend("bottomright", pal = color_pal, values = subject_matter_2$emission)
    
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
      layout(showlegend=TRUE)
    
  })
  
  linegraph_reactive <- reactive({
    
    selected_province = input$province
    selected_commodity = input$commodity1
    min_year = min(input$year)
    max_year = max(input$year)
    
    subject_matter_2 %>% filter(province == selected_province,
                                commodity %in% selected_commodity,
                                year %in% min_year:max_year) %>%
      mutate(efficiency=output/input)
  })
  
  linegraph_reactive2 <- reactive({
    
    selected_province = input$province
    selected_commodity = input$commodity2
    min_year = min(input$year)
    max_year = max(input$year)
    
    subject_matter_2 %>% filter(province == selected_province,
                                commodity %in% selected_commodity,
                                year %in% min_year:max_year) %>%
      mutate(efficiency=output/input)
  })
  
  output$linegraph <- renderPlotly({
    
    plot_ly() %>%
      add_trace(data=linegraph_reactive(),x= ~year, y= ~input,type = 'scatter', mode = 'lines', name= 'Input',
                line = list(color = '#55579A'),
                hoverinfo = "text",
                text = ~paste(input,' t')) %>%
      add_trace(data=linegraph_reactive(),x= ~year, y = ~output, type = 'scatter', mode = 'lines', name = 'Output', yaxis = 'y2',
                line = list(color = '#4db8ff'),
                hoverinfo = "text",
                text = ~paste(output,'GWh')) %>%
      add_trace(data=linegraph_reactive2(),x= ~year, y= ~input,type = 'scatter', mode = 'lines', name= 'Input',
                line = list(color = '#FF0000'),
                hoverinfo = "text",
                text = ~paste(input,' t')) %>%
      add_trace(data=linegraph_reactive2(),x= ~year, y = ~output, type = 'scatter', mode = 'lines', name = 'Output', yaxis = 'y2',
                line = list(color = '#9E1A1A'),
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
  
  output$bubble_2 <- renderPlotly({

    efficiency_selected() %>%
      plot_ly(type = 'scatter', mode = 'markers', x = ~efficiency, y = ~price, size = ~emission, color = ~commodity,
              colors = viridis_pal(option = "D")(10),
              marker = list(sizeref=0.1, line = list(width = 0.5, color = '#FFFFFF')), hoverinfo = 'text',
              text=~paste('<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
                          '<b>Price: </b>$',format(price,big.mark=",",scientific=FALSE),'<br>')) %>%
      layout(showlegend=TRUE)

  })
  
  
}

# run app
shinyApp(ui, server)

# https://stackoverflow.com/questions/37472915/r-shiny-how-to-generate-this-layout-with-nested-rows-in-column-2

# change prov color scale
#indicate circle meaning map - legend

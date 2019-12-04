

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
require(formattable)
require(DT)
# set working directory
# setwd("~/Documents/Projects/KnownSideEffects/")
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
  
  dashboardHeader(title = "Thermal Emissions Visualizations", titleWidth = '100%'),
  
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

        /* body */
        .content-wrapper, .right-side {
                              background-color: #FFFFFF;

        }

                              '))),
    fluidRow(
     
     
      helpText("These instructions are about the map, and I guess the whole thing in general")),
    
    
    fluidRow(
    
      box(title='Thematic Map of Emissions and Output by Fuel Type', status = "primary", solidHeader = TRUE, 
          collapsible = TRUE, column(6,selectInput("map_commodity", label = HTML('<FONT color="#55579A"><FONT size="4pt">Select Map Fuel Type and Years'),
                                                   choices = setNames(commodities,commodity_labels), selected = 'diesel')),
          column(6,sliderInput("year", h6(''), 2005, 2018, value=c(2005,2018),sep = "")),leafletOutput("plot", height= '600px'),width = '100%'
          )
      
      ),
    
    fluidRow(helpText("The graphs below compare fuel types per province selected. 
                      The bubble charts show all fuel types per province, 
                      while the line graphs show the fuels selected in the Compare and To drop down menus:")),
    
    fluidRow(
      
      box(title='Input(TJ), Output(MWh), emissions(metric tonnes), price($ x 1,000), efficiency ratio', status = "primary", solidHeader = TRUE, 
          collapsible = TRUE,
        
        column(6,selectInput("province", label = HTML('<FONT color="#55579A"><FONT size="4pt">Province'),
                               choices = areas, selected='Prince Edward Island')),
        column(3,selectInput("commodity1", label = HTML('<FONT color="#55579A"><FONT size="4pt">Compare'),
                               choices = setNames(commodities,commodity_labels), selected = 'heavy_fuel_oil')),
        column(3,selectInput("commodity2", label = HTML('<FONT color="#55579A"><FONT size="4pt">To'),
                               choices = setNames(commodities,commodity_labels), selected = 'light_fuel_oil')),
        column(plotlyOutput("bubble"),width= 6),
        column(plotlyOutput("linegraph_input"),width = 6),
        column(plotlyOutput("bubble_2"),width= 6),
        column(plotlyOutput("linegraph2"),width = 6),width='100%')
      
    ),
    fluidRow(helpText("The graphs below compare fuel types per province selected. 
                      The bubble charts show all fuel types per province, 
                      while the line graphs show the fuels selected in the Compare and To drop down menus:")),
    fluidRow(box(title = "Data table for now", status = "primary", solidHeader = TRUE, 
                 collapsible = TRUE,column(12,selectInput("variable", 
                                                         label = HTML('<FONT color="#55579A"><FONT size="4pt">Select variable to display:'),
                                                         choices = indicators, selected='input')), 
                 dataTableOutput('prov_comp'),width = '100%')
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
      setView(lng = -105.4, lat = 58.7, zoom = 4) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(opacity = 0.8))
    
  })
  
  selected_com_ind <- reactive({
    
    min_year = min(input$year)
    max_year = max(input$year)
    min_emissions = str_c(input$map_commodity,'_emission_',min_year)
    max_emissions = str_c(input$map_commodity,'_emission_',max_year)
    min_outputs = str_c(input$map_commodity,'_output_',min_year)
    max_outputs = str_c(input$map_commodity,'_output_',max_year)
    
    prov_map@data %<>% 
      mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
             outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0)) %>% 
      mutate(scaled_outputs = scale(outputs))
    
    prov_map
    
  })
  
  observe({
    
    prov_popup <- paste0('<strong>',selected_com_ind()$NAME,', ',
                         input$commodity1,"</strong> <br>",
                         '<strong>Emissions: </strong>',selected_com_ind()$emissions, " tonnes",
                         '<br><strong>Outputs: </strong>',selected_com_ind()$outputs, " MWh")
    
    color_pal <- colorNumeric(palette = "RdYlBu", 
                              domain = selected_com_ind()$emissions, reverse = TRUE)
    huey = selected_com_ind()$emissions
    
    leafletProxy("plot") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = selected_com_ind(),
                  fillColor = ~colorBin("RdYlBu", emissions, 5, reverse = TRUE)(emissions),
                  color = "#BDBDC3",
                  fillOpacity = 0.7,
                  weight = 1) %>%
      addCircles(data=selected_com_ind(), lng = ~x, lat = ~y,
                 fillOpacity = 1,
                 color = 'black',
                 popup = prov_popup,
                 weight = ~scaled_outputs*23) %>% 
      addLegend(opacity = 0.7, title = "CO2e Emissions (tonnes)","bottomleft", pal = colorBin(palette = "RdYlBu", 
                                                  domain = selected_com_ind()$emissions, reverse = FALSE), values = huey, 
                labFormat = labelFormat(transform = function(huey) sort(huey, decreasing = TRUE)))
    
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
              marker = list(sizeref=0.1, line = list(width = 1, color = '#FFFFFF')), hoverinfo = 'text', 
              text=~paste('<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
                          '<b>Price: </b>$',format(price,big.mark=",",scientific=FALSE),'<br>',
                          '<b>Input: </b>',format(input,big.mark=",",scientific=FALSE),'TJ','<br>',
                          '<b>Output: </b>',format(output,big.mark=",",scientific=FALSE),'MWh')) %>%
      layout(title = paste0('Comparing ',input$province,' Energy Types'),
             showlegend=TRUE)
    
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
  
  output$linegraph_input <- renderPlotly({
    
    a = plot_ly() %>%
      add_trace(data=linegraph_reactive(),x= ~year, y= ~input,type = 'scatter', mode = 'none', name= input$commodity1, fill = 'tozeroy',
                line = list(color = '#55579A'), opacity = 0.6,
                hoverinfo = "text",
                text = ~paste(format(round(input,1),big.mark = ",",scientific = FALSE),' TJ')) %>%
      add_trace(data=linegraph_reactive2(),x= ~year, y= ~input,type = 'scatter', mode = 'none', name= input$commodity2, fill = 'tozeroy',
                line = list(color = '#FF0000'), opacity = 0.6,
                hoverinfo = "text",
                text = ~paste(format(round(input,1),big.mark = ",",scientific = FALSE),' TJ')) %>%
      layout(title = paste0('Comparing Inputs of ',input$commodity1,' to ',input$commodity2),
             xaxis = list(title = ""),
             yaxis = list(range=c(~min(c(linegraph_reactive()$input,linegraph_reactive2()$input)),~max(c(linegraph_reactive()$input,linegraph_reactive2()$input))),
                          side = 'left', title = 'Input (TJ)', showgrid = FALSE, zeroline = FALSE))

    b = plot_ly() %>%
      add_trace(data=linegraph_reactive(),x= ~year, y = ~output, type = 'scatter', mode = 'none', name = input$commodity1, fill = 'tozeroy',
                line = list(color = '#55579A'), opacity = 0.6,
                hoverinfo = "text",
                text = ~paste(format(round(output,0),big.mark = ",",scientific = FALSE),'MWh')) %>%
      add_trace(data=linegraph_reactive2(),x= ~year, y = ~output, type = 'scatter', mode = 'none', name = input$commodity2, fill = 'tozeroy',
                line = list(color = '#FF0000'), opacity = 0.6,
                hoverinfo = "text",
                text = ~paste(format(round(output,0),big.mark = ",",scientific = FALSE),'MWh')) %>%
      layout(title = paste0('Comparing ',input$commodity1,' to ',input$commodity2,' in ',input$province),
             xaxis = list(title = ""),
             yaxis = list(range=c(~min(c(linegraph_reactive()$output,linegraph_reactive2()$output)),~max(c(linegraph_reactive()$output,linegraph_reactive2()$output))),
                          side = 'left', title = 'Output (MWh)', showgrid = FALSE, zeroline = FALSE))
    
    subplot(a,b,nrows=2,titleY=TRUE,margin=0.075)
    
  })
  
  output$linegraph2 <- renderPlotly({
    
    a = plot_ly() %>%
      add_trace(data = linegraph_reactive(), x= ~year, y= ~price,type = 'scatter', mode = 'none', 
                name= input$commodity1, fill = 'tozeroy',
                line = list(color = '#55579A', opacity = 0.5),
                opacity = 0.6,
                hoverinfo = "text",
                text = ~paste(format(round(price,0),big.mark = ",",scientific = FALSE),' $')) %>%
      add_trace(data=linegraph_reactive2(),x= ~year, y= ~price,type = 'scatter', mode = 'none', 
                name= input$commodity2, fill = 'tozeroy',
                line = list(color = '#FF0000', opacity = 0.5),
                opacity = 0.6,
                hoverinfo = "text",
                text = ~paste(format(round(price,0),big.mark = ",",scientific = FALSE),' $')) %>%
      layout(showLegend=FALSE,
             title = paste0('Comparing ',input$commodity1,' to ',input$commodity2,' in ',input$province),
             xaxis = list(title = ""),
             yaxis = list(range=c(~min(c(linegraph_reactive()$price,linegraph_reactive2()$price)),~max(c(linegraph_reactive()$price,linegraph_reactive2()$price))),
                          side = 'left', title = 'Price (thousands $)', showgrid = FALSE, zeroline = FALSE))
    
    b = plot_ly() %>%
      add_trace(data=linegraph_reactive(), x= ~year, y = ~efficiency, type = 'scatter', mode = 'none', name = input$commodity1,fill = 'tozeroy',
                line = list(color = '#55579A', opacity = 0.5),
                opacity = 0.6,
                hoverinfo = "text",
                text = ~paste(format(round(efficiency,1),big.mark = ",",scientific = FALSE),'MWh/TJ')) %>%
      add_trace(data=linegraph_reactive2(),x= ~year, y = ~efficiency, type = 'scatter', mode = 'none', name = input$commodity2,fill = 'tozeroy',
                line = list(color = '#FF0000', opacity = 0.5),
                opacity = 0.6,
                hoverinfo = "text",
                text = ~paste(format(round(efficiency,1),big.mark = ",",scientific = FALSE),'MWh/TJ')) %>%
      layout(title = paste0('Comparing ',input$commodity1,' to ',input$commodity2,' in ',input$province),
             xaxis = list(title = ""),
             yaxis = list(range=c(~min(c(linegraph_reactive()$efficiency,linegraph_reactive2()$efficiency)),~max(c(linegraph_reactive()$efficiency,linegraph_reactive2()$efficiency))),
                          side = 'left', title = 'Efficiency (MWh/TJ)', showgrid = FALSE, zeroline = FALSE))
    
    subplot(a,b,nrows=2,titleY=TRUE,margin=0.075)
    
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
      plot_ly(type = 'scatter', mode = 'markers', x = ~efficiency, y = ~emission, size = ~price, color = ~commodity,
              colors = viridis_pal(option = "D")(10),
              marker = list(sizeref=0.1, line = list(width = 1, color = '#FFFFFF')), hoverinfo = 'text',
              text=~paste('<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
                          '<b>Price: </b>$',format(price,big.mark=",",scientific=FALSE),'<br>')) %>%
      layout(title = paste0('Comparing ',input$province,' Energy Types'),
             showlegend=TRUE)

  })
  
  table_reactive <- reactive({
    
    min_year = min(input$year)
    max_year = max(input$year)
    selected_indicator = input$variable
    
    subject_matter_2 %>%
      filter(year %in% min_year:max_year) %>%
      select(province,commodity,noquote(paste0(selected_indicator))) %>%
      rename(indicator=noquote(paste0(selected_indicator))) %>%
      group_by(province,commodity) %>%
      summarize(mean_value=comma(round(mean(indicator,na.rm=TRUE),0),format='d')) %>%
      spread(commodity,mean_value) %>%
      replace(is.na(.), 0) %>%
      arrange(-diesel)
    
  })
  
  output$prov_comp <- renderDataTable({
    
    custom_color_tile <- function (...) 
      
    {
      
      formatter("span",
                style = function(x) style(display = "block", 
                                          padding = "0 4px", 
                                          `color` = "black", 
                                          `border-radius` = "4px", 
                                          width = "80px",
                                          `background-color` = csscolor(gradient(as.numeric(x), 
                                                                                 ...))))
    }
    
    custom_color_tile_2 <- function (...)
      
    {
      
      formatter("span",
                style = function(x) style(display = "block", 
                                          padding = "0 2px", 
                                          `color` = "black", 
                                          `border-radius` = "2px", 
                                          width = "250px"))
    }
    
    as.datatable(formattable(table_reactive(), align = c("l", rep("r", ncol(table_reactive())-1)),
                list('province' = custom_color_tile_2(),
                     'diesel' = custom_color_tile('white','#00ff00'),
                     'heavy_fuel_oil' = custom_color_tile('white','#00ff00'),
                     'light_fuel_oil' = custom_color_tile('white','#00ff00'),
                     'wood' = custom_color_tile('white','#00ff00'),
                     'propane' = custom_color_tile('white','#00ff00'),
                     'uranium' = custom_color_tile('white','#00ff00'),
                     'total_coal' = custom_color_tile('white','#00ff00'),
                     'natural_gas' = custom_color_tile('white','#00ff00'),
                     'methane' = custom_color_tile('white','#00ff00')
                     )
                ), options=list(pageLength=15,dom='t'))
                     
                     })
  
  
}

# run app
shinyApp(ui, server)





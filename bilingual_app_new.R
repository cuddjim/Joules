

# set working directory
setwd("~/Documents/Projects/KnownSideEffects/")
# setwd("C:/Users/jimmy/OneDrive/Documents/GitHub/KnownSideEffects")
# setwd("C:/Users/lab/Documents/GitHub/KnownSideEffects")
# setwd("C:/Users/cuddjim/Documents/KnownSideEffects")

# create provincial data
source("create_data.R")

ui <- shinyUI(
  
  fluidPage(
    
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                   padding-top:14px !important;
                   padding-bottom:4px !important;
                   height: 50px;
                 }
                 .navbar {min-height:50px !important;}')),
    tags$head(tags$style(HTML('.navbar .navbar-menu{ background-color: #00b8bd; color: #00b8bd}'))),
    tags$script("$(\"input:radio[name='selected_language'][value='fr']\").parent().css('background-color', '#FFFFFF');"),
    theme = shinytheme("flatly"),
    chooseSliderSkin("HTML5",color='#000039'),
    setBackgroundColor('white'),
    
    titlePanel(h1('A Story of Thermal Emissions in Canada', 
                  style = "font-family: 'Palatino', bold; font-weight: bold; line-height: 1.1; color: #000000;")),
    uiOutput('page_content')
    
  )
)

translator <- Translator$new(translation_json_path = "translation.json")

server <- shinyServer(function(input, output) {
  
  tr <- reactive({
    
    selected <- input$selected_language
    
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    
    translator
    
  })
  
  bubble_reactive <- reactive({
    
    selected_province = input$province
    min_year = min(input$year); max_year = max(input$year)
    x_indicator = input$x_indicator;  y_indicator = input$y_indicator; b_indicator = input$b_indicator
    
    subject_matter_2 %>%
      filter(province == selected_province & year %in% min_year:max_year) %>%
      mutate(year_opacity = ((year-1999)^3)/6859,
             efficiency=output/input) %>%
      mutate(selected_x_axis = as.numeric(paste0(!!rlang::sym(x_indicator))),
             selected_y_axis = as.numeric(paste0(!!rlang::sym(y_indicator))),
             selected_b_axis = as.numeric(paste0(!!rlang::sym(b_indicator))))
    
  })
  
  output$bubble <- renderPlotly({
    
    bubble_reactive() %>%
      plot_ly(
        type = 'scatter', mode = 'markers', 
        marker = list(size = ~selected_b_axis*3, opacity = 0.5, line = list(width = 1, color = '#FFFFFF')), 
        x = ~selected_x_axis, y = ~selected_y_axis, 
        color = ~commodity, colors = c("#FF3200","#E9A17C","#E9E4A6","#1BB6AF","#0076BB","#172869"),
        hoverinfo = 'text',
        text=~paste(
          '<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
          '<b>Cost: </b>$',format(round(price,2),big.mark=",",scientific=FALSE),'<br>',
          '<b>',tr()$t('Efficiency'),': </b>', format(round(input,1), big.mark=",", scientific=FALSE), 'TJ', '<br>',
          '<b>Emissions: </b>',format(round(emission,1),big.mark=",",scientific=FALSE),'tonnes'
          )) %>%
      layout(
        title = paste0('<b>Comparing ',input$province,' Energy Types</b>'),
        xaxis = list(
          font = list(family='Helvetica Neue', weight='bold',size = 20),
          title = paste0('<b>',tr()$t(paste0(indicator_labels[which(indicators==input$x_indicator)])),'</b>'), 
          zeroline=FALSE),
        yaxis = list(
          title = paste0('<b>',tr()$t(paste0(indicator_labels[which(indicators==input$y_indicator)])),'</b>'), 
          zeroline=FALSE),
        legend = list(font = list(family='Helvetica Neue', weight='bold',size = 10))
        ) %>% 
      config(displayModeBar = F)
    
  })
  
  map_reactive <- reactive({
    
    min_year = min(input$year)
    max_year = max(input$year)
    map_commodity = input$map_commodity
    min_emissions = str_c(map_commodity,'_emission_',min_year)
    max_emissions = str_c(map_commodity,'_emission_',max_year)
    min_outputs = str_c(map_commodity,'_output_',min_year)
    max_outputs = str_c(map_commodity,'_output_',max_year)
    
    prov_map@data %<>% 
      mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
             outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0)) %>% 
      mutate(scaled_outputs = log(1+outputs)^2.5)
    
    prov_map
    
  })
  
  output$plot <- renderLeaflet({
    
    prov_popup <- paste0('<strong>',map_reactive()$NAME,', ',
                         tr()$t(toTitleCase(gsub('_', ' ', input$map_commodity))),"</strong> <br>",
                         '<strong>',tr()$t("Emissions"),': </strong>',formatC(round(map_reactive()$emissions,0), format = 'd', big.mark = ","), " tonnes",
                         '<br><strong>',tr()$t("Outputs"),': </strong>',formatC(round(map_reactive()$outputs,0), format = 'd', big.mark = ","), " MWh")
    
    huey = map_reactive()$emissions
    
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 4,
                                     attributionControl=FALSE)) %>%
      setView(lng = -98.4, lat = 58.2, zoom = 4) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(opacity = 0.8)) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = map_reactive(),
                  fillColor = ~colorBin(c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), emissions, 5)(emissions),
                  color = "#BDBDC3",
                  fillOpacity = 0.7,
                  weight = 4) %>%
      addCircles(data=map_reactive(), lng = ~x, lat = ~y,
                 fillOpacity = 1,
                 color = 'black',
                 popup = prov_popup,
                 weight = ~scaled_outputs) %>% 
      addLegend(opacity = 0.7, title = tr()$t("CO2e Emissions (tonnes)"),"bottomleft", 
                pal = colorBin(palette = c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), domain = map_reactive()$emissions, 5), values = huey, 
                labFormat = labelFormat(transform = function(huey) sort(huey, decreasing = FALSE)))
    
  })
  
  output$page_content <- renderUI({
    
    navbarPage('Statistics Canada',
               
               tabPanel(
                 
                 'Language',
                 
                 radioGroupButtons(
                   inputId = "selected_language",
                   choiceValues = translator$languages,
                   choiceNames = c('English','Francais'),
                   selected = input$selected_language,
                   justified = TRUE,width="200px"
                 ),
                 icon = NULL
                 
               ),
               
               tabPanel(
                 
                 tr()$t('Comparing Provincial Thermal Emissions'),
                 
                 sidebarPanel(
                   selectizeInput("map_commodity", label = tr()$t('Select Fuel Type'),
                                  choices = setNames(commodities,tr()$t(commodity_labels)),
                                  selected = 'diesel'), 
                   width=2
                 ),
                 
                 mainPanel(
                   leafletOutput("plot",height=600), 
                   width=10
                 )
                 
               ),
               
               tabPanel(
                 
                 tr()$t('Comparing Thermal Energy Types'),
                 
                 sidebarPanel(
                   selectizeInput("province", label = tr()$t('Select Province'),
                                  choices = tr()$t(areas), selected='Ontario'),
                   selectizeInput("x_indicator", label = tr()$t('Select X Indicator'),
                                  choices = setNames(indicators,tr()$t(indicator_labels)), selected='input'),
                   selectizeInput("y_indicator", label = tr()$t('Select Y Indicator'),
                                  choices = setNames(indicators,tr()$t(indicator_labels)), selected='output'),
                   selectizeInput("b_indicator", label = tr()$t('Select B Indicator'),
                                  choices = setNames(indicators,tr()$t(indicator_labels)), selected='price'),
                   sliderInput("year", label = tr()$t('Select Year'), 
                               2005, 2018, value=c(2005,2018),
                               sep = ""),
                   width=3
                 ),
                 
                 mainPanel(
                   fluidRow(
                     plotlyOutput("bubble")
                   ),
                   width=8
                 )
                 
               )
               
    )
    
  })
  
})

shinyApp(ui = ui, server = server)


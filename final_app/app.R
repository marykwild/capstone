library(shiny)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(htmltools))

suppressPackageStartupMessages(library(tigris))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(maps))

suppressPackageStartupMessages(library(classInt))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(ggplot2))
library(leaflet)
library(plotly)

states <- states(cb=TRUE)
states <- as_Spatial(states, cast = TRUE)
states <- states[!states@data$STUSPS %in% c("VI","GU","MP","PR"), ]
states@data$NAME <- tolower(states@data$NAME)

df <- read_csv("county_level_denialrate.csv") %>% subset(select = -X1)
lei <- read_csv('lei.csv') %>% filter(debt_to_income_ratio_bracket != ">60%")
lei$denial_rate_adj <- ifelse(lei$total_loans < 100, NA, lei$denial_rate)

left <- read_csv('df.csv')

sidebar_yaxis <- list(
  tickvals=list(0.5),
  ticktext=list(""),
  zerolinecolor = '#ffff',
  zerolinewidth = 2,
  showgrid = FALSE)

#Join up a statewide denial rate.
df_state <- df %>% 
  #filter(!state_code%in%c("AK","HI")) %>% 
  group_by(state_code) %>% 
  summarize(white_apps = sum(white_apps),
            black_apps = sum(black_apps),
            asian_apps = sum(asian_apps),
            hisp_apps = sum(hisp_apps),
            white_fails = sum(white_fails),
            hisp_fails = sum(hisp_fails),
            black_fails = sum(black_fails),
            asian_fails = sum(asian_fails),
            denial_rate = round(sum(loan_failed) / sum(total_loans),3)*100,
            white_app_rate = round(white_apps / sum(total_loans),3)*100,
            hisp_app_rate = round(hisp_apps / sum(total_loans),3)*100,
            black_app_rate = round(black_apps / sum(total_loans),3)*100,
            asian_app_rate = round(asian_apps / sum(total_loans),3)*100) %>% 
  mutate(white_denial_rate=round(white_fails / white_apps,3)*100,
         hisp_denial_rate=round(hisp_fails / hisp_apps,3)*100,
         black_denial_rate=round(black_fails / black_apps,3)*100,
         asian_denial_rate= round(asian_fails / asian_apps,3)*100) %>% 
  subset(select = -c(white_fails,hisp_fails,black_fails,asian_fails,white_apps,hisp_apps,black_apps,asian_apps))


states <- sp::merge(states, df_state, by.x = "STUSPS", by.y = "state_code", all=TRUE)
pal <- colorNumeric(
  palette = "Reds", domain = states$denial_rate, na.color = NA
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      
      #sidebar-top {
        position:fixed;
        top:0;
        left:0;
        height:25vh;
        width:40vw;
        background-color:#EEEDE7;
        z-index:99;
        padding:2%;
      }
    
      #sidebar {
        position: fixed;
        top: 25vh;
        left: 0;
        height:75vh;
        width:40vw;
        background-color:#EEEDE7;
        z-index:98;
        padding:2%;
      }
      
      .plotpad {
        margin-top:2%;
        padding-top:1%;
        padding-bottom:1%;
        background-color:white;
      }
      
      #leiPlot {
        height:300px;
      }
      
      .chartTitle {
        font-size:12px;
      }
      
      #source {
        position:fixed;
        right:0;
        bottom:0;
        background-color:#EEEDE7;
        height:5vh;
        z-index:98;
        font-size:9px;
        padding:1%;        
      }
      
      #notes {
        font-size:10px;
        padding-top:1vh;
      }
      
      #introtext {
        font-size:12px;
        text-align:center;
        padding-left:5%;
        padding-right:5%;
      }
      
      #map {
        position:fixed;
        height: 100vh !important;
        background:white;
      }
      
      #forcepad {
        height: 15px;
      }
      
    "))
  ), #End of CSS
  
  div(id="sidebar-top",
      fluidRow(
        p(id="introtext", "New data shows how even when controlling for income and debt levels, the lending market is tougher for non-white borrowers."),
        uiOutput("loc"),
        column(6,selectInput("incomeSelect", "Choose an income bracket:", c("All","$50k to $100k","$100k to $150k", "$150k to $200k", "$200k to $250k"), selected = "All")),
        column(6,selectInput("debtSelect", "And a debt-to-income level:", c("All","<20%","20%-29%","30%-39%","40%-49%","50%-60%"), selected = "All")
        ))
      
      
  ),#Sidebar-top
  
  div(id="sidebar",
      tabsetPanel(
        tabPanel("Race and ethnicity",
                 div(id="forcepad"),
                 strong(class="chartTitle", "% OF MORTGAGE APPLICANTS WHO FAILED TO GET A HOME LOAN"),
                 div(class="plotpad",
                     div(class="plotborder",plotlyOutput("whitePlot", height = "80px")),
                     div(class="plotborder",plotlyOutput("hispPlot", height = "80px")),
                     div(class="plotborder",plotlyOutput("blackPlot", height = "80px")),
                     div(class="plotborder",plotlyOutput("asianPlot", height = "80px"))
                 ),#plotpad
                 div(id="notes",strong("RATE ONLY SHOWN WHEN 100 OR MORE SAMPLES PRESENT"))
        ),#tabPanel1
        tabPanel(
          "Top 5 Lenders",
          div(id="forcepad"),
          strong(class="chartTitle", "% OF MORTGAGE APPLICANTS WHO FAILED TO GET A HOME LOAN"),
          div(class="plotpad", id="leiPlot", plotlyOutput("leiPlot")),
          div(id="notes",strong("RATE ONLY SHOWN WHEN 100 OR MORE SAMPLES PRESENT"))
        )#tabPanel2
      )#tabsetPanel
  ), ##sidebar
  div(id="source",strong("YEARS: 2018-2020 | SOURCE: CONSUMER FINANCIAL PROTECTION BUREAU")),
  leafletOutput('map')
  
  #  )
)

server <- function(input, output){
  #REACTIVE VARIABLES
  reac_state <- reactiveVal("National")
  reac_state_code <- reactiveVal("")
  
  hl <- reactive({ as.numeric(left[left$applicant_race_ethnicity == "Hispanic or Latino" & left$debt_to_income_ratio_bracket == input$debtSelect & left$income_bracket == input$incomeSelect & left$Location == reac_state(),7])*100 })
  
  nhw <- reactive({ as.numeric(left[left$applicant_race_ethnicity == "Non-Hispanic White" & left$debt_to_income_ratio_bracket == input$debtSelect & left$income_bracket == input$incomeSelect & left$Location ==  reac_state(),7])*100 })
  
  nhb <- reactive({ as.numeric(left[left$applicant_race_ethnicity == "Non-Hispanic Black" & left$debt_to_income_ratio_bracket == input$debtSelect & left$income_bracket == input$incomeSelect & left$Location ==  reac_state(),7])*100 })
  
  nha <- reactive({ as.numeric(left[left$applicant_race_ethnicity == "Non-Hispanic Asian" & left$debt_to_income_ratio_bracket == input$debtSelect & left$income_bracket == input$incomeSelect & left$Location ==  reac_state(),7])*100 })
  
  lei_df <- reactive({ lei %>% filter(debt_to_income_ratio_bracket == input$debtSelect & income_bracket == input$incomeSelect & Location ==  reac_state()) })
  
  #We need to make the sidebar y-axis scale depending on the limits. 
  
  output$loc <- renderUI({
    HTML(paste("<p style='color: black; font-size: 20px; text-align: center;border-bottom: 2px dotted black;'> Scope: ",reac_state(),"</p>"))
  })
  
  observe({ 
    
    if ((hl()<50 & hl()!=999 & nhw()<50 & nhw()!=999 & nhb()<50 & nhb()!=999 & nha()<50 & nha()!=999) == TRUE) {
      sidebar_xaxis <<- list(
        range = c(0,50),
        tickvals=list(0,25,50),
        ticktext = list("0%","25%","50%"),
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        showgrid = FALSE) 
      
      heatbar <<- list(
        list(
          # Add image
          source = "https://raw.githubusercontent.com/marykwild/capstone/main/one_heatbar.png",
          xref = "x",
          yref = "y",
          #position of the anchor. 
          x = 0,
          y = 0,
          #Units of size
          sizex = 50,
          sizey = 1,
          xanchor = "left",
          yanchor = "bottom",
          opacity = 1,
          layer = "below",
          sizing = "stretch"
        )
      )
    }
    
    else {
      sidebar_xaxis <<- list(
        range = c(0,100),
        tickvals=list(0,50,100),
        ticktext = list("0%","50%","100%"),
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        showgrid = FALSE) 
      
      heatbar <<- list(
        list(
          # Add image
          source = "https://raw.githubusercontent.com/marykwild/capstone/main/one_heatbar.png",
          xref = "x",
          yref = "y",
          #position of the anchor. 
          x = 0,
          y = 0,
          #Units of size
          sizex = 100,
          sizey = 1,
          xanchor = "left",
          yanchor = "bottom",
          opacity = 1,
          layer = "below",
          sizing = "stretch"
        )
      )
    } # End of else
  })
  
  #BEGINNING OF MAP
  output$map = renderLeaflet({
    
    labs <- lapply(seq(nrow(states)), function(i) {
      paste0("<h4>",states@data[i,"STUSPS"],"</h4>",
             '<p><strong>Overall denial rate:</strong> ',states@data[i,"denial_rate"],'%<p>', 
             '<p><strong>White |</strong> Denial rate: ',states@data[i,"white_denial_rate"],'%, Portion of applicants: ', states@data[i,"white_app_rate"],'%</p>',
             '<p><strong>Black |</strong> Denial rate: ',states@data[i,"black_denial_rate"],'%, Portion of applicants: ', states@data[i,"black_app_rate"],'%</p>',
             '<p><strong>Hispanic or Latino |</strong> Denial rate: ',states@data[i,"hisp_denial_rate"],'%, Portion of applicants: ', states@data[i,"hisp_app_rate"],'%</p>',
             '<p><strong>Asian |</strong> Denial rate: ',states@data[i,"asian_denial_rate"],'%, Portion of applicants: ', states@data[i,"asian_app_rate"],'%</p>')
    })
    
    m <<- leaflet(states, options = leafletOptions(zoomControl = FALSE)) %>%
      setView(-115, 38.5, 4) %>% 
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('pk.eyJ1IjoibWstd2lsZGVtYW4iLCJhIjoiY2t4aHRldzJuMDNzejJucG45MjJjYWpucyJ9.9w4xxT5P8uyWW616aKDpag'))) %>% 
      addPolygons(
        fillColor = ~pal(denial_rate),
        weight = 2.5,
        opacity = 1,
        color = "white",
        fillOpacity = .95,
        label= ~lapply(labs, htmltools::HTML)
      ) %>% 
      addLegend(
        pal = pal, 
        values = ~denial_rate,
        title = "Overall denial rate",
        labFormat = labelFormat(suffix = "%"),
        opacity = 1,
        na.label = NULL
      )
      
  }) #Leaflet output
  
  observeEvent(input$map_shape_click, {
    
    # get the geo coordinates of the shape click event 
    lon <- input$map_shape_click$lng
    lat <- input$map_shape_click$lat
    
    # find the state name using the maps package
    st <- map.where("state", lon, lat)
    
    # update the reactive variable to trigger changes in the plots 
    st <- gsub(":.*", "", st) # get everything until : character 
    
    reac_state(as.character(states@data %>% filter(NAME == st) %>% subset(select = STUSPS)))
    
    reac_state_code(as.character(states@data %>% filter(NAME == st) %>% subset(select = STATEFP)))
    
    #Time to load Texas when cb = TRUE, 7.3 seconds, 12.8 seconds without
    
    df_county <- df %>% 
      filter(state_code==reac_state()) %>%
      mutate(white_app_rate = round(white_apps / total_loans,3)*100,
             hisp_app_rate = round(hisp_apps / total_loans,3)*100,
             black_app_rate = round(black_apps / total_loans,3)*100,
             asian_app_rate = round(asian_apps / total_loans,3)*100,
             white_denial_rate=round(white_fails / white_apps,3)*100,
             hisp_denial_rate=round(hisp_fails / hisp_apps,3)*100,
             black_denial_rate=round(black_fails / black_apps,3)*100,
             asian_denial_rate=round(asian_fails / asian_apps,3)*100)
    
    df_county$denial_rate <- ifelse(df_county$total_loans < 100, NA, df_county$denial_rate)
    df_county$white_denial_rate <- ifelse(df_county$white_apps < 100, NA, df_county$white_app_rate)
    df_county$hisp_denial_rate <- ifelse(df_county$hisp_apps < 100, NA, df_county$hisp_app_rate)
    df_county$black_denial_rate <- ifelse(df_county$black_apps < 100, NA, df_county$black_app_rate)
    df_county$asian_denial_rate <- ifelse(df_county$asian_apps < 100, NA, df_county$asian_denial_rate)
    
    df_county <- df_county %>% 
      subset(select = -c(white_fails,hisp_fails,black_fails,asian_fails,white_apps,hisp_apps,black_apps,asian_apps))
    
    counties <- counties(state=reac_state_code(), cb=TRUE)
    counties <- as_Spatial(counties, cast = TRUE)
    counties <- sp::merge(counties, df_county, by.x="GEOID", by.y="county_code", all=TRUE)
    
    pal <- colorNumeric(
      palette = "Reds", domain = counties$denial_rate, na.color = "gray"
    )
    
    output$map = renderLeaflet({
      labs <- lapply(seq(nrow(counties)), function(i) {
        paste0("<h4>",counties@data[i,"NAMELSAD"],"</h4>",
               '<p><strong>Overall denial rate:</strong> ',counties@data[i,"denial_rate"],'%, Total no. of applications: ', counties@data[i,"total_loans"],'<p>', 
               '<p><strong>% of white |</strong> applicants denied: ',counties@data[i,"white_denial_rate"],'%, Portion of applicants: ', counties@data[i,"white_app_rate"],'%</p>',
               '<p><strong>% of Black |</strong> applicants denied: ',counties@data[i,"black_denial_rate"],'%, Portion of applicants: ', counties@data[i,"black_app_rate"],'%</p>',
               '<p><strong>% of Hispanic or Latino |</strong> applicants denied: ',counties@data[i,"hisp_denial_rate"],'%, Portion of applicants: ', counties@data[i,"hisp_app_rate"],'%</p>',
               '<p><strong>% of Asian |</strong> applicants denied: ',counties@data[i,"asian_denial_rate"],'%, Portion of applicants: ', counties@data[i,"asian_app_rate"],'%</p>',
               '<p><i>Rates only displayed with 100 or more records</i></p>')
      })
      
      
      #If we made this variable reactive as well then it wouldn't clear the state selection on each click.
      m <- m %>% 
        addPolygons(data=counties, 
                    weight = 1.5, 
                    fillColor = ~pal(denial_rate), 
                    color = "white",
                    label= ~lapply(labs, htmltools::HTML),
                    fillOpacity = .95) %>% 
        clearControls() %>% 
        setView(lon-5, lat, 6) %>% 
        addLegend(
          pal = pal, 
          values = ~counties$denial_rate,
          title = "Overall denial rate",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
      
    }) ### END OF NEW COUNTIES LAYER MAP
    
  }) # End of observeEvent(input$map_shape_click)
  
  
  #Because of spacing issues, we're going to generate four separate Plotly outputs for each race/eth.
  #Plot for Non-Hispanic White
  output$whitePlot <- renderPlotly({ 
    fig <- plot_ly(x = c(nhw()), 
                   y = c(0.5),
                   height = 85,
                   type = 'scatter',
                   mode= 'markers',
                   hoverinfo = 'x', hovertemplate = '%{x:.1f}%',
                   marker = list(symbol = "diamond-tall", color = 'rgb(0,0,0)', size = 12)) %>% 
      layout(
        images = heatbar
      ) %>%
      layout(
        title = list(
          text="NON-HISPANIC WHITE",
          font = list(size = 12),
          x = 0.1
        ),
        xaxis = sidebar_xaxis,
        yaxis = sidebar_yaxis  
      )
    fig
  })
  #Plot for Hispanic or Latino
  output$hispPlot <- renderPlotly({
    #Reimplementing as a nicer plotly chart. 
    fig <- plot_ly(x = c(hl()), 
                   y = c(0.5),
                   height = 85,
                   type = 'scatter',
                   mode= 'markers',
                   hoverinfo = 'x', hovertemplate = '%{x:.1f}%',
                   marker = list(symbol = "diamond-tall", color = 'rgb(0,0,0)', size = 12)) %>% 
      layout(
        images = heatbar
      ) %>%
      layout(
        title = list(
          text="HISPANIC OR LATINO",
          font = list(size = 12),
          x = 0.1
        ),
        xaxis = sidebar_xaxis,
        yaxis = sidebar_yaxis  
      )
    fig
  })
  #Plot for Non-Hispanic Black
  output$blackPlot <- renderPlotly({
    #Reimplementing as a nicer plotly chart. 
    fig <- plot_ly(x = c(nhb()), 
                   y = c(0.5),
                   height = 85,
                   type = 'scatter',
                   mode= 'markers',
                   hoverinfo = 'x', hovertemplate = '%{x:.1f}%',
                   marker = list(symbol = "diamond-tall", color = 'rgb(0,0,0)', size = 12)) %>% 
      layout(
        images = heatbar
      ) %>%
      layout(
        title = list(
          text="NON-HISPANIC BLACK",
          font = list(size = 12),
          x = 0.1
        ),
        xaxis = sidebar_xaxis,
        yaxis = sidebar_yaxis  
      )
    fig
  })
  #Plot for Non-Hispanic Asian
  output$asianPlot <- renderPlotly({
    #Reimplementing as a nicer plotly chart. 
    fig <- plot_ly(x = c(nha()), 
                   y = c(0.5),
                   height = 85,
                   type = 'scatter',
                   mode= 'markers',
                   hoverinfo = 'x', hovertemplate = '%{x:.1f}%',
                   marker = list(symbol = "diamond-tall", color = 'rgb(0,0,0)', size = 12)) %>% 
      layout(
        images = heatbar
      ) %>%
      layout(
        title = list(
          text="NON-HISPANIC ASIAN",
          font = list(size = 12),
          x = 0.1
        ),
        xaxis = sidebar_xaxis,
        yaxis = sidebar_yaxis  
      )
    fig
  })
  #END of the four race/eth plots
  
  output$leiPlot <- renderPlotly({
    fig <- plot_ly(lei_df(), y = ~denial_rate_adj*100, x = ~lei_name,
                   color = ~applicant_race_ethnicity, type = 'bar',
                   colors = c("Hispanic or Latino" = '#e41a1c', "Non-Hispanic Asian" = '#377eb8', "Non-Hispanic Black" = '#4daf4a', "Non-Hispanic White" = '#984ea3'), height = 300,hoverinfo = 'y', hovertemplate = '%{y:.1f}%') %>% 
      layout(
        xaxis = list(
          title=""
        ),
        yaxis = list(
          title=""
        )
      )
    fig
    
  })
  #End of lender plot.
  
}

shinyApp(ui, server)

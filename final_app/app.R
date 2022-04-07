library(shiny)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))

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
states <- states[!states@data$STUSPS %in% c("AK", "HI", "VI","GU","MP","PR"), ]
states@data$NAME <- tolower(states@data$NAME)

df <- read_csv("county_level_denialrate.csv")
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
## ADD HERE: LENDER DENIAL RATES FOR RACE / ETHNICITY
df_state <- df %>% 
  filter(!state_code%in%c("AK","HI")) %>% 
  group_by(state_code) %>% 
  summarize(denial_rate = sum(loan_failed) / sum(total_loans))

states@data <- merge(states@data, df_state, by.x = "STUSPS", by.y = "state_code", all=TRUE)

numcat <- 6
cate <- classIntervals(states@data$denial_rate, numcat, style="kmeans")
color.pal <- brewer.pal(numcat,"Reds")
brks <- cate$brks
leg <- list(paste0(round(brks[1],1),"% - ",round(brks[2]*100,1),"%"),
     paste0(round(brks[2]*100,1),"% - ",round(brks[3]*100,1),"%"),
     paste0(round(brks[3]*100,1),"% - ",round(brks[4]*100,1),"%"),
     paste0(round(brks[4]*100,1),"% - ",round(brks[5]*100,1),"%"),
     paste0(round(brks[5]*100,1),"% - ",round(brks[6]*100,1),"%"),
     paste0(round(brks[6]*100,1),"% - ",round(brks[7]*100,1),"%"))

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
      
      h4 {
        padding-left:20%;
        padding-right:20%;
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
          h4("Pct. of mortgage applicants who failed to get a home loan"),
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
          h4("Pct. of mortgage applicants who failed to get a home loan"),
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
    m <<- leaflet(states, options = leafletOptions(zoomControl = FALSE)) %>%
      setView(-115, 38.5, 4) %>% 
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('pk.eyJ1IjoibWstd2lsZGVtYW4iLCJhIjoiY2t4aHRldzJuMDNzejJucG45MjJjYWpucyJ9.9w4xxT5P8uyWW616aKDpag'))) %>% 
      addPolygons(
        fillColor = color.pal,
        weight = 2.5,
        opacity = 1,
        color = "white",
        fillOpacity = .95
      ) %>% 
      addLegend(
        colors = color.pal, 
        labels = leg, 
        position = "topright", 
        title="Denial rate",
        opacity = .95
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
    
    counties <- counties(state=reac_state_code(), cb=TRUE)
    counties <- as_Spatial(counties, cast = TRUE)
    
    df_county <- df %>% 
      filter(state_code==reac_state()) %>% 
      group_by(county_code) %>% 
      summarize(denial_rate = sum(loan_failed) / sum(total_loans))
    counties@data <- merge(counties@data, df_county, by.x="GEOID", by.y="county_code", all.x=TRUE)
    
    numcat <- 6
    cate <- classIntervals(counties@data$denial_rate, numcat, style="kmeans")
    color.pal <- brewer.pal(numcat,"Reds")
    brks <- cate$brks
    leg <- list(paste0(round(brks[1],1),"% - ",round(brks[2]*100,1),"%"),
                paste0(round(brks[2]*100,1),"% - ",round(brks[3]*100,1),"%"),
                paste0(round(brks[3]*100,1),"% - ",round(brks[4]*100,1),"%"),
                paste0(round(brks[4]*100,1),"% - ",round(brks[5]*100,1),"%"),
                paste0(round(brks[5]*100,1),"% - ",round(brks[6]*100,1),"%"),
                paste0(round(brks[6]*100,1),"% - ",round(brks[7]*100,1),"%"))
    
    output$map = renderLeaflet({
      popup <- paste("popup text would go here")
      
      #If we made this variable reactive as well then it wouldn't clear the state selection on each click.
      m <- m %>% 
        addPolygons(data=counties, weight = 1.5, fillColor = color.pal, color = "white",fillOpacity = .95) %>% 
        clearControls() %>% 
        setView(lon-5, lat, 6) %>% 
        addLegend(
          colors = color.pal, 
          labels = leg, 
          position = "topright", 
          title="Denial rate",
          opacity = .95
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
          #tickvals=list(0,10,20,30,40,50,60,70,80,90,100),
          #ticktext=list("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"),
          title=""
        )
      )
    fig
    
  })
  #End of lender plot.
  
}

shinyApp(ui, server)


#

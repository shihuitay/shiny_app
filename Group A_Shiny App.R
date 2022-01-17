library(shiny)
library(shinythemes)
library(leaflet)
library(leafpop)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(lattice)

id1 <-
  "1aqBWSnTrY-vMKzZYYYNio_FT8oNawwEz" # sharable google drive link
df_2020 <-
  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id1))

id2 <- "1yF4-0DzqJ8OMUXGzWSzhk8by1p_7VMcZ"
df_2021 <-
  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id2))

id3 <- "1FhhyiD61aBLQWov3f_Os2ZhrQXGYyR30"
df_national_2020 <- 
  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id3))

id4 <- "1NsQZmLGuRSIDuYmRQ8smgysAqq_2k1zz"
df_national_2021 <- 
  read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id4))

not_sel <- "Not Selected"

states <- c(
  "Johor",
  "Kedah",
  "Kelantan",
  "Kuala-Lumpur",
  "Melaka",
  "Negeri-Sembilan",
  "Pahang",
  "Perak",
  "Perlis",
  "Pulau-Pinang",
  "Putrajaya",
  "Selangor",
  "Terengganu",
  "Sabah",
  "Sarawak"
)

metrics <-
  c(
    '4G Availability',
    'Upload Speed Experience',
    'Download Speed Experience',
    'Video Experience',
    'Games Experience',
    'Voice App Experience'
  )

lat <- c(
  2.0290671689888127,
  5.798945766401923,
  5.274610071675809,
  3.183203322127951,
  2.315433424798207,
  2.7718930950723744,
  3.7285983969141667,
  4.760891207579571,
  6.4878649605926055,
  5.3427464505364775,
  2.9215326102734442,
  3.257887938826263,
  4.940334855185515,
  5.354777046926457,
  2.582970475516899
)

lng <- c(
  103.41435804839205,
  100.68980351396789,
  102.0497051288232,
  101.77853883722334,
  102.30881458302078,
  102.2063037700581,
  102.560747464195,
  101.10740379164758,
  100.26771704782558,
  100.46877380006322,
  101.69848274636746,
  101.49252471778672,
  102.974038797285,
  117.17443340569622,
  113.28445091564592
)

df_location <- data.frame(states, lat, lng)

bar_2020 <- function(metric, state) {
  df_2020 <- df_2020 %>%
    filter(Metrics == metric, id == tolower(state)) %>%
    select(2:7)
  
  col_order <-
    c('Yes', 'DiGi', 'U.Mobile', 'Unifi', 'Celcom', 'Maxis')
  df_2020 <- df_2020[, col_order]
  df_2020 = df_2020[, !sapply(df_2020, function(x)
    mean(is.na(x))) > 0]
  mx <- t(df_2020)
  df_t <- data.frame(mx)
  df_t$provider <- rownames(df_t)
  rownames(df_t) <- NULL
  names(df_t) <- c('score', 'provider')
  if(metric=='Upload Speed Experience' | metric=='Download Speed Experience')
  {graph_title<-'Internet Speed (mbps)'}
  else{graph_title<-'Internet Speed (1-100 points)'}
  graph <-
    barchart(
      provider ~ score,
      data = df_t,
      main = graph_title,
      xlab = toString(metric),
      xlim = c(0, max(mx) * 1.2),
      col = "#f1c40f"
    )
  return(graph)
}

bar_2021 <- function(metric, state) {
  df_2021 <- df_2021 %>%
    filter(Metrics == metric, id == tolower(state)) %>%
    select(2:7)
  
  col_order <-
    c('Yes', 'DiGi', 'U.Mobile', 'Unifi', 'Celcom', 'Maxis')
  df_2021 <- df_2021[, col_order]
  df_2021 <-
    df_2021[, !sapply(df_2021, function(x)
      mean(is.na(x))) > 0]
  mx <- t(df_2021)
  df_t <- data.frame(mx)
  df_t$provider <- rownames(df_t)
  rownames(df_t) <- NULL
  names(df_t) <- c('score', 'provider')
  if(metric=='Upload Speed Experience' | metric=='Download Speed Experience')
  {graph_title<-'Internet Speed (mbps)'}
  else{graph_title<-'Internet Speed (1-100 points)'}
  graph <-
    barchart(
      provider ~ score,
      data = df_t,
      main = graph_title,
      xlab = toString(metric),
      xlim = c(0, max(mx) * 1.2),
      col = "#27ae60"
    )
  return(graph)
}

bar_national_2020 <- function(metric_national) {
  df_national_2020 <- df_national_2020 %>%
    filter(Metrics == metric_national) %>%
    select('Provider','Value')

  if(metric_national=='Upload Speed Experience' | metric_national=='Download Speed Experience')
  {graph_title<-'Internet Speed (mbps) - 2020'}
  else{graph_title<-'Internet Speed (1-100 points) - 2020'}
  graph <-
    barchart(
      Provider ~ Value,
      data = df_national_2020,
      main = graph_title,
      xlab = toString(metric_national),
      col = c("#27ae60",
              "#2c3e50",
              "#f1c40f",
              "#e67e22",
              "#e74c3c")
    )
  return(graph)
}

bar_national_2021 <- function(metric_national) {
  df_national_2021 <- df_national_2021 %>%
    filter(Metrics == metric_national) %>%
    select('Provider','Value')
  if(metric_national=='Upload Speed Experience' | metric_national=='Download Speed Experience')
  {graph_title<-'Internet Speed (mbps) - 2021'}
  else{graph_title<-'Internet Speed (1-100 points) - 2021'}
  graph <-
    barchart(
      Provider ~ Value,
      data = df_national_2021,
      main = graph_title,
      xlab = toString(metric_national),
      col = c("#2c3e50",
             "#f1c40f",
             "#27ae60",
             "#e67e22",
             "#e74c3c")
    )
  return(graph)
}

gps <- function(state) {
  df_location <- df_location %>%
    filter(states == state) %>%
    select(2:3)
  return(df_location)
}

main_page <- tabPanel(title = h4("National"),
                      titlePanel(h4("Please select one of the Metrics")),
                      mainPanel(
                        actionButton("fourg", "4G Availability"),
                        actionButton("upload", "Upload Speed Experience"), 
                        actionButton("download", "Download Speed Experience"),
                        actionButton("video", "Video Experience"), 
                        actionButton("game", "Games Experience"),
                        actionButton("voice", "Voice App Experience"), 
                        hr(),
                        splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))
                      ))

sec_page <- tabPanel(title = h4("Regional"),
                     titlePanel(h4("Please select your preference")),
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(
                           "year",
                           label = "Year",
                           choices = list("2020" = 2020, "2021" = 2021),
                           inline = TRUE,
                           selected = 2020
                         ),
                         hr(),
                         selectInput("metric", "Metric", choices = c(not_sel, metrics)),
                         selectInput("state", "State", choices = c(not_sel, states)),
                         actionButton(
                           "apply_button",
                           "Apply Selections",
                           icon = icon("arrow-alt-circle-right")
                         ),
                       ),
                       mainPanel(
                         leafletOutput('map', width = "100%", height = 600),
                         textOutput('text')
                       )
                     )
                     )
third_page <- tabPanel(title = h4("About"),
                       h3(strong("An overview of the dataset:")),
                       p("Two datasets (National Analysis and Regional Analysis), each containing the mobile network experience data of years 2020 and 2021, were obtained from OpenSignal.com using web scraping.  The datasets contain several metrics, namely 4G Availability, Upload Speed Experience, Download Speed Experience, Video Experience, Games Experience, and Voice App Experience to compare the mobile network experience of internet users on Malaysia's five national operators: Celcom, Maxis, DiGi, U Mobile and Unifi."),
                       
                       p("The National Analysis dataset contain the metrics for the nation in general, comparing the mobile network experience provided by the five operators."),
                       
                       p("The Regional Analysis dataset contains the metrics for each of the 13 states and two federal territories in Malaysia, comparing the mobile network experience provided by the five operators as well as the 4G-only network provider 'Yes'."),
                       br(),
                       br(),
                       br(),
                       h3(strong("The definition of each metric is provided as follows:")),
                       
                       
                       p("Video Experience quantifies the quality of mobile video experienced by users on real-world video streams. To calculate Video Experience, video streams from end-user devices were measured directly, using an ITU-based approach to quantify factors such as load times, stalling and video resolution over an operator's networks. Video Experience for each operator is calculated on a scale from 0 to 100."),
                       
                       
                       p("Games Experience metric is a measure of how mobile users experience real-time multiplayer mobile gaming on an operator's network. Measured on a scale of 0-100, it analyzes how the multiplayer mobile Games Experience is affected by mobile network conditions including latency, packet loss and jitter to determine the impact on gameplay and the overall multiplayer Games Experience. Games Experience for each operator is calculated on a scale from 0 to 100."),
                       
                       
                       p("Voice App Experience measures the quality of experience for over-the-top (OTT) voice services - mobile voice apps such as WhatsApp, Skype, Facebook Messenger etc. - using a model derived from the International Telecommunication Union (ITU)-based approach for quantifying overall voice call quality and a series of calibrated technical parameters. This model characterizes the exact relationship between the technical measurements and perceived call quality. Voice App Experience for each operator is calculated on a scale from 0 to 100."),
                       
                       
                       p("Download Speed Experience shows the average download speed experienced by users across an operator's networks."),
                       
                       p("Upload Speed Experience measures the average upload speeds experienced by users across an operator's networks."),
                       
                       
                       p("4G Availability measures the average proportion of time users spend with a 4G or better connection on each operator's network. 4G Availability is not a measure of coverage or the geographic extent of a network."),
                       br(),
                       br(),
                       br(),
                       h3(strong("Please refer to the following user guide:")),
                       
                       p("In the 'National' tab, user can click on any metric, and two bar charts of years 2020 and 2021, each showing the mobile network experience provided by the five operators, will be shown side by side for easy comparison."),
                       
                       p("In the 'Regional' tab, user has to select the year (2020 or 2021), the metric and the state. Then, a bar chart showing the mobile network experience provided by the six operators will be shown on top of the selected state on the map of Malaysia."),
                       br(),
                       p('From the output, user will be able to identify the most optimal telco operator (Celcom, Maxis, DiGi, U Mobile, Unifi or Yes) based on his preferred metric and state.')

)



ui <- fluidPage(theme=shinytheme('superhero'),navbarPage(title = h4("Internet Speed Analysis"),
                 main_page,
                 sec_page, third_page))

server <- function(input, output) {
  year_var <- eventReactive(input$apply_button, input$year)
  metric_var <- eventReactive(input$apply_button, input$metric)
  state_var <- eventReactive(input$apply_button, input$state)
  metric_national <- reactiveValues(a = '4G Availability')

  output$map <- renderLeaflet(
    leaflet(options = leafletOptions(
      minZoom = 5, doubleClickZoom = FALSE
    )) %>%
      addTiles() %>%
      fitBounds(
        99.76380822572955,
        6.440324289105044,
        119.5553198616571,
        1.081521249153311
      )
  )
  
  observeEvent(input$apply_button, {
    if (metric_var() != not_sel &
        state_var() != not_sel & year_var() == "2020") {
      graph <- bar_2020(metric_var(), state_var())
      gps <- gps(state_var())
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearPopups() %>%
        addCircleMarkers(
          lng = gps$lng,
          lat = gps$lat,
          popup = popupGraph(graph),
          options = popupOptions(closeButton = TRUE)
        ) %>%
        addPopups(
          lng = gps$lng,
          lat = gps$lat,
          popup = popupGraph(graph),
          options = popupOptions(closeButton = TRUE)
        )
      
    } else if (metric_var() != not_sel &
               state_var() != not_sel & year_var() == "2021") {
      graph <- bar_2021(metric_var(), state_var())
      gps <- gps(state_var())
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearPopups() %>%
        addCircleMarkers(
          lng = gps$lng,
          lat = gps$lat,
          popup = popupGraph(graph),
          options = popupOptions(closeButton = TRUE)
        ) %>%
        addPopups(
          lng = gps$lng,
          lat = gps$lat,
          popup = popupGraph(graph),
          options = popupOptions(closeButton = TRUE)
        )
    }
  })

  observeEvent(input$fourg, {
    metric_national$a <- '4G Availability'
  })
  
  observeEvent(input$upload, {
    metric_national$a <- 'Upload Speed Experience'
  })
  
  observeEvent(input$download, {
    metric_national$a <- 'Download Speed Experience'
  })
  
  observeEvent(input$video, {
    metric_national$a <- 'Video Experience'
  })
  
  observeEvent(input$game, {
    metric_national$a <- 'Games Experience'
  })

  observeEvent(input$voice, {
    metric_national$a <- 'Voice App Experience'
  })
  
  output_text <- eventReactive(input$apply_button, {
    if (metric_var() != not_sel & state_var() != not_sel) {
      print("Result according to your selection.")
    } else{
      validate(
        need(metric_var() != not_sel, "Please choose a metric."),
        need(state_var() != not_sel, "Please choose a state.")
      )
    }
  })
  
  output_plot <- eventReactive(input$apply_button_national, {
    if (metric_national_var() != not_sel & year_national_var() == "2020") {
      graph <- bar_national_2020(metric_national_var())
      plot(graph)
    } else if (metric_national_var() != not_sel & year_national_var() == "2021") {
      graph <- bar_national_2021(metric_national_var())
      plot(graph)
    }
  })
  
  
  output$plot1 <- renderPlot(
    bar_national_2020(metric_national$a)
  )
  output$plot2 <- renderPlot(
    bar_national_2021(metric_national$a)
  )
  output$text <- renderText(output_text())
}

shinyApp(ui = ui, server = server)

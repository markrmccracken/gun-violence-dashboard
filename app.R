# Modernized Gun Violence Dashboard - app.R
# Updated for 2025 with Bluesky integration

# Load required libraries
library(shiny)
library(tm)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(wordcloud2)
library(plotly)
library(stringr)
library(httr2)
library(jsonlite)

# Load environment variables
if (file.exists(".env")) {
  readRenviron(".env")
}

# Bluesky API Functions
source("R/bluesky_functions.R")

# Define the %||% operator if it doesn't exist (for Bluesky functions)
`%||%` <- function(x, y) if (is.null(x)) y else x

# UI Definition
ui <- dashboardPage(
  skin = "purple",
  header = dashboardHeader(title = "Gun Violence Dashboard 2025"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "page1", icon = icon("home")),
      menuItem("Mass Shootings Map", tabName = "page2", icon = icon("map-marker", lib="glyphicon")),
      menuItem("Mass Shooting Trends", tabName = "page3", icon = icon("time", lib="glyphicon")),
      menuItem("Mass Shootings Detail", tabName = "page4", icon = icon("screenshot", lib="glyphicon")),
      menuItem("Gun Violence Sentiment", tabName = "page5", icon = icon("heart", lib="glyphicon")),
      tags$script(HTML("$('body').addClass('fixed');"))
    )
  ),
  body = dashboardBody(
    tabItems(
      # About Page
      tabItem(tabName = "page1",
              fluidPage(
                h1("Gun Violence and Mass Shootings in the United States", align = "center"),
                h2(icon("home"), "About", align = "left"),
                p("Gun Violence is a serious issue in the United States. For most of us, however, this issue feels distant as we are not
            exposed to it every day. The times when the issue rises to national attention is typically when there is a mass shooting. Mass 
            shootings are defined as events involving gun violence where four or more people were killed in indiscriminate rampages in a 
            public place (", a(href="https://www.motherjones.com/crime-justice/2012/08/what-is-a-mass-shooting/", target="_blank", "source"), "). 
            Such events often trigger deep conversations about gun rights and mental health."),
                p("This interactive web application is built to explore data around mass shootings in the United States in order to address the four main research questions below and gain
            a better understanding for the trends and issues around mass shootings and gun violence more broadly. On the first tab you will 
            see each mass shooting plotted on the map with details about each incident. The second tab features charts showing the number of incidents
            each year, over time. The third tab highlights various characteristics about the perpetrators of the crimes as related to the victims
            of their crimes. Finally, the fourth tab features a wordcloud created in real time from live posts on Bluesky based on relevant hashtags."),
                
                h2(icon("question"),"Research Questions"),
                tags$ol(
                  tags$li("What is the human cost of gun violence when examining specifically mass shootings?"),
                  tags$li("What kind of people contribute most to the casualty and death tolls of mass shootings?"),
                  tags$li("What legislation can the U.S. government consider implementing that might help reduce the casualty and death tolls of mass shootings?"),
                  tags$li("What are people saying right now in the national conversation around gun violence on social media?")
                ),
                
                h2(icon("th"), "Gun Violence Data Set Information"),
                p(span("Our data set is related to mass shootings in the United States and comes from Mother Jones, a nonprofit American print and online magazine that focuses
            on news and investigative reporting. The dataset is an open source repository documenting mass shootings in the United States and catalogs incidents where four or more
            people were killed in indiscriminate rampages in a public place. Conventionally motivated crimes such as armed robbery or gang violence are excluded from this data set."),
                  " The data continues to be updated and maintained as new incidents occur."),
                
                fluidRow(
                  box(
                    width = 7,
                    title = "Variables",
                    "Key variables in this dataset include case name, date, fatalities, injured, total victims, location type, shooter characteristics (age, race, gender), mental health indicators, weapon legality, and incident summaries.",
                    tags$ul(
                      tags$li(strong("case"),": The name attributed to the mass shooting"),
                      tags$li(strong("date"),": The date of the mass shooting"),
                      tags$li(strong("fatalities"),": The number of people the shooter killed"),
                      tags$li(strong("injured"),": The number of people the shooter injured"),
                      tags$li(strong("ttl.victims"),": The total number of people the shooter killed or injured"),
                      tags$li(strong("location_type"),": The type of location where the shooting took place"),
                      tags$li(strong("age_of_shooter"),": The age of the shooter at the time of the event"),
                      tags$li(strong("prior_signs_mental_health_issues"),": Whether or not the shooter had signs of mental health issues"),
                      tags$li(strong("weapons_obtained_legally"),": Whether or not the weapons were obtained legally"),
                      tags$li(strong("race & gender"),": Demographic information about the shooter")
                    )
                  ),
                  box(
                    width = 5,
                    title = "Data Updates",
                    "This modernized version includes updated data processing and improved visualizations for better analysis and understanding of trends in gun violence."
                  )
                ),
                
                h2(icon("comments"), "Social Media Integration"),
                p("To provide insight into current public discourse around gun violence, we've integrated with Bluesky, a decentralized social network. 
            The application gathers recent posts featuring relevant hashtags and creates word cloud visualizations to show trending topics and sentiments."),
                
                h2(icon("users"), "Development Team"),
                p("This dashboard was originally developed as part of an MBA program project and has been modernized for current web technologies and social media platforms.")
              )
      ),
      
      # Map Tab
      tabItem(tabName = "page2",
              h1("Step 1: Explore the Data on Mass Shootings in the United States"),
              fluidRow(
                box(
                  width = 3,
                  p("To the right is a map of mass shootings in the United States, courtesy of the Mother Jones dataset. 
              Hover over an incident for its name; click to see details including fatalities, date, summary, and news source."),
                  p("The size of each incident represents the total victims, including fatalities and injured."),
                  p("Use the slider below to filter by year range."),
                  sliderInput("yearSlider", label=h4("Select Year(s)"), 
                              min = 1982, max = 2025, value = c(1982, 2025), sep="")
                ),
                box(
                  width = 9,
                  leafletOutput("map", width="100%", height=500)
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h3("Analysis"),
                  p("The map reveals geographical patterns in mass shooting incidents, with concentrations in more populated areas. 
              Notable outliers like the Las Vegas Strip massacre demonstrate the varying scale of these tragic events. 
              The year slider allows exploration of temporal patterns and trends over the decades covered by the dataset.")
                )
              )
      ),
      
      # Trends Tab
      tabItem(tabName = "page3",
              h1("Step 2: Identify Mass Shooting Trends"),
              fluidRow(
                box(
                  width=12,
                  plotlyOutput("allVictims", width="100%", height = 500),
                  h4("Remove Las Vegas Outlier Event:"),
                  checkboxInput("removeOutlier", 
                                label="Check this box to remove the 2017 Las Vegas Strip Massacre from the data set", 
                                value = FALSE)
                )
              ),
              fluidRow(
                box(
                  width = 5,
                  h4("Total Incidents"),
                  plotOutput("Incidents", width = "100%", height = 300)
                ),
                box(
                  width = 7,
                  h3("Analysis"),
                  p("The data shows concerning trends in mass shooting frequency and severity over time. 
              The Las Vegas Strip Massacre represents a significant outlier that can skew overall trend analysis. 
              Use the outlier toggle to examine underlying patterns in the data."),
                  strong("This addresses our first research question about the human cost of gun violence in mass shootings.")
                )
              )
      ),
      
      # Details Tab  
      tabItem(tabName = "page4",
              h1("Step 3: Explore Mass Shootings in Detail"),
              h2("Victims Based on Shooter Characteristics", align="center"),
              fluidRow(  
                box(
                  width = 4,
                  selectInput("characteristic", label = h3("Shooter Characteristic"), 
                              choices = list("race"=1, "gender"=2, "age"=3, "mental health issues"=4,
                                             "fully- or semi- automatic weapon"=5, "legal weapon"=6, "location type"=7),
                              selected=1),
                  radioButtons("selectingVics", label=h3("Show by Fatalities or Total Victims"), 
                               choices=list("Total Victims [Fatalities + Injured]"=1, "Just Fatalities"=2), selected=1),
                  h3("Toggle Outlier"),
                  checkboxInput("toggleOutlier", label="Remove 2017 Las Vegas Outlier", value = FALSE)
                ),
                box(
                  width = 8,
                  plotOutput("waffles", width="100%", height=400)
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h1(textOutput("headline"), align="center", style="color:purple")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h3("How it Works"),
                  p("Select a characteristic from the dropdown to see victim counts colored by shooter demographics. 
              Toggle between total victims and fatalities, and optionally remove the Las Vegas outlier for clearer trends.")
                )
              )
      ),
      
      # Social Media Sentiment Tab
      tabItem(tabName = "page5",
              h1("Step 4: Explore Gun Violence Sentiment on Social Media"),
              p("This page generates word clouds based on recent posts from Bluesky. Content changes based on current social media discussions. 
          Explore different hashtags and adjust the number of posts captured.",
                strong(" This addresses our fourth research question about current public discourse.")),
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Parameters", width = 12,
                    selectInput("selecthash", label= h3("Choose a Hashtag to Explore"),
                                choices = list("#gunviolence" = "#gunviolence",
                                               "#guncontrol" = "#guncontrol", 
                                               "#gunrights" = "#gunrights", 
                                               "#gunsafety" = "#gunsafety",
                                               "#massshooting" = "#massshooting",
                                               "#secondamendment" = "#secondamendment"),
                                selected = "#gunviolence"),
                    sliderInput(inputId = "slidermark", label = "Number of Posts to Capture", 
                                min = 40, max = 200, value = 50),
                    br(),
                    actionButton("goButton", "Generate Word Cloud", class = "btn-primary"),
                    p("Click the button to update the word cloud with current posts")
                  ),
                  box(
                    title = "Hashtag Descriptions",
                    width = 12,
                    tags$ul(
                      tags$li(strong("#gunviolence"),": General discussion about violence involving firearms"),
                      tags$li(strong("#guncontrol"),": Discussions about firearm regulation and policy"),
                      tags$li(strong("#gunrights"),": Conversations about gun ownership rights"),
                      tags$li(strong("#gunsafety"),": Focus on safe firearm practices and storage"),
                      tags$li(strong("#massshooting"),": Direct discussion of mass shooting events"),
                      tags$li(strong("#secondamendment"),": Constitutional and legal discussions about firearm rights")
                    )
                  )
                ),
                column(
                  width = 8,
                  box(
                    title = "Social Media Sentiment Word Cloud", width = 12, height = "840", 
                    wordcloud2Output("wordcloud1", height = "750px")
                  )
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Data Loading and Cleaning
  MJFull <- reactive({
    data_path <- file.path("www", "MJFull.csv")
    if (file.exists(data_path)) {
      read.csv(data_path, header = TRUE, blank.lines.skip = TRUE, stringsAsFactors = FALSE)
    } else {
      data.frame(
        case = "Sample Data",
        date = "1/1/20",
        fatalities = 4,
        injured = 2,
        ttl.victims = 6,
        location = "Other",
        age_of_shooter = 25,
        race = "White",
        gender = "M",
        prior_signs_mental_health_issues = "Yes",
        weapons_obtained_legally = "Yes",
        Were.any.guns.used.semi.automatic.or.fully.auto.matic. = "yes",
        latitude = 39.8283,
        longitude = -98.5795,
        summary = "Sample data for testing",
        one_source = "http://example.com",
        year = 2020
      )
    }
  })
  
  # Data processing reactive
  MJ <- reactive({
    data <- MJFull()
    if (nrow(data) == 0) return(data.frame())
    
    # Convert latitude and longitude to numeric
    if ("latitude" %in% names(data)) {
      data$latitude <- as.numeric(as.character(data$latitude))
    }
    if ("longitude" %in% names(data)) {
      data$longitude <- as.numeric(as.character(data$longitude))
    }
    
    # Create age categories
    if ("age_of_shooter" %in% names(data)) {
      data$age_of_shooter <- as.numeric(as.character(data$age_of_shooter))
      
      data$ageCat <- case_when(
        is.na(data$age_of_shooter) ~ "Unknown",
        data$age_of_shooter < 20 ~ "10s (Teens)",
        data$age_of_shooter >= 20 & data$age_of_shooter < 30 ~ "20s",
        data$age_of_shooter >= 30 & data$age_of_shooter < 40 ~ "30s", 
        data$age_of_shooter >= 40 & data$age_of_shooter < 50 ~ "40s",
        data$age_of_shooter >= 50 ~ "50+",
        TRUE ~ "Unknown"
      )
    } else {
      data$ageCat <- "Unknown"
    }
    
    return(data)
  })
  
  # Map functionality
  subData <- reactive({
    data <- MJ()
    if (nrow(data) == 0) return(data)
    data[data$year >= input$yearSlider[1] & data$year <= input$yearSlider[2], ]
  })
  
  output$map <- renderLeaflet({
    data <- subData()
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = -100, lat = 40, zoom = 4))
    }
    
    # Remove rows with missing coordinates
    data <- data[!is.na(data$latitude) & !is.na(data$longitude), ]
    
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = -100, lat = 40, zoom = 4))
    }
    
    # Create popup content
    popup_content <- paste(
      sep = "<br/>",
      paste0("<b>", data$case, "</b>"),
      paste0("<b>Date:</b> ", data$date),
      paste0("<b>Total Victims:</b> ", data$ttl.victims, " (", data$fatalities, " Dead)"),
      paste0("<b><a href='", data$one_source,"' target= _blank' rel='noopener noreferrer'>News Source for the Incident</a></b>"),
      paste0("<b>Incident Details:</b> ", subData()$summary)
      
    )
    
    leaflet() %>%
      addTiles() %>%
      addCircles(
        data = data, 
        lng = ~longitude, 
        lat = ~latitude, 
        color = "red", 
        radius = ~(ttl.victims^1.5),
        popup = popup_content,
        label = ~case
      ) %>%
      setView(lng = -100, lat = 40, zoom = 4)
  })
  
  # Charts functionality
  MJyear <- reactive({
    data <- MJ()
    if (nrow(data) == 0) return(data.frame())
    
    if (!input$removeOutlier) {
      filtered_data <- data
    } else {
      filtered_data <- data[data$ttl.victims < 103, ]
    }
    
    # Aggregate by year
    year_summary <- filtered_data %>%
      group_by(year) %>%
      summarise(
        ttl.victims = sum(ttl.victims, na.rm = TRUE),
        fatalities = sum(fatalities, na.rm = TRUE),
        injured = sum(injured, na.rm = TRUE),
        total_incidents = n(),
        .groups = 'drop'
      )
    
    return(year_summary)
  })
  
  output$Incidents <- renderPlot({
    data <- MJyear()
    if (nrow(data) == 0) return(ggplot())
    
    ggplot(data, aes(x = year, y = total_incidents)) +
      geom_line() +
      geom_point() +
      geom_smooth(se = FALSE) +
      ggtitle("Number of Mass Shootings Per Year") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Year", y = "Total Incidents")
  })
  
  output$allVictims <- renderPlotly({
    data <- MJyear()
    if (nrow(data) == 0) return(plotly_empty())
    
    p <- ggplot(data) +
      geom_point(aes(x = year, y = ttl.victims, color = "Total Victims")) +
      geom_point(aes(x = year, y = fatalities, color = "Fatalities")) +
      geom_point(aes(x = year, y = injured, color = "Injured")) +
      geom_line(aes(x = year, y = ttl.victims, color = "Total Victims")) +
      geom_line(aes(x = year, y = fatalities, color = "Fatalities")) +
      geom_line(aes(x = year, y = injured, color = "Injured")) +
      ggtitle("Mass Shooting Victims Per Year") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Year", y = "Number of People", color = "Category")
    
    ggplotly(p)
  })
  
  # Bar chart functionality
  output$waffles <- renderPlot({
    data <- MJ()
    if (nrow(data) == 0) return(ggplot())
    
    # Apply outlier filter
    if (input$toggleOutlier) {
      data <- data[data$ttl.victims < 103, ]
    }
    
    # Select what to count: total victims or just fatalities
    count_column <- if (input$selectingVics == 1) "ttl.victims" else "fatalities"
    
    # Select which characteristic to analyze
    if (input$characteristic == 1) {
      char_column <- "race"
      title_text <- "Mass Shooting Victims by Shooter's Race"
    } else if (input$characteristic == 2) {
      char_column <- "gender"
      title_text <- "Mass Shooting Victims by Shooter's Gender"
    } else if (input$characteristic == 3) {
      char_column <- "ageCat"
      title_text <- "Mass Shooting Victims by Shooter's Age"
    } else if (input$characteristic == 4) {
      char_column <- "prior_signs_mental_health_issues"
      title_text <- "Mass Shooting Victims by Mental Health History"
    } else if (input$characteristic == 5) {
      char_column <- "Were.any.guns.used.semi.automatic.or.fully.auto.matic."
      title_text <- "Mass Shooting Victims by Weapon Type"
    } else if (input$characteristic == 6) {
      char_column <- "weapons_obtained_legally"
      title_text <- "Mass Shooting Victims by Weapon Legality"
    } else {
      char_column <- "location"
      title_text <- "Mass Shooting Victims by Location Type"
    }
    
    # Check if the column exists
    if (!char_column %in% names(data)) {
      return(ggplot() + 
               ggtitle(paste("Column", char_column, "not found in data")) +
               theme_minimal())
    }
    
    # Aggregate data by characteristic
    plot_data <- data %>%
      group_by(.data[[char_column]]) %>%
      summarise(total = sum(.data[[count_column]], na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(.data[[char_column]]) & .data[[char_column]] != "")
    
    # Create the plot
    ggplot(plot_data, aes(x = reorder(.data[[char_column]], total), y = total)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = title_text,
        subtitle = paste("Total:", sum(plot_data$total)),
        x = "Category",
        y = if (input$selectingVics == 1) "Total Victims" else "Fatalities"
      ) +
      theme(plot.title = element_text(size = 12, hjust = 0.5))
  })
  
  output$headline <- renderText({
    data <- MJ()
    if (nrow(data) == 0) return("No data available")
    
    # Apply outlier filter
    if (input$toggleOutlier) {
      data <- data[data$ttl.victims < 103, ]
    }
    
    # Map characteristic numbers to your actual column names
    char_column <- switch(as.character(input$characteristic),
                          "1" = "race",
                          "2" = "gender", 
                          "3" = "ageCat",
                          "4" = "prior_signs_mental_health_issues",
                          "5" = "Were.any.guns.used.semi.automatic.or.fully.auto.matic.",
                          "6" = "weapons_obtained_legally",
                          "7" = "location"
    )
    
    if (is.null(char_column) || !char_column %in% names(data)) {
      return("Column not found")
    }
    
    count_column <- if (input$selectingVics == 1) "ttl.victims" else "fatalities"
    
    # Calculate percentages
    totals <- data %>%
      group_by(.data[[char_column]]) %>%
      summarise(total = sum(.data[[count_column]], na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(.data[[char_column]]) & .data[[char_column]] != "")
    
    if (nrow(totals) > 0) {
      max_category <- totals[which.max(totals$total), ]
      percentage <- round((max_category$total / sum(totals$total)) * 100)
      
      return(paste0(percentage, "% of victims were affected by shooters with characteristic: ", 
                    max_category[[char_column]]))
    }
    
    "No data found for this characteristic"
  })
  
  # Bluesky Integration
  bluesky_session <- reactiveVal(NULL)
  
  # Initialize Bluesky session on app start (if credentials available)
  observe({
    username <- Sys.getenv("BLUESKY_USERNAME")
    password <- Sys.getenv("BLUESKY_PASSWORD")
    
    if (username != "" && password != "") {
      tryCatch({
        session <- authenticate_bluesky(username, password)
        bluesky_session(session)
      }, error = function(e) {
        message("Bluesky authentication failed: ", e$message)
      })
    }
  })
  
  output$wordcloud1 <- renderWordcloud2({
    input$goButton
    isolate({
      session <- bluesky_session()
      if (!is.null(session)) {
        GrabBlueskyPosts(input$selecthash, input$slidermark, session)
      } else {
        # Fallback wordcloud when no Bluesky session
        sample_data <- data.frame(
          word = c("authentication", "required", "bluesky", "configure", "credentials", "social", "media"),
          freq = c(5, 4, 6, 3, 3, 2, 2)
        )
        wordcloud2(sample_data, size = 2, color = "gray", backgroundColor = "white")
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
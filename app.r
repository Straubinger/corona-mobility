
# Define global parameters ------------------------------------------------

# Install and load packages ----
if(!require(pacman))
{install.packages("pacman")}

# Install/update and load covdata package ----
remotes::install_github("kjhealy/covdata")
library(covdata)

# Load packages ----
pacman::p_load(shiny, shinydashboard, shinyWidgets, tidyverse, plotly, scales, RColorBrewer, tools, Cairo)

# Load Google data from the covdata package ----
dta_google <- google_mobility %>%
  filter(is.na(sub_region_2)) %>%
  mutate(date = as.Date(date),
         pct_diff = pct_diff / 100,
         Percent = percent(pct_diff),
         sub_region_1 = replace_na(sub_region_1, "All regions")) %>% 
  rename(Place = type,
         Date = date)

# Load Apple data from the covdata package ----
dta_apple <- apple_mobility %>%
  filter(geo_type %in% c("country/region", "sub-region")) %>%
  mutate(date = as.Date(date),
         index = (index - 100)/100,
         Percent = percent(index),
         country = coalesce(country, region),
         region = case_when(geo_type == "country/region" ~ "All regions",
                              TRUE ~ region)) %>% 
  rename(`Transportation type` = transportation_type,
          Date = date)

# Create vectors for Google countries, places and last update ----
country_google <- unique(dta_google$country_region)
type_google <- unique(dta_google$Place)
date_google <- max(dta_google$Date)

# Create vectors for Apple countries, transportations types and last update ----
country_apple <- unique(dta_apple$country)
type_apple <- unique(dta_apple$`Transportation type`)
date_apple <- max(dta_apple$Date)


# Define UI ---------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  
  # Header with app title and links  ----
  dashboardHeader(
    title = "COVID-19 Mobility",
    
    # Link to covdata package ----
    tags$li(
      class = "dropdown",
      tags$a(href = "https://github.com/kjhealy/covdata", icon("github"), "|", "covdata")
    ),
    
    # Link to source code on GitHub ----
    tags$li(
      class = "dropdown",
      tags$a(href = "https://github.com/Straubinger/corona-mobility", icon("github"), "|", "Source code")
    )
    
  ),
  
  # Sidebar menu tabs ----
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem("Google", tabName = "google", icon = icon("google")),
      menuItem("Apple", tabName = "apple", icon = icon("apple"))
    ),
    
    # Sidebar menu options - Google ----
    conditionalPanel(
      condition = 'input.menu == "google"',
      selectInput(
        "country_g",
        label = "Country",
        selected = c("Denmark"),
        choices = country_google,
        multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = 'input.menu == "google"',
      selectInput(
        "region_g",
        label = "Region",
        choices = NULL,
        multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = 'input.menu == "google"',
      selectInput(
        "places",
        label = "Places",
        selected = c("retail"),
        choices = type_google,
        multiple = TRUE
      )
    ),
    
    # Sidebar menu options - Apple ----
    conditionalPanel(
      condition = 'input.menu == "apple"',
      selectInput(
        "country_a",
        label = "Country",
        selected = c("Denmark"),
        choices = country_apple,
        multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = 'input.menu == "apple"',
      selectInput(
        "region_a",
        label = "Region",
        choices = NULL,
        multiple = FALSE
      )
    ),
    conditionalPanel(
      condition = 'input.menu == "apple"',
      selectInput(
        "transportation",
        label = "Transportation types",
        selected = c("driving"),
        choices = type_apple,
        multiple = TRUE
      )
    ),
    
    # Choose between a single plot or small multiples ----
    radioGroupButtons(
      inputId = "id01",
      label = "Chart type",
      choices = c("Single",
                  "Small multiples"),
      justified = TRUE
    ),
    br(),
    
    # Info about data and app developer ----
    conditionalPanel(
      condition = 'input.menu == "google"',
      helpText("The baseline is the median value for the corresonding day of the week during the 
               5-week period Jan 3-Feb 6 2020. More info:",br(),br(),
               a("Google Community Mobility Reports", href = "https://www.google.com/covid19/mobility/"),
               style = "padding-left:1em; padding-right:1em; color:white"
               ),
    ),
    conditionalPanel(
      condition = 'input.menu == "apple"',
      helpText("The baseline is the activity level on Jan 13 2020. More info:",br(),br(),
               a("Apple Mobility Trends Reports", href = "https://www.apple.com/covid19/mobility"),
               style = "padding-left:1em; padding-right:1em; color:white"
      ),
    ),
    helpText("Developed by ", 
             a("@StraubingerDK", href = "https://twitter.com/straubingerdk"),
             style = "padding-left:1em; padding-right:1em; bottom:1em; position:absolute"),
    width = 300
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(
      'body {font-family: Verdana;}'
    )),
    tags$style(
      HTML('.rightAlign{margin-left: 1360px;}')
    )),
    
    # Dashboard body with plots ----
    tabItems(
      tabItem(tabName = "google",
              fluidRow(column(
                width = 12,
                box(
                  plotlyOutput("google", height = "840px"),
                  width = NULL,
                  solidHeader = TRUE
                )
              ))), 
      tabItem(tabName = "apple",
              fluidRow(column(
                width = 12,
                box(
                  plotlyOutput("apple", height = "840px"),
                  width = NULL,
                  solidHeader = TRUE
                )
              )))
    )
  )
)


# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  options(shiny.usecairo = TRUE)
  
  theme_set(theme_minimal())
  
  # Create depedent dropdown for Google regions ----
  choices_region_g <- reactive({
    choices_region <- dta_google %>%
      select(country_region, sub_region_1) %>%
      unique() %>%
      filter(country_region == input$country_g)
  })
  
  observe({
    updateSelectInput(
      session = session,
      inputId = "region_g",
      choices = choices_region_g()$sub_region_1
    )
  }) 
  
  # Create depedent dropdown for Apple regions ----
  choices_region_a <- reactive({
    choices_region <- dta_apple %>%
      select(region, country) %>%
      unique() %>%
      filter(country == input$country_a)
  })
  
  observe({
    updateSelectInput(session = session,
                      inputId = "region_a",
                      choices = choices_region_a()$region)
  }) 
  
  # Subset Google data on country, region and places ----
  google <- reactive({
    dta_google %>% filter(
      country_region == input$country_g &
        sub_region_1 == input$region_g &
        Place %in% input$places
    ) %>%
      mutate(Place = toTitleCase(Place))
  })
  
  # Subset Google data on country, region and transportation types ----
  apple <- reactive({
    dta_apple %>% filter(
      country == input$country_a &
        region == input$region_a &
        `Transportation type` %in% input$transportation
    ) %>%
      mutate(`Transportation type` = toTitleCase(`Transportation type`))
  })
  
  # Plot Googel data with plotly ----  
  output$google <- renderPlotly({
    if (input$id01 == "Single") {
      ggplotly(
        ggplot() +
          geom_line(
            data = google(),
            aes(
              x = Date,
              y = pct_diff,
              colour = Place,
              label = Percent
            )
          ) +
          geom_hline(yintercept = 0) +
          scale_x_date(date_labels = "%b %d") +
          scale_y_continuous(labels = scales::percent) +
          scale_color_brewer(palette = "Dark2") +
          labs(
            title = paste0('Google Community Mobility Reports: ', input$country_g, " - ", input$region_g),
            subtitle = paste0("Data updated: ", date_google)
          ) +
          theme(
            legend.title = element_blank(),
            axis.title = element_blank(),
            text = element_text(size = 16)
          )
        ,
        tooltip = c("Place", "Date", "label")
      ) %>%
        config(displayModeBar = F) %>%
        layout(title = list(
          text = paste0('Google Community Mobility Reports: ', input$country_g, " - ", input$region_g,
            '<br>', '<sup>', "Data updated: ", date_google,'</sup>')
        ))
    }
    
    else if (input$id01 == "Small multiples") {
      ggplotly(
        ggplot() +
          geom_line(data = google(), aes(
            x = Date,
            y = pct_diff,
            label = Percent,
            color = "red"
          )) +
          geom_hline(yintercept = 0) +
          facet_wrap(~ Place, ncol = 2) +
          scale_x_date(date_labels = "%b %d") +
          scale_y_continuous(labels = scales::percent) +
          labs(
            title = paste0('Google Community Mobility Reports: ', input$country_g, " - ", input$region_g),
            subtitle = paste0("Data updated: ", date_google)
          ) +
          theme(
            legend.position = "none",
            axis.title = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            text = element_text(size = 16)
          )
        ,
        tooltip = c("Date", "label"
        )
      ) %>%
        config(displayModeBar = F) %>%
        layout(title = list(
          text = paste0('Google Community Mobility Reports: ', input$country_g, " - ", input$region_g,
            '<br>', '<sup>', "Data updated: ", date_google, '</sup>')
        ))
    }
  })
  
  # Plot Apple data with plotly ----  
  output$apple <- renderPlotly({
    if (input$id01 == "Single") {
      ggplotly(
        ggplot() +
          geom_line(
            data = apple(),
            aes(
              x = Date,
              y = index,
              color = `Transportation type`,
              label = Percent
            )
          ) +
          geom_hline(yintercept = 0) +
          scale_x_date(date_labels = "%b %d") +
          scale_y_continuous(labels = scales::percent) +
          scale_color_brewer(palette = "Dark2") +
          labs(
            title = paste0('Apple Mobility Trends Reports: ', input$country_a, " - ", input$region_a),
            subtitle = paste0("Data updated: ", date_apple)
          ) +
          theme(
            legend.title = element_blank(),
            axis.title = element_blank(),
            text = element_text(size = 16)
          )
        ,
        tooltip = c("Transportation type", "Date", "label")
      ) %>%
        config(displayModeBar = F) %>%
        layout(title = list(
          text = paste0('Apple Mobility Trends Reports: ', input$country_a, " - ", input$region_a,
            '<br>', '<sup>', "Data updated: ", date_apple, '</sup>')
        ))
    }
    
    else if (input$id01 == "Small multiples") {
      ggplotly(
        ggplot() +
          geom_line(data = apple(), aes(
            x = Date,
            y = index,
            label = Percent,
            color = "red"
          )) +
          geom_hline(yintercept = 0) +
          facet_wrap(~ `Transportation type`, ncol = 2) +
          scale_x_date(date_labels = "%b %d") +
          scale_y_continuous(labels = scales::percent) +
          labs(
            title = paste0('Apple Mobility Trends Reports: ', input$country_a, " - ", input$region_a),
            subtitle = paste0("Data updated: ", date_apple)
          ) +
          theme(
            legend.position = "none",
            axis.title = element_blank(),
            panel.spacing.x = unit(1, "lines"),
            text = element_text(size = 16)
          )
        ,
        tooltip = c("Date", "label")
      ) %>%
        config(displayModeBar = F) %>%
        layout(title = list(
          text = paste0('Apple Mobility Trends Reports: ', input$country_a, " - ", input$region_a,
            '<br>', '<sup>', "Data updated: ", date_apple, '</sup>')
        ))
    }
  })
}

# Create Shiny app ----
shinyApp(ui, server)
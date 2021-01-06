### LIBRARIES
library(tidyverse)
library(rvest)
library(dplyr)
library(purrr)
library(utils)
library(maps)
library(shiny)
library(shinythemes)
library(rsconnect)
library(reactable)
library(ggiraph)


### SCRAPING INFO
beginning_url <- "https://247sports.com/Season/"
ending_url <- "-Basketball/CompositeRecruitRankings/?InstitutionGroup=HighSchool"
classes <- 2003:2021
final_urls <- paste0(beginning_url, classes, ending_url)


raw_data <- data.frame()
class <- 2003
for (url in final_urls) {
  
  ranks <- read_html(url) %>%
    html_nodes(".primary") %>%
    html_text() %>%
    as.data.frame()
  
  hometowns <- read_html(url) %>%
    html_nodes(".meta") %>%
    html_text() %>%
    as.data.frame()
  
  names <- read_html(url) %>%
    html_nodes(".rankings-page__name-link") %>%
    html_text() %>%
    as.data.frame()
  
  current_url <- cbind(class, ranks, names, hometowns)
  raw_data <- rbind(raw_data, current_url)
  
  class <- class + 1
}
names(raw_data) <- c("Class", "Rank", "Name", "Hometown")
raw_data$Rank <- trimws(raw_data$Rank)
raw_data$Rank <- sub(" .*", "", raw_data$Rank)
raw_data$Rank <- as.numeric(raw_data$Rank)
raw_data[176,4] <- "Hargrave Military Academy (Chatham, VA)" #prev included (HS)
raw_data[190,4] <- "Hargrave Military Academy (Chatham, VA)" #prev included (HS)
raw_data[199,4] <- "Stoneridge Preparatory School (Simi Valley, CA)" #prev included (2)
raw_data[358,4] <- "Stoneridge Preparatory School (Simi Valley, CA)" #prev included (2)
raw_data[413,4] <- "Hargrave Military Academy (Chatham, VA)" #prev included (HS)
raw_data[529,4] <- "Hargrave Military Academy (Cleveland, OH)" #prev included (HS)
raw_data[693,4] <- "Hargrave Military Academy (Marion, SC)" #prev included (HS)
raw_data[660,4] <- "Athlete Institute Basketball Academy (NA, NA)" #prev hometown was blank


# Full 247 Player Database
player_database <- raw_data %>%
  separate(Hometown, c("School", "Hometown", "Homestate", NA), sep = "[\\(\\,\\)]") %>%
  mutate(Rank = ifelse(Rank > 13, (Rank - 1), Rank))
player_database$Homestate <- as.character(apply(player_database[c('Homestate')], 2, str_trim))
player_database$Rank <- as.numeric(player_database$Rank)

# Database of players from the United States
states_and_dc <- append(state.abb, "DC")
player_database_usa <- player_database %>%
  filter(Homestate %in% states_and_dc)

# US Census Bureua population estimates by year
# I use average of state populations since 2002 when first class
# began their senior year of HS
state_populations <- read_csv("USpopulationByState.csv")
state_populations <- state_populations %>%
  separate(`Geographic Area`, c(NA, "area"), sep = "[.]")
state_populations$area <- state.abb[match(state_populations$`area`,state.name)]
state_populations[1,1] <- "USA"
state_populations[10,1] <- "DC"
state_populations[53,1] <- "PUER"
state_populations <- state_populations[, -c(2:19, 21)]
names(state_populations) <- c("area", "avg_pop")


init_state_counts <- function(rank_input, class_input, df = player_database_usa) {
  
  state_counts <- df %>%
    filter(Rank <= rank_input) %>%
    filter(Class >= class_input[1] & Class <= class_input[2]) %>%
    group_by(Homestate, .drop = FALSE) %>%
    summarise(count = n(),
              .groups = "keep") %>% # to get rid of warning message
    mutate(area = Homestate, .before = count, .keep = "unused")
  
  for (state in state.abb) {
    if (!state %in% state_counts$area) {
      new_row <- c(state, 0)
      state_counts <- rbind(state_counts, new_row)
    }
  }
  
  state_counts$count <- as.numeric(state_counts$count)
  
  invisible(state_counts)
}


ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  
  titlePanel("HOOPS HOTBEDS"),
  
  sidebarPanel(
    helpText("Create graphs that reveal which US states have 
             produced the most elite high school 
             basketball players by adjusting the widgets below:"),
    sliderInput(inputId = "slider",
                label = "Select recruiting class(es):",
                sep = "",
                value = c(2003, 2021), min = 2003, max = 2021),
    numericInput(inputId = "num",
                 label = "Select top __ recruits per class (max. 50):",
                 value = 50, min = 0, max = 50, step = 5),
    selectInput(inputId = "select_data",
                label = "Select data type:",
                choices = c("totals", "per capita"),
                selected = "totals"),
    numericInput(inputId = "top",
                 label = "Select top __ states (plus DC) to show in bar graph (max. 51):",
                 value = 51, min = 1, max = 51, step = 5),
    h3("About", align = "center"),
    p("This application allows for an interactive look into which 
    US states are home to the best high school basketball players. 
      Player data comes from the ",
      tags$a(href = "https://247sports.com/Season/2021-Basketball/CompositeRecruitRankings/?InstitutionGroup=HighSchool",
             "247 Sports Composite Rankings",
             target = "_blank"),
      " which began with the class of 2003. Population data comes from ",
      tags$a(href = "https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html#par_textimage_1873399417",
             "US Census Bureua",
             target = "_blank"),
      " estimates. Per capita values are computed by averaging 
             yearly state population estimates from 2003 to now.")
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("heat map",
               girafeOutput(outputId = "map")),
      tabPanel("bar graph", 
               plotOutput(outputId = "bar",
                          height = "560px")),
      tabPanel("table",
               reactableOutput(outputId = "table"))
    )
  )
)


server <- function(input, output) {
  
  graph <- reactive({input$select_graph})
  data <- reactive({input$select_data})
  
  output$map <- renderGirafe({
    
    if (data() == "totals") {
      state_counts <- init_state_counts(input$num, input$slider)
      
      state_counts$state <- tolower(state.name[match(state_counts$area, state.abb)])
      state_counts$tooltip <- c(paste0(state_counts$area, ": ", state_counts$count))
      
      map <- map_data("state")
      
      heat_map <- state_counts %>%
        ggplot(aes(fill = count)) +
        geom_map_interactive(aes(map_id = state,
                                 tooltip = tooltip), 
                             map = map) +
        scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
        expand_limits(x = map$long, y = map$lat) +
        theme_classic() +
        labs(x = "Latitude",
             y = "Longitude",
             title = "Heat Map of United States by Total Recruits",
             subtitle = "hover over map to reveal counts") +
        theme(plot.title = element_text(size = 22,
                                        hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
      
      girafe(ggobj = heat_map,
             width_svg = 10,
             height_svg = 6,
             options = list(opts_sizing(rescale = TRUE,
                                        width = 1))
      )
      
    } else if (data() == "per capita") {
      state_counts <- init_state_counts(input$num, input$slider)
      
      per_capita <- left_join(state_counts, state_populations, by = "area")
      
      per_capita <- per_capita %>%
        mutate(percap = count * 10000000 / avg_pop)
      
      per_capita$state <- tolower(state.name[match(per_capita$area, state.abb)])
      per_capita$tooltip <- c(paste0(per_capita$area, ": ", round(per_capita$percap, 1)))
      
      map <- map_data("state")
      heat_map <- per_capita %>%
        ggplot(aes(fill = percap)) +
        geom_map_interactive(aes(map_id = state,
                                 tooltip = tooltip), 
                             map = map) +
        scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
        expand_limits(x = map$long, y = map$lat) +
        theme_classic() +
        labs(x = "Latitude",
             y = "Longitude",
             title = "Heat Map of United States by Total Recruits per 10 million state residents",
             subtitle = "hover over map to reveal counts") +
        theme(plot.title = element_text(size = 18,
                                        hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
      
      girafe(ggobj = heat_map,
             width_svg = 10,
             height_svg = 6,
             options = list(opts_sizing(rescale = TRUE,
                                        width = 1))
      )
      
    }
  })
  
  output$bar <- renderPlot({
    
    if (data() == "totals") {
      player_database_usa <- player_database_usa %>%
        filter(Rank <= input$num) %>%
        filter(Class >= input$slider[1] & Class <= input$slider[2]) %>%
        group_by(Homestate, .drop = FALSE) %>%
        summarise(count = n(), 
                  .groups = "keep") # to get rid of warning message
      
      for (state in state.abb) {
        if (!state %in% player_database_usa$Homestate) {
          new_row <- data.frame(Homestate = state, count = 0)
          player_database_usa <- rbind(player_database_usa, new_row)
        }
      }
      
      player_database_usa %>%
        arrange(-count) %>%
        head(n = input$top) %>%
        ggplot(mapping = aes(x = reorder(Homestate, -count), y = count, 
                             fill = Homestate)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = Homestate),
                  size = 3,
                  vjust = -0.25) +
        theme(axis.text.x = element_text(angle = 45)) +
        labs(x = "State",
             y = "Recruits",
             title = "Total Recruits by State") +
        theme(plot.title = element_text(size = 22,
                                        hjust = 0.5))
    } else if (data() == "per capita") {
      state_counts <- init_state_counts(input$num, input$slider)
      
      per_capita <- left_join(state_counts, state_populations, by = "area")
      
      per_capita <- per_capita %>%
        mutate(percap = count / avg_pop)
      
      per_capita %>%
        filter(!(area %in% c('SW', 'PUER', 'ON', 'NA', 'CONG'))) %>%
        ggplot(aes(x = reorder(area, -percap), y = percap * 10000000,
                   fill = area)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = area),
                  size = 3,
                  vjust = -0.25) +
        theme(axis.text.x = element_text(angle = 45)) +
        labs(x = "State",
             y = "Recruits (per 10 million state residents)",
             title = "Total Recruits Per Capita by State") +
        theme(plot.title = element_text(size = 22,
                                        hjust = 0.5))
    }
  })
  
  output$table <- renderReactable({
    
    player_database <- player_database %>%
      mutate(State = Homestate, .keep = "unused") %>%
      filter(Rank <= input$num) %>%
      filter(Class >= input$slider[1] & Class <= input$slider[2])
    
    options(reactable.theme = reactableTheme(
      color = "#000000",
      backgroundColor = "#FFFFFF"))
    
    reactable(player_database,
              defaultPageSize = 10,
              pagination = FALSE,
              filterable = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              striped = TRUE,
              columns = list(
                Class = colDef(align = "right", width = 52),
                Rank = colDef(align = "right", width = 52),
                Name = colDef(align = "left", width = 185),
                School = colDef(align = "left"),
                Hometown = colDef(align = "left", width = 178),
                State = colDef(align = "left", width = 54)
              ))
  })
  
}

shinyApp(ui = ui, server = server)

#deployApp('/Users/cal/Desktop/BasketballAnalytics/HSRecruitHotspots/')
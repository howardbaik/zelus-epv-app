library(shiny)
library(tidyverse)
# Function to draw full NBA court
source("full_court.R")

# EPV data
epv <- read_csv("data/epv.csv")

# Create vector of Possession Id (Overnight)
poss_id_overnight <- epv |>
  filter(!is.na(possID)) |>  
  pull(possID) |> 
  unique()

poss_id_overnight_names <- paste("Possession", poss_id_overnight)

# Make a list to put into choices inside selectInput
poss_id_overnight_list <- as.list(poss_id_overnight)
names(poss_id_overnight_list) <- poss_id_overnight_names


# Create vector of Possession Id (Past)
poss_id_past <- list.files(full.names = F, recursive = T, pattern = ".*.GIF") |>  
  str_extract("[0-9]+") %>% 
  as.character() %>% 
  as.numeric()

poss_id_past_df <- tibble(poss_id_past) |>  
  arrange(poss_id_past)

poss_id_past_names <- paste("Possession", poss_id_past_df$poss_id_past)

# Make a list to put into choices inside selectInput
poss_id_past_list <- as.list(poss_id_past_df$poss_id_past)
names(poss_id_past_list) <- poss_id_past_names


# CSS


# Define UI
ui <- fluidPage(
  # CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Application title
  titlePanel(tagList(
    span("NBA Expected Possession Value App",
         style = "font-family: proxima-nova;
             font-size: 35px;
             font-weight: bold;
             letter-spacing: -0.025em;"),
    span(
      actionButton("github",
                   label = "Code",
                   icon = icon("github"),
                   width = "90px",
                   onclick ="window.open(`https://github.com/howardbaik/zelus-epv-app`, '_blank')",
                   style="color: #fff; background-color: #767676; border-color: black"),
      style = "position:absolute;right:2em;"
    )
  ),
  windowTitle = "NBA EPV App"),
  # Horizontal Line to separate title
  hr(),
  tabsetPanel(
    tabPanel("Overnight Games",
             fluidRow(
               column(5,
                      br(),
                      h3("Possession ID", 
                         style = "font-size: 25px;font-family: proxima-nova;font-weight: bold;"),
                      selectInput(inputId = "poss_id_overnight",
                                  label = "",
                                  choices = poss_id_overnight_list)
               ),
               column(7,
                      br(),
                      plotOutput("overnight_epv_curve")
               )
             ),
             hr(),
             fluidRow(
               column(5,
                      br(),
                      h3("Player Formation when EPV is Highest", style = "font-size: 25px;font-family: proxima-nova;font-weight: bold;")
               ),
               column(7,
                      br(),
                      plotOutput("overnight_formation_highest")
               )
             ),
             hr(),
             fluidRow(
               column(5,
                      br(),
                      h3("Player Formation when EPV is Lowest", style = "font-size: 25px;font-family: proxima-nova;font-weight: bold;")
               ),
               column(7,
                      br(),
                      plotOutput("overnight_formation_lowest")
               )
             )
             
    ),
    tabPanel("Past Games", 
             fluidRow(
               column(5,
                      br(),
                      h4("Possession ID", style = "font-size: 25px;font-family: proxima-nova;font-weight: bold;"),
                      selectInput(inputId = "poss_id_past",
                                  label = "",
                                  choices = poss_id_past_list)
               ),
               column(7,
                      br(),
                      # Show a plot of the generated distribution
                      imageOutput("poss_anim_past")
               )
             )
    )
  )
)

# Define server
server <- function(input, output) {
  # Data filtered for Possession ID
  epv_poss <- reactive({
    epv |> filter(possID == input$poss_id_overnight)
  })
  # Data when EPV is highest
  epv_poss_highest <- reactive({
    epv_poss() |> 
      filter(epv.smooth == max(epv.smooth, na.rm = TRUE)) |> 
      filter(time == min(time, na.rm = TRUE))
  })
  # Data when EPV is lowest
  epv_poss_lowest <- reactive({
    epv_poss() |> 
      filter(epv.smooth == min(epv.smooth, na.rm = TRUE)) |> 
      filter(time == min(time, na.rm = TRUE))
  })
  
  output$overnight_epv_curve <- renderPlot({
    epv_poss() |> 
      ggplot(aes(x = 720 - game_clock, y = epv.smooth)) +
      geom_path() +
      labs(x = "Time (Seconds)",
           y = "EPV",
           title = paste("EPV for Possession ID", input$poss_id_overnight)) + 
      theme_light() +
      theme(
        text = element_text(size = 19, family = "serif"),    # Global font settings
        axis.title = element_text(size = 17),               # Axis titles font size
        axis.text = element_text(size = 16)                # Axis text font size
      ) 
  })
  
  output$overnight_formation_highest <- renderPlot({
    full_court() +
      # Home Players + Jersey Numbers
      geom_point(data = epv_poss_highest(), aes(x = h1_x, y = h1_y, group = possID), size = 9, color = "lightskyblue1") +  
      geom_text(data = epv_poss_highest(), aes(x = h1_x, y = h1_y, group = possID, label = h1_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = h2_x, y = h2_y, group = possID), size = 9, color = "lightskyblue1") +  
      geom_text(data = epv_poss_highest(), aes(x = h2_x, y = h2_y, group = possID, label = h2_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = h3_x, y = h3_y, group = possID), size = 9, color = "lightskyblue1") +  
      geom_text(data = epv_poss_highest(), aes(x = h3_x, y = h3_y, group = possID, label = h3_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = h4_x, y = h4_y, group = possID), size = 9, color = "lightskyblue1") +  
      geom_text(data = epv_poss_highest(), aes(x = h4_x, y = h4_y, group = possID, label = h4_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = h5_x, y = h5_y, group = possID), size = 9, color = "lightskyblue1") + 
      geom_text(data = epv_poss_highest(), aes(x = h5_x, y = h5_y, group = possID, label = h5_jersey_number), color = 'black') + 
      
      # Away Players
      geom_point(data = epv_poss_highest(), aes(x = a1_x, y = a1_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_highest(), aes(x = a1_x, y = a1_y, group = possID, label = a1_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = a2_x, y = a2_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_highest(), aes(x = a2_x, y = a2_y, group = possID, label = a2_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = a3_x, y = a3_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_highest(), aes(x = a3_x, y = a3_y, group = possID, label = a3_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = a4_x, y = a4_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_highest(), aes(x = a4_x, y = a4_y, group = possID, label = a4_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = a5_x, y = a5_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_highest(), aes(x = a5_x, y = a5_y, group = possID, label = a5_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_highest(), aes(x = x, y = y, group = possID), size = 6, color = "gold") +
      
      ggtitle(paste0(epv_poss_highest()$quarter_processed, " ", 
                     epv_poss_highest()$game_clock %/% 60, ":", round(epv_poss_highest()$game_clock %% 60, 0))) +
      theme(plot.title = element_text(hjust = 0.5, size = 19, family = "serif"))
  })
  
  output$overnight_formation_lowest <- renderPlot({
    full_court() +
      # Home Players + Jersey Numbers
      geom_point(data = epv_poss_lowest(), aes(x = h1_x, y = h1_y, group = possID), size = 9, color = "lightskyblue1") +  
      geom_text(data = epv_poss_lowest(), aes(x = h1_x, y = h1_y, group = possID, label = h1_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = h2_x, y = h2_y, group = possID), size = 9, color = "lightskyblue1") +  
      geom_text(data = epv_poss_lowest(), aes(x = h2_x, y = h2_y, group = possID, label = h2_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = h3_x, y = h3_y, group = possID), size = 9, color = "lightskyblue1") +  
      geom_text(data = epv_poss_lowest(), aes(x = h3_x, y = h3_y, group = possID, label = h3_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = h4_x, y = h4_y, group = possID), size = 9, color = "lightskyblue1") +  
      geom_text(data = epv_poss_lowest(), aes(x = h4_x, y = h4_y, group = possID, label = h4_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = h5_x, y = h5_y, group = possID), size = 9, color = "lightskyblue1") + 
      geom_text(data = epv_poss_lowest(), aes(x = h5_x, y = h5_y, group = possID, label = h5_jersey_number), color = 'black') + 
      
      # Away Players
      geom_point(data = epv_poss_lowest(), aes(x = a1_x, y = a1_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_lowest(), aes(x = a1_x, y = a1_y, group = possID, label = a1_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = a2_x, y = a2_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_lowest(), aes(x = a2_x, y = a2_y, group = possID, label = a2_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = a3_x, y = a3_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_lowest(), aes(x = a3_x, y = a3_y, group = possID, label = a3_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = a4_x, y = a4_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_lowest(), aes(x = a4_x, y = a4_y, group = possID, label = a4_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = a5_x, y = a5_y, group = possID), size = 9, color = "salmon1") +  
      geom_text(data = epv_poss_lowest(), aes(x = a5_x, y = a5_y, group = possID, label = a5_jersey_number), color = 'black') + 
      
      geom_point(data = epv_poss_lowest(), aes(x = x, y = y, group = possID), size = 6, color = "gold") +
      
      ggtitle(paste0(epv_poss_lowest()$quarter_processed, " ", 
                     epv_poss_lowest()$game_clock %/% 60, ":", round(epv_poss_lowest()$game_clock %% 60, 0))) +
      theme(plot.title = element_text(hjust = 0.5, size = 19, family = "serif"))
  })
  
  
  # Past Games Animation 
  output$poss_anim_past <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images',
                                        paste('anim-', input$poss_id_past, '.GIF', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

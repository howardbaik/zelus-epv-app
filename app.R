library(shiny)
library(tidyverse)

# Create vector of Possession Id
poss_id <- list.files(full.names = F, recursive = T, pattern = ".*.GIF") |>  
  str_extract("[0-9]+") %>% 
  as.character() %>% 
  as.numeric()

poss_id_df <- tibble(poss_id) |>  
  arrange(poss_id)

poss_id_modified <- paste("Possession", poss_id_df$poss_id)

# Make a list to put into choices inside selectInput
poss_id_list <- as.list(poss_id_df$poss_id)
names(poss_id_list) <- poss_id_modified



# Define UI for application
ui <- fluidPage(
  
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
                   width = "80px",
                   onclick ="window.open(`https://github.com/howardbaek/epv-app`, '_blank')",
                   style="color: #fff; background-color: #767676; border-color: #767676"),
      actionButton("homepage",
                   label = "Info",
                   icon = icon("info"),
                   width = "76px",
                   onclick ="window.open(`http://insidethetv.rbind.io/post/animating-expected-possession-value-in-the-nba/`, '_blank')",
                   style="color: #fff; background-color: #767676; border-color: #767676"),
      style = "position:absolute;right:2em;"
    )
  ),
  windowTitle = "NBA EPV App"),
  # Horizontal Line to separate title
  hr(),
  fluidRow(
    column(5,
           h4("Possession ID", style = "font-size: 25px;font-family: proxima-nova;font-weight: bold;"),
           selectInput(inputId = "poss_id",
                       label = "",
                       choices = poss_id_list),
           br(),
           
           h4("EPV Equation", style = "font-size: 25px;font-family: proxima-nova;font-weight: bold;"),
           img(src="epv-equation.png", width = 550),
           br(),
           
           h4("Resources", style = "font-size: 25px;font-family: proxima-nova;font-weight: bold;"),
           uiOutput("resource_1"),
           uiOutput("resource_2"),
           uiOutput("resource_3"),
           
           br(),
           
           h5("Built with",
              img(src = "https://rstudio.github.io/shiny/reference/figures/logo.png", height = "30px"),
              "by",
              img(src = "https://pbs.twimg.com/profile_images/1587792523757428738/IT9LDwBu_400x400.jpg", height = "28px")
           )
           
           # style='border:1px solid;'
    ),
    column(7,
           br(),
           # Show a plot of the generated distribution
           imageOutput("poss_anim")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Animation
  output$poss_anim <- renderImage({
    
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images',
                                        paste('anim-', input$poss_id, '.GIF', sep='')))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  # Resources
  paper_1 <- a("MIT Sloan Sports Analytics Conference Paper", href="http://www.sloansportsconference.com/content/pointwise-predicting-points-and-valuing-decisions-in-real-time-with-nba-optical-tracking-data/")
  paper_2 <- a("Theoretical Paper", href="https://arxiv.org/pdf/1408.0777.pdf")
  code_data <- a("Original Code + Dataset", href = "https://github.com/dcervone/EPVDemo")
  
  output$resource_1 <- renderUI({
    
    tagList(paper_1)
    
  })
  
  output$resource_2 <- renderUI({
    
    tagList(paper_2)
    
  })
  
  output$resource_3 <- renderUI({
    
    tagList(code_data)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

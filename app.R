library(shiny)
library(bslib)
library(googlesheets4)
library(dplyr)
library(glue)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  title = "Gimmi spectacle!",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
   checkboxInput(
     inputId = "if_female",
     label = "Female director?",
     value = TRUE
   ),
   checkboxInput(
     inputId = "if_english",
     label = "Is english fine?",
     value = TRUE
   ),
   checkboxInput(
     inputId = "if_musical",
     label = "Is musical fine?",
     value = FALSE
   ),
   actionButton(
     inputId = "run_random",
     label = "Gimmi!"
   )
  ),
  verbatimTextOutput(outputId = "spectacle")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  all_spectacles <- read.csv("spektakle_online.tsv",
                             header = TRUE,
                             sep = "\t") %>%
    mutate(if_musical = as.numeric(if_musical),
           if_female = as.numeric(if_female),
           if_english = as.numeric(if_english))
  
  filtered <- reactive({
    
    all_spectacles %>%
      filter(if_musical == input[["if_musical"]],
             if_female == input[["if_female"]],
             if_english == input[["if_english"]])
    
  })
  
  n_opts <- reactive({
    
    nrow(filtered())
    
    })
  
  n_random <- reactive({
    
    input[["run_random"]]
    
    set.seed(Sys.time())
    
    sample(1:n_opts(), 1)
    
    })
  
  spectacle_random <- reactive({
    
    filtered()[n_random(), ]
    
    })
  
  output[["spectacle"]] <- renderText({
    
    if(input[["run_random"]] > 0) {
      # paste0("Spectacle: ", spectacle_random()[["title"]])
      
      glue::glue_data(spectacle_random(), 
                      "Spectacle: {title}
                      Directed by: {director}
                      Duration: {length}
                      Comments: {comments}
                      Famous names: {famous.names}
                      ")
      
    } else ""
    
    
    
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)

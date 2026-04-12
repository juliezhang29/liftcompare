library(shiny)
library(data.table)
library(arrow)

source("scripts/02_match_lifters.R")

# ---------------------------
# Weight class choices for UI
# ---------------------------
get_grouped_class_choices = function(sex) {
  if (sex == "M") {
    list(
      "Traditional (USAPL)" = c(
        "52" = "52",
        "56" = "56",
        "60" = "60",
        "67.5" = "67.5",
        "75" = "75",
        "82.5" = "82.5",
        "90" = "90",
        "100" = "100",
        "110" = "110",
        "125" = "125",
        "140" = "140",
        "140+" = "140+"
      ),
      "IPF/PLA" = c(
        "59" = "59",
        "66" = "66",
        "74" = "74",
        "83" = "83",
        "93" = "93",
        "105" = "105",
        "120" = "120",
        "120+" = "120+"
      )
    )
  } else if (sex == "F") {
    list(
      "Traditional (USAPL)" = c(
        "44" = "44",
        "48" = "48",
        "52" = "52",
        "56" = "56",
        "60" = "60",
        "67.5" = "67.5",
        "75" = "75",
        "82.5" = "82.5",
        "90" = "90",
        "100" = "100",
        "100+" = "100+"
      ),
      "IPF/PLA" = c(
        "47" = "47",
        "52" = "52",
        "57" = "57",
        "63" = "63",
        "69" = "69",
        "76" = "76",
        "84" = "84",
        "84+" = "84+"
      )
    )
  } else {
    list()
  }
}

# ---------------------------
# Build OpenPowerlifting profile slug from name
# ---------------------------
make_opl_slug = function(name) {
  slug = tolower(name)
  slug = gsub("[^a-z0-9 ]", "", slug)
  slug = gsub(" ", "", slug)
  slug
}

# ---------------------------
# UI
# ---------------------------
ui = navbarPage(
  "LiftCompare",
  
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body {
        font-family: 'Poppins', sans-serif;
        font-size: 15px;
        line-height: 1.6;
      }
      h1, h2, h3, h4 {
        font-weight: 600;
      }
      .navbar {
        font-weight: 600;
      }
    "))
  ),
  
  tabPanel(
    "Match",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          numericInput("squat", "Squat (kg)", 180, min = 0),
          numericInput("bench", "Bench (kg)", 120, min = 0),
          numericInput("deadlift", "Deadlift (kg)", 220, min = 0),
          
          selectInput("sex", "Sex", choices = c("M", "F")),
          
          selectInput(
            "weight_class",
            "Weight Class",
            choices = append(list("All Classes" = ""), get_grouped_class_choices("M")),
            selected = ""
          ),
          
          selectInput(
            "fed",
            "Federation",
            choices = c("both", "IPF/PLA", "USAPL"),
            selected = "both"
          ),
          
          sliderInput("elite_n", "Elite pool size", min = 10, max = 200, value = 50),
          sliderInput("top_n", "Matches", min = 5, max = 20, value = 10),
          sliderInput(
            "max_dist",
            "How close should matches be?",
            min = 0.00,
            max = 0.15,
            value = 0.05,
            step = 0.005
          )
        ),
        
        mainPanel(
          tableOutput("results")
        )
      )
    )
  ),
  
  tabPanel(
    "About",
    fluidPage(
      br(),
      
      h3("LiftCompare helps you find elite powerlifters whose squat, bench, and deadlift proportions are similar to yours."),

      br(),
      
      h3("Inspiration"),
      p("The idea to create this app came from countless discussions with fellow powerlifters about how powerlifting is largely a game of leverages (not to say other factors don't matter), and how interesting it is that different people can be good at just different lifts. I've found that observing other lifters techniques can be so helpful for your own training, but you really need to copy/find inspiration from people who are built like you."),
      
      
      h3("What this app does"),
      p("Instead of comparing total strength alone, LiftCompare compares how your squat, bench, and deadlift are balanced relative to one another."),
      
      
      h3("How matching works"),
      p("You enter your squat, bench, and deadlift in kilograms, along with your sex and optional filters."),
      p("The app filters OpenPowerlifting data to high-quality results, computes lift proportions and compares you to elite lifters."),
      
      
      h3("What Match % means"),
      p("Match % shows how similar your lift proportions are."),
      p(strong("Higher % = more similar lifting style.")),
      
      
      h3("Notes and limitations"),
      tags$ul(
        tags$li("Compares proportions, not absolute strength."),
        tags$li("Depends on available competition data."),
        tags$li("Bodyweight-based filtering, not exact classes.")
      ),
      
      
      hr(),
      
      
      h4("Data source"),
      p("This app uses data from the OpenPowerlifting project."),
      p(HTML('<a href="https://www.openpowerlifting.org" target="_blank">openpowerlifting.org</a>')),
      p(HTML('<a href="https://github.com/openpowerlifting/openpowerlifting" target="_blank">dataset on GitHub</a>'))
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server = function(input, output, session) {
  
  observeEvent(input$sex, {
    updateSelectInput(
      session,
      "weight_class",
      choices = append(list("All Classes" = ""), get_grouped_class_choices(input$sex)),
      selected = ""
    )
  })
  
  output$results = renderTable({
    
    req(input$squat, input$bench, input$deadlift, input$sex, input$fed)
    
    df = match_lifters(
      squat = input$squat,
      bench = input$bench,
      deadlift = input$deadlift,
      sex = input$sex,
      selected_class = input$weight_class,
      federation = input$fed,
      top_n = input$top_n,
      elite_n = input$elite_n,
      max_dist = input$max_dist
    )
    
    if (nrow(df) > 0) {
      slugs = make_opl_slug(df$Name)
      
      df$Name = paste0(
        '<a href="https://www.openpowerlifting.org/u/',
        slugs,
        '" target="_blank">',
        df$Name,
        '</a>'
      )
    }
    
    df
    
  }, sanitize.text.function = function(x) x)
}

shinyApp(ui, server)
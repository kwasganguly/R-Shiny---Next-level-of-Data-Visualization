## A basic shiny app

library(shiny)

server = function(input, output, session) {} # the server

ui = basicPage("our first basic app") # the user interface

shinyApp(ui = ui, server = server) # perform app launch



## Using Input Widgets

server <- function(input,output, session) {
}

ui <-   basicPage(
  h1("Using textInput and checkboxInput"),
  textInput("mystring", "Write here"),
  checkboxInput("mycheckbox", "Factor Y")
)

shinyApp(ui = ui, server = server)



## Making the app reactive

server <- function(input, output, session) {
  observe({
    addtext <- paste("your initial input:", input$mystring)
    updateTextInput(session, "mystring2", value=addtext)
  })
}

ui <-   basicPage(
  h1("Using Observe"),
  textInput("mystring", "Write here"),
  textInput("mystring2", "Full App Output")
)

shinyApp(ui = ui, server = server)



## using reactive and render in one app

server <- function(input, output, session) {
  
  data <- reactive({
    rnorm(50) * input$myslider
  })
  
  output$plot <- renderPlot({
    plot(data(), col = "red", pch = 21, bty = "n")
  })
}

ui <- basicPage(
  h1("Using Reactive"),
  sliderInput(inputId = "myslider",
              label = "Slider1",
              value = 1, min = 1, max = 20),
  plotOutput("plot")
)

shinyApp(ui = ui, server = server)



## layouting - basic sidebar layout

server <- function(input, output, session) {}

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      "my sidebar"
    ),
    
    mainPanel(
      "my mainpanel"
    )
  )
)

shinyApp(ui = ui, server = server)



## layouting - tabsets

server <- function(input, output, session) {}

ui <- fluidPage(
  
  titlePanel("using Tabsets"), # our title
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(inputId = "s1",
                  label = "My Slider",
                  value = 1, min = 1, max = 20)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tab1", "First Tab"),
        tabPanel("Tab2", "Second Tab"),
        tabPanel("Tab3", "Third Tab")
      )
    )
  )
)

shinyApp(ui = ui, server = server)

####

names(tags)

####

## Tags

server <- function(input, output, session) {}

ui <- fluidPage(
  
  titlePanel(strong("This is the STRONG tag on the Title")), # using strong as a direct tag
  
  sidebarLayout(
    
    sidebarPanel(
      withTags(
        div(
          b("bold text: here you see a line break, a horizontal line and some code"),
          br(),
          hr(),
          code("plot(lynx)")
        ))),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Weblinks with direct tag a", a(href="www.r-tutorials.com", "r-tutorials")),
        tabPanel(tags$b("Using b for bold text"), tags$b("a bold text")),
        tabPanel("Citations with the blockquote tag", tags$blockquote("R is Great", cite = "R Programmer"))
      )
    )
  ))

shinyApp(ui = ui, server = server)





## changing the themes

server <- function(input, output, session) {}

library(shinythemes) # adding the shinythemese package

ui <- fluidPage(themeSelector(), # displaying the different themes, replace this line when publishing with theme = shinytheme("darkly")
                
                titlePanel(strong("This is the STRONG tag on the Title")), # using strong as a direct tag
                
                sidebarLayout(
                  
                  sidebarPanel(
                    withTags(
                      div(
                        b("bold text: here you see a line break, a horizontal line and some code"),
                        br(),
                        hr(),
                        code("plot(lynx)")
                      ))),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Weblinks with direct tag a", a(href="www.r-tutorials.com", "r-tutorials")),
                      tabPanel(tags$b("Using b for bold text"), tags$b("a bold text")),
                      tabPanel("Citations with the blockquote tag", tags$blockquote("R is Great", cite = "R Programmer"))
                    )
                  )
                ))

shinyApp(ui = ui, server = server)





## Advanced Shiny with R ##

## Simple datatable

library(shiny)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  output$tableDT <- DT::renderDataTable(diamonds[1:1000,],
                                        options = list(paging=F),
                                        rownames=F,
                                        filter = "top")
  
}

ui <- fluidPage(
  DT::dataTableOutput("tableDT")
)

shinyApp(ui = ui, server = server)





## Datatable styling

library(shiny)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  
  output$tableDT <- DT::renderDataTable(datatable(diamonds[1:1000,],
                                                  options = list(paging=F),
                                                  rownames=F,
                                                  filter = "top") %>%
                                          formatCurrency("price", "$") %>%
                                          formatStyle("price", color = "green") %>%
                                          formatStyle("cut",
                                                      transform = "rotateX(20deg) rotateY(5deg) rotateZ(5deg)",
                                                      backgroundColor = styleEqual(
                                                        unique(diamonds$cut), c("salmon", "lightblue",
                                                                                "grey", "lightgreen", "lightpink"))))
  
  
}

ui <- fluidPage(
  DT::dataTableOutput("tableDT")
)

shinyApp(ui = ui, server = server)





## Advanced App - brush

server <- function(input,output, session) {
  
  
  library(ggplot2) # for the diamonds dataset, and ggplot feature
  library(DT) # for the dataTableOutput
  library(shiny) # should always be activated
  
  
  output$plot <- renderPlot({
    ggplot(diamonds, aes(price, carat)) + geom_point()
  })
  
  diam <- reactive({
    
    user_brush <- input$user_brush
    sel <- brushedPoints(diamonds, user_brush)
    return(sel)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(diam()))
}

ui <-   fluidPage(
  h1("Using the brush feature to select specific observations"),
  plotOutput("plot", brush = "user_brush"),
  dataTableOutput("table")
)

shinyApp(ui = ui, server = server)

## Advanced App - click

server <- function(input,output, session) {
  
  library(ggplot2) # for the diamonds dataset, and ggplot feature
  library(DT) # for the dataTableOutput
  
  output$plot <- renderPlot({
    ggplot(diamonds, aes(price, carat)) + geom_point()
  })
  
  diam <- reactive({
    
    user_click <- input$user_click
    sel <- nearPoints(diamonds, user_click, threshold = 10, maxpoints = 5)
    # maxpoints gives the maximum number of observations in the table
    # threshold gives the maximum distance in the dataset
    return(sel)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(diam()))
}

ui <-   fluidPage(
  h1("Using the click feature to select specific observations"),
  plotOutput("plot", click = "user_click"),
  dataTableOutput("table")
)

shinyApp(ui = ui, server = server)



## Advanced Plot with csv export

server <- function(input,output, session) {
  
  library(ggplot2) # for the diamonds dataset, and ggplot feature
  library(DT) # for the dataTableOutput
  
  output$plot <- renderPlot({
    
    ggplot(diamonds, aes(price, carat)) + geom_point()
    
  })
  
  diam <- reactive({
    
    user_brush <- input$user_brush
    sel <- brushedPoints(diamonds, user_brush)
    return(sel)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(diam()))
  
  output$mydownload <- downloadHandler(
    filename = "plotextract.csv",
    content = function(file) {
      write.csv(diam(), file)})
}

ui <- fluidPage(
  h3("Exporting Data as .csv"),
  plotOutput("plot", brush = "user_brush"),
  dataTableOutput("table"),
  downloadButton(outputId = "mydownload", label = "Download Table")
)

shinyApp(ui = ui, server = server)



## Media Integration

library(shiny)

server = function(input,output) {
}

ui = navbarPage("Integration of different media types",
                
                tabPanel("Image sourced locally",
                         tags$img(src = "logo.png", width = "100px", height = "100px")),
                
                tabPanel("Video sourced locally",
                         tags$video(src = "comist.mp4", type = "video/mp4", controls = T,
                                    width = "900px", height = "800px")),
                
                tabPanel("Pdf sourced online, Iframe",
                         tags$iframe(style="height:600px; width:100%; scrolling=yes",
                                     src="https://cran.r-project.org/web/packages/shiny/shiny.pdf")),
                
                tabPanel("Text as .txt",
                         includeText("mytxt.txt"))
)

shinyApp(ui = ui, server = server)


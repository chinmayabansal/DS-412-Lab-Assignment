

library(plotly)
library(shinyforms)

library(shiny)

library(metricsgraphics)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .mg-histogram .mg-bar rect {
          fill: #75AADB;
          shape-rendering: auto;
      }
      .mg-histogram .mg-bar  {
          fill: #75AADB;
          shape-rendering: auto;
      }


      .mg-histogram .mg-bar rect.active {
          fill: #ffa500;
      }"))),
   # Application title
   #titlePanel("Histogram"),
   #selectInput("typee","Select the type of distribution",c("Normal Distribution"="normm","Uniform Distribution"="unii","Exponential Distribution"="expp")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(   selectInput("typee","Select the type of distribution",c("Uniform Distribution"="unii","Normal Distribution"="normm","Exponential Distribution"="expp"),selected = "Unifrom Distribution"),
                      selectInput("sele","Replacement",c("Yes"="yess","No"="noo")),
                      
         sliderInput("numb",
                     "Number of Samples:",
                     min = 20,
                     max = 100,
                     value = 20),
         sliderInput("unimin",
                     "Enter the Minimum Value for for Uniform Distibution: ",
                     min = 1,
                     max = 99,
                     value = 1),
         sliderInput("unimax",
                     "Enter the Maximum Value for Uniform Distibution: ",
                     min = 2,
                     max = 100,
                     value = 100),
        sliderInput("meann",
                    "Enter the Mean for Normal Distibution: ",
                    min = 0,
                    max = 50,
                    value = 10),
      sliderInput("stddev",
                  "Enter the Standard Deviation for Normal Distibution: ",
                  min = 0,
                  max = 50,
                  value = 10),
      
      sliderInput("exprate",
                  "Enter the Rate for Exponential Distibution: ",
                  min = 1,
                  max = 100,
                  value = 1)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         #plotOutput("distPlot",hover=hoverOpts(id="plot_hover")),    includeHTML('script.js')
        metricsgraphicsOutput('distPlot',width = "100%",height = "500px")
        
        )
   )
   #verbatimTextOutput("hover_info")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  d <- reactive({
    
    sele <- switch(input$sele,
                   noo = TRUE,
                   yess = FALSE,
                   TRUE)
    dist<-switch(input$typee,
                 unii=sample(input$unimin:input$unimax,input$numb,replace = sele),
                 normm=rnorm(input$numb,mean=input$meann,sd=input$stddev),
                 expp=rexp(input$numb,input$exprate),
                 sample(input$unimin:input$unimax,input$numb,replace = sele)
                 )

  })
   
   output$distPlot <- renderMetricsgraphics({
     dist <- input$dist
     n <- input$n
     mjs_plot(d(), format="count") %>% 
       mjs_histogram()
   })
   output$hover_info <- renderPrint({
     cat("input$plot_hover:\n")
     str(input$plot_hover)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


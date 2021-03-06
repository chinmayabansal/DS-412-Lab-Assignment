

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
      plotOutput("plot",brush = brushOpts(id = "plot_brush"),hover = hoverOpts(id = "plot_hover"))
      
      
      # metricsgraphicsOutput('distPlot',width = "100%",height = "400px"),
      #plotOutput("plot2",hover = hoverOpts(id = "plot_hover"))
      
      
      
    )
  )
  , verbatimTextOutput("hover_info")
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  d <- reactive({
    
    sele <- switch(input$sele,
                   noo = TRUE,
                   yess = FALSE,
                   TRUE)
    disttt<-switch(input$typee,
                   unii=sample(input$unimin:input$unimax,input$numb,replace = sele),
                   normm=rnorm(input$numb,mean=input$meann,sd=input$stddev),
                   expp=rexp(input$numb,input$exprate),
                   sample(input$unimin:input$unimax,input$numb,replace = sele)
    )
    #colorrrr<-switch(input$plot_brush$xmin,
    
    #)
    
    
  })
  faa<- reactive({
    disttt<-switch(input$typee,
                   unii="Uniform Distribution of ",
                   normm="Normal Distribution of ",
                   expp="Exponential Distribution of ",
                   "Uniform Distribution of "
    )
  })
  dkk<-reactive({
    # abcc=(as.integer((input$plot_hover$x-  min(d()))*10  / (max(d())-min(d()) ))+1)
    #print(abcc)
    if( is.null(input$plot_brush$xmax) && is.null(input$plot_hover$x)) 
      color="blue"
    else if(!is.null(input$plot_hover$x))
     {
      color=dkkb2()
      }
    else if( !is.null(input$plot_brush$xmax) && is.null(input$plot_hover$x)) 
          {
      color=dkkb()
          }

    else color=dkkb()
    #observeEvent(input$plot_hover, color=dkkb2())
      
  })
  
  dkkb<-reactive({
    flag=1
    i=1
    differe =((max(d())-min(d()))/10)
    check=min(d())
    while(i<11)
    {  
      if (check>(input$plot_brush$xmax))
      {
        flag=2
      }
      if(((input$plot_brush$xmin-differe)<check) && (flag==1))
      {
        color[[i]]<-"orange"
      }  
      else{
        color[[i]]<-"blue"
      }
      i=i+1
      check=check+differe
    }
    check=min(d())
    
    
    return(color)
  })
  dkk2<-reactive({
    # abcc=(as.integer((input$plot_hover$x-  min(d()))*10  / (max(d())-min(d()) ))+1)
    #print(abcc)
    if( is.null(input$plot_hover$x) ) 
    {
      color="blue"
    }
    else
      color=dkkb2()
    
  })
  dkkb2<-reactive({
    #color=c("blue","blue","blue","blue","blue","blue","blue","blue","blue","blue")
    #print("HIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
    abcc=(as.integer((input$plot_hover$x-  min(d()))*10  / (max(d())-min(d()) ))+1)
    color[[abcc]]="orange"
    print(color)
    })
  output$distPlot <- renderMetricsgraphics({
    dist <- input$dist
    n <- input$n
    mjs_plot(d(), format="count") %>% 
      mjs_histogram()
  })
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$numb
    minv=min(d())
    maxv=max(d())
    hist(d(),breaks=seq(minv,maxv,l=11),main = paste(faa(),n, " Random Variables", sep = ""),col = dkk(), border = "white")
    
    #   hist(d(),breaks=tb,main = paste("r", dist, "(", n, ")", sep = ""),col = "blue", border = "white")
    #qplot(d(), geom="histogram", fill=I("lightblue"), col=I("red"))
  })
  output$plot2 <- renderPlot({
    dist <- input$dist
    n <- input$numb
    minv=min(d())
    maxv=max(d())
    hist(d(),breaks=seq(minv,maxv,l=11),main = paste(faa(),n, " Random Variables", sep = ""),col = dkk2(), border = "white")
    
    #   hist(d(),breaks=tb,main = paste("r", dist, "(", n, ")", sep = ""),col = "blue", border = "white")
    #qplot(d(), geom="histogram", fill=I("lightblue"), col=I("red"))
  })
  
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


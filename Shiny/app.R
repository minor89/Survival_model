library(shiny)
library (ggplot2)



source("./sawflymodel.R")
# Define UI for slider demo app ----
ui <- fluidPage(title = "Parameter investigation",
  
  # App title ----
  titlePanel(
    fluidRow(
      column(9, div(HTML("Parameter investigation")))
    )
  ),

  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Btrees
      sliderInput("Btrees", "Btrees",
                  min = 0.0, max = 1,
                  value = c(0.1, 0.2), step=0.01),
      
      textInput("Btrees.step", "Btrees Step",
                value=0.01),
      
      # Input: DC
      sliderInput("DC", "DC",
                  min = 0.0, max = 0.5,
                  value = c(0.01, 0.03), step=0.01),
      
      textInput("DC.step", "DC Step",
                value=0.01),
      
      # Input: Confidence levels
      sliderInput("SlarvaeC", "SlarvaeC",
                  min = 0.45, max = 0.6,
                  value = c(0.55, 0.58), step=0.01),
      
      textInput("SlarvaeC.step", "SlarvaeC Step",
                value=0.01)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
    
      # Output: Table summarizing the values entered ----
      tableOutput("values"),
      actionButton("run_model", "Run Simulation", icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      plotOutput("plot1"),
      
      strong("Notes"), p("default values")
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {

 # paste(poissonLarge(eff=input$efficacy, loD=input$detection, conf=input$conf))
  
#  table1 <- reactive({
#    hypergeomObjSolve
#  })
  
  
  table4 <- reactive({
    poissonLarge(eff=input$efficacy, loD=input$detection, conf=as.numeric(input$conf_select)*100)
  })

  
  det_eff<- reactive({
    input$detection * (input$efficacy/10) * as.numeric(input$conf_select)
  })
  
  sawfly_mod <- eventReactive(input$run_model, {
    
    Btrees.dat <- seq(from=as.numeric(input$Btrees[1]), to=as.numeric(input$Btrees[2]), by=as.numeric(input$Btrees.step))
    DC.dat <- seq(from=input$DC[1], to=input$DC[2], by=as.numeric(input$DC.step))
    SlarvaeC.dat <- seq(from=input$SlarvaeC[1], to=input$SlarvaeC[2], by=as.numeric(input$SlarvaeC.step))
    
    param.args <- expand.grid(Btrees = Btrees.dat, DC=DC.dat, SlarvaeC=SlarvaeC.dat)
    
    # using apply, iterate across every row and pass the row values as the arguments to the tmii.func
    output.list <- apply(param.args,1,function(params)sawfly.model(Btrees=params[1],DC=params[2],SlarvaeC=params[3]))
    
    param.args$run <- paste0("run_", seq_along(param.args[,1])) 
    #output.df <-data.frame((matrix(ncol = length(output.list), nrow = years)))
    
    output.df <- data.frame("Step"=seq(from=1, to=years, by=1))
    for (i in 1:length(output.list)){
      output.df[i+1] <- output.list[[i]]$population
      colnames(output.df)[i+1] <- paste0("run_",i)
    }
    
    
    #Tag values > 200 in the output df
    #output.df$value<-ifelse(output.df$run_1>200,"Yes","No")
    #But it only needs to be one value in the run. 
    #Should rather be added to a df or list of the parameter combinations 
    
    
    output.df <- reshape2::melt (output.df, id="Step")
    
    output.df$threshold <- 0
    output.df$threshold[output.df$value >= 200] <- 1
    
    ## This step makes all the runs that have non-0 values as "1" for the whole run - good for plotting later
    output.df$threshold[output.df$variable %in% unique(output.df$variable[output.df$threshold != 0])] <- 1
    
    output.df
  
  })
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Parameter = c("Btrees min",
                    "Btrees max",
                    "DC min",
                    "DC max",
                    "SlarvaeC min",
                    "SlarvaeC max"),
      Value = paste(as.character(c(input$Btrees[1],
                                   input$Btrees[2],
                             input$DC,
                             input$SlarvaeC))),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  
  output$plot1 <- renderPlot({
    ggplot (sawfly_mod(), aes(Step, value,fill=variable))+
      geom_line(alpha=0.2, aes(colour=as.factor(threshold)))+
      coord_cartesian(ylim=c(0,500))+
      geom_hline(yintercept=200,linetype=2,colour="lightblue",size=1.2)+
      theme_dark()+
      theme(legend.position = "none")
  })
  
  output$plot2 <- renderPlot({
  plot_ly(sawfly_mod(), x = ~Btrees, y = ~DC, z = ~SlarvaeC, color = ~threshold, colors = c('#BF382A', '#0C4B8E')) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = '% browsed trees'),
                        yaxis = list(title = 'Direct consumtion (%)'),
                        zaxis = list(title = 'Baseline larval survival (control)')))
  })
  

  
}

# Create Shiny app ----
shinyApp(ui, server)




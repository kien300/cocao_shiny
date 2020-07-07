if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, qdap, shiny, scales, data.table, 
               waffle, ggExtra)

#load dataset from Dropbox----
main <- read.csv("https://www.dropbox.com/s/o4gjbhn6dtg928k/main_v2.csv?raw=1")

#Tidying dataset----
#Visuals Setting
theme_set(theme_bw())
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", 
                  "#DDCC77", "#CC6677", "#882255", "#AA4499")

#tidy datasets
#collect Var Names that fulfill data type conditions
ctgr <- main %>% select_if(is.factor) %>% 
  names()

cont <- main %>% select_if(is.numeric) %>% 
  names()

#define UI----
ui <- fluidPage(
  titlePanel("Coconut Survey data checks"),
  
  navbarPage("", selected = 'Key Variables',
             tabPanel("Key Variables",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Select desired variables"),
                          
                          selectInput('xcol', 'X Variable', names(main)),
                          selectInput('ycol', 'Y Variable', names(main),
                                      selected = names(main)[[3]]),
                          selectInput('color', 'Color', c('None', names(main))),
                          
                          checkboxInput('jitter', 'Jitter'),
                          checkboxInput('smooth', 'Smooth'),
                          checkboxInput('flip', 'Flip'),
                          
                          selectInput('facet_row', 'Facet Row', c(None='.', names(main))),
                          selectInput('facet_col', 'Facet Column', c(None='.', names(main)))
                        ),
                        
                        mainPanel(
                          plotOutput('scatter')
                          
                        ))),
             
             tabPanel("Enumerators",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput('xcol2', 'Enumerator', names(main))
                          
                        ),
                        
                        mainPanel(
                          plotOutput('bar')
                        ))
             )
  )
)

# Define server function ----
server <- function(input, output){
  
  #Reactivity
  selectedData <- reactive({main})
  
  output$scatter <- renderPlot({
    if (input$xcol %in% ctgr & input$ycol %in% cont)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol,y=input$ycol)) + 
        geom_boxplot()
    else if (input$xcol %in% cont & input$ycol %in% ctgr)
      g3 <- ggplot(selectedData(), aes_string(x=input$ycol,y=input$xcol)) + 
        geom_boxplot() + coord_flip()
    else if (input$xcol %in% ctgr & input$ycol %in% ctgr)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol)) + 
        geom_bar(width = .5, fill='#CC79A7') +
        geom_text(stat='count',aes(label=..count..),vjust=-0.6) +
        labs(x='Values')
    else if (input$xcol %in% cont & input$ycol %in% cont & input$xcol==input$ycol)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol)) + 
        geom_density()
    else
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol,y=input$ycol)) + 
        geom_point()
    
    # some theme elements
    g3 <- g3 + theme(axis.title = element_text(size = 15, face = "bold"))
    
    if (input$color != 'None')
      g3 <- g3 + aes_string(color=input$color)
    
    facets1 <- paste(input$facet_row, '~', input$facet_col)
    if (facets1 != '. ~ .')
      g3 <- g3 + facet_grid(facets1)
    
    if (input$flip)
      g3 <- g3 + coord_flip()
    
    if (input$jitter)
      g3 <- g3 + geom_jitter()
    
    if (input$smooth)
      g3 <- g3 + geom_smooth(method = 'loess')
    
    print(g3)
  }, height = 600)
  
  # output$bar <- renderPlot({
  #   g4 <- ggplot(livestock, aes(HHID,number,fill=livestock)) +
  #     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +   #no tick marks
  #     labs(x = 'Households', y = 'Percentage') + 
  #     geom_bar(position = 'fill', stat = 'identity') +
  #     scale_y_continuous(labels = percent_format()) +
  #     scale_fill_manual(values=tol9qualitative)
  #   
  #   facets2 <- paste(input$facet_row2, '~', input$facet_col2)
  #   if (facets2 != '. ~ .')
  #     g4 <- g4 + facet_grid(facets2, space="free_x", scales="free_x")
  #   
  #   print(g4)
  # }, height = 600)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

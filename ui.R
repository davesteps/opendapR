
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyUI(bootstrapPage(useShinyjs(),
                      
                      # Hidden input boxes to save the variable to 
                      HTML(' <input type="text" id="username" name="username" style="display: none;"> '), 
                      includeScript("www/getusername.js"), 
                      # include the js code 
                      
                      
                      
                      # Add custom CSS & Javascript;
                      tagList(
                        tags$head(
                          tags$link(rel="stylesheet", type="text/css",href="style.css"),
                          tags$script(type="text/javascript", src = "busy.js"),
                          tags$script(src = "message-handler.js")
                        )
                      ),dashboardPage(
  
  # Application title
  dashboardHeader(title = "opendapR"),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inputs", tabName = "inputs"),
      menuItem("Spatial Output", tabName = "spatial"),
      menuItem("Temporal Output", tabName = "temporal"))
    
    
    #uiOutput('queryUI')
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(
    #h6('Longnames'),
    #verbatimTextOutput('longNames'),
    tabItems(
      tabItem('inputs',
              fluidRow(

                
                box(width = 3,#height=700,
                    
                    selectInput('src',label = 'Source',choices = c('',src$name),selected = ''),
                    uiOutput('var'),
                    sliderInput('xr',label = 'longitude range',min = -20,max=20,value = c(1,3),step = 0.1),
                    sliderInput('yr',label = 'latitude range',min = 40,max=60,value = c(52,54),step=0.1),
#                     flowLayout(
#                       
#                       textInput('xmin',label = 'xmin',value = 1),
#                       textInput('xmax',label = 'xmax',value = 3)),
#                     flowLayout(
#                       textInput('ymin',label = 'ymin',value = 52),
#                       textInput('ymax',label = 'ymax',value = 54)
#                     ),
                    flowLayout(
                      dateRangeInput('dateR',label = 'Date Range',start = Sys.Date()-(365*15),end = Sys.Date(),min = '1980-01-01',max = Sys.Date()),

                      uiOutput('vertUI')),
                    actionButton('run',label = 'Run')
                    
                ),
                box(width = 9,#height=500,
                    leafletOutput("map",height = 650)
                )
                ),
              fluidRow(
                
                valueBox(width=3,textOutput('query'),subtitle = 'values'),
                #box(
                #    ),
                box(width=3,
                    verbatimTextOutput('varSummary')),
                box(width=6,
                    verbatimTextOutput('queryout'),
                    downloadButton(outputId = 'downloadData',label = 'Download')
                )
              )),
      tabItem('spatial',

              box(width=3,
                flowLayout(
                
                selectInput('spstat','Statistic',
                            choices = c('mean','median','min','max',
                                        'p5','p95','sum','sd','n')),
                
                selectInput('spfacet','Facet',
                            choices = c('None','Month','Year','Month+Year')),
                
                  checkboxInput(inputId = 'splog',label = 'log10 scale')
                )
                
              ),
              box(width=9,plotOutput('SPplot',height = 700))
              
              ),
      tabItem('temporal',
              box(plotOutput('TSplot')),
              box(
                flowLayout(
                  checkboxInput(inputId = 'tslog',label = 'log10 scale'),
                  checkboxInput(inputId = 'tslm',label = ' linear fit')
                  )
                
                )
              #tableOutput('dims'),
      )
    )
    
  ))
  ,
  div(class = "busy", 
      h4("working..."),
      h2(HTML('<i class="fa fa-cog fa-spin"></i>')),
            singleton(
              tags$head(tags$script(src = "message-handler.js"))
            )
  )
  
  
  
))

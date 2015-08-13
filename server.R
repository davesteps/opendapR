
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
require(ggplot2)
require(reshape2)
library(shiny)
library(ncdf)
require(plyr)
require(dplyr)
require(lubridate)
require(raster)


shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    print('create map')
    # print(isolate(input$baseMap))
    leaflet() %>% #addTiles() %>% 
      #basemaps
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter (CartoDB)") %>%
      #overlays
      #layer control
      addLayersControl(baseGroups = c("OpenStreetmap", 'Esri.WorldImagery',"Esri.OceanBasemap",'DarkMatter (CartoDB)'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>% 
      setView(lng = 1.75,lat = 52.48, zoom = 10)
    
    
  })
  
  
  nc <- reactive({
    if(input$src==''){
      return(NULL)
    }
    open.ncdf(src$opendap[which(src$name==input$src)])
    
  })
  
  nc.longnames <- reactive(longnames(nc()))
  
  output$var <- renderUI({
    if(input$src==''){
      return(NULL)
    }
    
    selectInput('var','Choose Parameter:',nc.longnames())})
  
  var <- reactive(nc()$var[[input$var]])
  
  var.dims <- reactive(var()%>%vardims())
  var.ndims <- reactive(length(var.dims()))
  var.hasTime <- reactive(var()%>%hasTime())
  var.hasVert<-reactive(var()%>%hasVert())
  var.vid <- reactive(ifelse(var.hasVert(),var()%>%vertid(),NA))
  xdim <- reactive(var.dims()[1])
  ydim <- reactive(var.dims()[2])
  var.dl <- reactive(datelist(var()))
  #tdim <- reactive(ifelse(var.hasTime(),tail(var.dims(),1),NA))
  #vdim <- reactive(ifelse(var.hasVert(),var.dims()[[var.vid()]],NA))
  
  output$longNames <- renderPrint(nc.longnames())
  
  
  vdim <- reactive({
    if(is.null(var()))
      return(NULL)
    
    if(!var.hasVert())
      return(NULL)
    
    var()$dim[[var.vid()]]
    
  })
  tdim <- reactive({
    if(is.null(var()))
      return(NULL)
    
    if(!var.hasTime())
      return(NULL)
    
    var()$dim[[var.ndims()]]
    
  })
  
  
  observe({
    input$var
    
    leafletProxy("map") %>% clearShapes()
    
    leafletProxy("map") %>%
      addRectangles(lat1=input$yr[1],layerId = '1',
                    lng1=input$xr[1],
                    lat2=input$yr[2],
                    lng2=input$xr[2],
                    options=list(color="#ff7800",fill=F, fillOpacity=0.7, weight= 1,
                                 stroke=TRUE, opacity=1)
      ) %>%
      fitBounds(input$xr[1],input$yr[1],input$xr[2],input$yr[2])
  })
  
  
  
  
  output$varSummary <- renderPrint({
    
    if(is.null(var()))
      return(NULL)
    
    print(paste('Name:',var()$longname))
    print(paste('Units',var()$units))
    #print('Dims:')
    #print(var.dims())
    print('Dim Size:')
    print(var()$varsize)
    print(paste('xdim:',xdim()))
    print(paste('ydim:',ydim()))
    print(paste('tdim:',tdim()$name))
    print(paste('vdim:',vdim()$name))
    
  })
  
  #   dims <- reactive({
  #     if(is.null(var()))
  #       return(NULL)
  #     
  #     ldply(vardims(var()),function(x) 
  #       cbind.data.frame(dim=x,units=dimunits(var(),x),res=dimres(var(),x),
  #                        min=dimmin(var(),x),max=dimmax(var(),x)))
  #     
  #   })
  #   
  #   output$dims <- renderTable(dims())
  #   
  #   dimSlider <- function(dim){
  #     sliderInput(inputId = dim$name,label = dim$name,min = min(dim$vals),
  #                 max = max(dim$vals),value = min(dim$vals,step = res(dim$vals))
  #   }
  # 
  # 
  output$vertUI <- renderUI({
    
    if(is.null(vdim()))
      return(NULL)
    
    vals <- as.numeric(vdim()$vals)
    
    sliderInput(inputId = 'vert',label = vdim()$name,min = min(vals),
                max = max(vals),value = c(min(vals),min(vals)),step = resolution(vals))
    
  })
  
  
  
  output$query <- renderText( prod(query()$count))
  
  
  query <- reactive({
    buildQuery(var = var(),xr = input$xr,yr = input$yr,vr = input$vert,tr = input$dateR)})
  
  
  df<-NULL
  makeReactiveBinding('df')
  
  observeEvent(input$run,{
    
    isolate({
      qry <- query() 
      if(prod(qry$count)>2e6){
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Query too large - reduce at least one of the dimensions')
        
        return(NULL)
      }
      
      toggleState("run")
      df<<-getQuery(nc(),input$var,qry)
      
      str(df)
#       
#       r <- dm %>%
#         group_by(y,x) %>%
#         summarize(value=mean(value)) %>%
#         rasterFromXYZ()
#       
#       pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
#                           na.color = "transparent")
#       
#       leafletProxy("map") %>% 
#                 addRasterImage(r, colors = pal, opacity = 0.8,layerId = '2') #%>%
# #                 addLegend(pal = pal, values = values(r),
# #                           title = "value")
# #       
      
    })
    toggleState("run")
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('output', '.csv', sep='') },
    content = function(file) {
      write.csv(df, file)
    }
  )
  
  
  output$queryout <- renderPrint(str(df))
  
  
  
  
  
  output$TSplot <- renderPlot({
    
    
#     t=df%>%group_by(time)%>%
#       summarise(value=mean(value,na.rm=T))
    
    p=ggplot(df,aes(x=as.Date(time),y=value))+
      geom_line()
    
    if(input$tslog){p=p+scale_y_log10()}
    if(input$tslm){p=p+stat_smooth(method='lm')}
    
    
    print(p)
    
    
  })
  
  spstat <- function(x, type) {
    switch(type,
           mean = mean(x),
           median = median(x),
           min = min(x),
           max = max(x),
           p5 = quantile(x,0.05),
           p95 = quantile(x,0.95),
           sum = sum(x),
           sd = sd(x),
           n=sum(!is.na(x))
    )
  }
  spgroup <- function(df, type) {
    switch(type,
           None= group_by(df,y,x),
           Year= group_by(df,y,x,year),
           Month= group_by(df,y,x,month),
           'Month+Year'=group_by(df,y,x,month,year)
    )
  }
  spfacet <- function(p, type) {
    switch(type,
           None= p,
           Year= p+facet_wrap(~year),
           Month= p+facet_wrap(~month),
           'Month+Year'=p+facet_grid(month~year)
    )
  }
  
  output$SPplot <- renderPlot({ 
    
    t=df%>%
      spgroup(input$spfacet)%>%
      summarise(value=spstat(value,input$spstat))
    
    p=ggplot(t)+coord_quickmap()
    
    if(input$splog){
      p=p+geom_raster(aes(x=x,y=y,fill=log10(value)))
    } else {
      p=p+geom_raster(aes(x=x,y=y,fill=value))}
    
    p=p%>%spfacet(type=input$spfacet)
    
    p=p+scale_fill_gradientn(colours = rainbow(7))    
    
    print(p)
    
  })                            
  
})

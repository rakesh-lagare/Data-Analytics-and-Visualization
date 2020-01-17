library(grid)
library(shiny)
library(tidyr)
library(pacman)
library(plotly)
library(scales)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(rworldmap)

p_load(tidyverse,stringr,data.table)

#***************** Loading adn refactoring the original data *****************#

#air_data_orig <- read.table(file = 'E:/Masters/Semester_3/LAB/Project/Dataset/EuroStat/rail.tsv', sep = '\t', header = TRUE)
inland_data_orig <- read.table(file = 'E:/Dataviz-20171116T170106Z-001/Dataviz/inland.tsv', sep = '\t', header = TRUE)
rail_data_orig <- read.table(file = 'E:/Dataviz-20171116T170106Z-001/Dataviz/rail.tsv', sep = '\t', header = TRUE)
road_data_orig <- read.table(file = 'E:/Dataviz-20171116T170106Z-001/Dataviz/road.tsv', sep = '\t', header = TRUE)
sea_data_orig <- read.table(file = 'E:/Dataviz-20171116T170106Z-001/Dataviz/sea.tsv', sep = '\t', header = TRUE)

# #air_restructured_data <- gather(data, yr, cnt, X2005:X2016) %>% filter(!grepl("^TOTAL.*THS.*$", tra_cov.unit.geo.time))
inland_restructured_data <- gather(inland_data_orig, yr, cnt, X2005:X2016) %>%
    filter(!grepl("^.*MIO_TKM.*$", unit.vessel.tra_cov.geo.time)) %>%
    mutate(land = str_replace_all(unit.vessel.tra_cov.geo.time,"^THS_T,TOTAL,TOTAL,",""))%>%
    select(land, yr, cnt)

rail_restructured_data <- gather(rail_data_orig, yr, cnt, X2004:X2015) %>%
    filter(!grepl("^.*MIO_TKM.*$", tra_cov.unit.geo.time)) %>%
    mutate(land = str_replace_all(tra_cov.unit.geo.time,"^.*THS_T,","")) %>%
    select(land, yr, cnt)

road_restructured_data <- gather(road_data_orig, yr, cnt, X2005:X2016) %>%
    filter(!grepl("^.*MIO_TKM.*$", carriage.tra_oper.unit.geo.time)) %>%
    mutate(land = str_replace_all(carriage.tra_oper.unit.geo.time,"^.*THS_T,","")) %>%
    select(land, yr, cnt)

sea_restructured_data <- gather(sea_data_orig, yr, cnt, X2005:X2016) %>%
    filter(!grepl("^.*MIO_TKM.*$", direct.unit.geo.time)) %>%
    mutate(land = str_replace_all(direct.unit.geo.time,"^.*THS_T,","")) %>%
    select(land, yr, cnt)

inland_restructured_data$dflag <- "Inland"
rail_restructured_data$dflag <- "Rail"
road_restructured_data$dflag <- "Road"
sea_restructured_data$dflag <- "Sea"

#*****************************************************************************#

restructured_data <- do.call("rbind", list(inland_restructured_data, rail_restructured_data, road_restructured_data,sea_restructured_data))

regions <- select(restructured_data,land) %>% distinct() %>% filter(land != "EU27" & land !="EU28")

years <- as.numeric(str_replace_all(restructured_data$yr,"X","")) %>% sort()


ui <- dashboardPage(
    #skin = "red",
    dashboardHeader(title = "Transportation of goods",
                    titleWidth = 250
    ),
    
    dashboardSidebar(
        width = 250,
        sidebarMenu(id="tabs",
                    sidebarMenuOutput("menu")
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "t1",
                    
                    fluidRow(
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Dashboard",
                            status = "danger",
                            width = 9,
                            
                            box(
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Inputs",
                                status = "info", 
                                width = 4,
                                selectInput("dtype", "Dataset", choices = c("Rail","Road","Inland","Sea"), selected = "Rail",multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                selectInput("yor", "Select the Year", years, selected = years[0],multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                radioButtons("type","Type of Graph", choices = c("Bar chart"=1,"Horizontal bar chart"=2, "Pie chart"=3,"Map"=4), selected = 1),
                                checkboxInput("top5", "Show top 5", TRUE)
                            ),
                            
                            box(
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 4,
                                title = "Performance",
                                status = "success",
                                
                                fluidRow(
                                    infoBoxOutput("top", width = 12)
                                ),
                                fluidRow(
                                    column(1,offset = 5,
                                           icon("option-horizontal", lib = "glyphicon")
                                    ) 
                                ),
                                fluidRow(
                                    infoBoxOutput("last", width = 12)
                                )
                            ),
                            
                            conditionalPanel(
                                "input.top5",
                                box(
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    title = "Top five regions",
                                    status = "warning", 
                                    width = 4,
                                    tableOutput("mostExpensive"),
                                    footer = "Quantity: in thousand tonnes"
                                )
                            )),
                        
                        
                        #fluidRow(plotOutput("map")
                        conditionalPanel(
                            "input.type== 4", 
                            box(
                                width = 9,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Map",
                                status = "danger",
                                plotOutput("map"),
                                footer = "Source: Eurostats"
                            )
                        ),
                        
                        conditionalPanel(
                            "input.type== 2", 
                            box(
                                width = 9,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Horizontal bar plot",
                                status = "danger",
                                plotOutput("hBarPlot"),
                                footer = "Source: Eurostats"
                            )
                        ),
                        
                        conditionalPanel(
                            "input.type== 1",
                            box(width = 9,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Vertical bar plot",
                                status = "danger",
                                plotOutput("distPlot"),
                                footer = "Source: Eurostats"
                            )
                        ),
                        
                        conditionalPanel(
                            "input.type== 3",
                            box(width = 9,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Pie chart",
                                status = "danger",
                                plotlyOutput("piePlot"),
                                footer = "Source: Eurostats"
                            )
                        ) )
                    # )
            ),
            
            tabItem(tabName = "t2",
                    fluidRow(
                        box(
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Dashboard",
                            status = "danger",
                            width = 12,
                            footer = "Source: Eurostats, Quantity: in thousand tonnes",
                            box(
                                title = "Inputs",
                                status = "info", 
                                width = 4,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                selectInput("rdtype", "Dataset", choices = c("Rail","Road","Inland","Sea"), selected = "Rail",multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                                selectInput("land", "Select the Region:", choices=regions, selected = 'AT'),
                                radioButtons("rtype","Type of Graph", choices = c("Bar chart"=4,"Horizontal bar chart"=5, "Pie chart"=6, "Overall Analysis"=7), selected = 4)
                                
                            ),
                            
                            
                            box(
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 4,
                                title = "Performance",
                                status = "success",
                                #footer = "TKM: Million tonne-kilometre",
                                
                                fluidRow(
                                    infoBoxOutput("highest", width = 12)
                                ),
                                
                                fluidRow(
                                    infoBoxOutput("lowest", width = 12)
                                )
                            ),
                            conditionalPanel(
                                "input.rtype == 7",
                                
                                box(
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = 4,
                                    title = "Overall Performance",
                                    status = "warning",
                                    #footer = "TKM: Million tonne-kilometre",
                                    tableOutput("overall")
                                ))
                        ),
                        
                        
                        #fluidRow(
                        
                        
                        conditionalPanel(
                            "input.rtype == 7",
                            box(width = 9,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Stacked bar chart",
                                status = "danger",
                                plotlyOutput("rStackedPlot"),
                                footer = "Source: Eurostats"
                            )
                        ),
                        conditionalPanel(
                            "input.rtype == 4",
                            box(width = 9,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Vertical bar plot",
                                status = "danger",
                                plotOutput("rDistPlot"),
                                footer = "Source: Eurostats"
                            )
                        ),
                        
                        conditionalPanel(
                            "input.rtype == 5",
                            box(width = 9,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Horizontal bar plot",
                                status = "danger",  
                                plotOutput("rHBarPlot"),
                                footer = "Source: Eurostats"
                            )
                        ),
                        
                        conditionalPanel(
                            "input.rtype == 6",
                            box(width = 9,
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                title = "Horizontal bar plot",
                                status = "danger",   
                                plotlyOutput("rPiePlot"),
                                footer = "Source: Eurostats"
                            )
                        )
                        #)
                    )
            ),
            tabItem(tabName = "dat",
                    box(width = 12,
                        solidHeader = TRUE,
                        collapsible = FALSE,
                        title = "Data",
                        status = "danger",
                        dataTableOutput('table'),
                        downloadButton("downloadData", "Download Dataset")
                    )
            )
        )
    )
)

server <- function(input, output,session) {
    
    output$menu <- renderMenu({
        sidebarMenu(
            menuItem("Yearwise visualization", tabName="t1", icon = icon("calendar")),
            menuItem("Regionwise visualization", tabName="t2", icon = icon("map")),
            menuItem("Data", tabName="dat", icon = icon("hdd", lib = "glyphicon"))
        )
    })
    
    output$table <- renderDataTable(restructured_data%>%
                                        rename("Year"=yr) %>%
                                        rename("Quantity"=cnt)
                                    ,
                                    options = list(
                                        pageLength = 10
                                        #initComplete = I("function(settings, json) {alert('Done.');}")
                                    )
    )
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("transportation_data.csv", sep = "")
        },
        content = function(file) {
            write.csv(restructured_data, file, row.names = FALSE)
        }
    )
    
    output$distPlot <- renderPlot({
        restructured_data %>%
            filter(dflag == input$dtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(year == input$yor) %>%
            ggplot(aes(x=land, y=count1, fill=land)) + 
            geom_bar(stat="identity", width=.5) + 
            #labs(#title="Ordered Bar Chart", 
            #subtitle="Region vs TKM of goods", 
            #     caption="source: eurostats") + 
            theme(axis.text.x = element_text(angle=65, vjust=0.6))
    })
    
    
    output$map <- renderPlot({
        restructured_data %>%
            filter(dflag == input$dtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(weight = as.numeric(cnt)) %>%
            select(land, year, weight) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(year == input$yor) %>%
            select(land,weight) ->rail_restructured_data2
    
            mapped_data <- joinCountryData2Map(rail_restructured_data2, joinCode = "ISO2", 
                                           nameJoinColumn = "land")
            par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
            mapParams <-mapCountryData(mapped_data, nameColumnToPlot = "weight", mapRegion = 'Europe',colourPalette = 'rainbow' ,addLegend = TRUE)
            
            do.call(addMapLegend, c(mapParams
                                    ,legendLabels="all"
                                    ,legendWidth=0.5
                                    ,legendIntervals="data"))
        })
    
    output$hBarPlot <- renderPlot({
        restructured_data %>%
            filter(dflag == input$dtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(year == input$yor) %>%
            ggplot(aes(x=land,y=count1,fill=land)) +
            geom_bar(position="dodge",stat="identity") + 
            coord_flip() #+
        #ggtitle("Regionwise transportation of goods by Rail")
        
    })
    
    output$piePlot <- renderPlotly({
        colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
        restructured_data %>%
            filter(dflag == input$dtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(year == input$yor) %>%
            plot_ly(labels = ~land, values = ~count1, type = "pie",
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE) %>%
            layout(#title = 'Regionwise transportation of goods by Rail',
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
    output$mostExpensive <- renderTable({
        restructured_data %>%
            filter(dflag == input$dtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(year == input$yor) %>%
            select(land, count1) %>%
            arrange(-count1) %>% 
            rename("Quantity"=count1) %>%
            rename("Region"=land) %>%
            head(5)
    }, align='c'
    )
    
    output$top <- renderInfoBox({
        rr <- restructured_data %>%
            filter(dflag == input$dtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(year == input$yor) %>%
            select(land, count1) %>%
            arrange(-count1) %>% 
            head(1)
        infoBox(
            title = "Top Region", value = paste(rr$land,": ",rr$count1, " Thousand tonnes"), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "light-blue", fill = TRUE
        )    
    })
    
    output$last <- renderInfoBox({
        rr <- restructured_data %>%
            filter(dflag == input$dtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(year == input$yor) %>%
            select(land, count1) %>%
            arrange(count1) %>% 
            head(1)
        infoBox(
            "Last Region", value = paste(rr$land,": ",rr$count1," Thousand tonnes"), icon = icon("thumbs-down", lib = "glyphicon"),
            color = "orange", fill = TRUE
        )
    })
    
    output$highest <- renderInfoBox({
        h1 <- restructured_data %>%
            filter(dflag == input$rdtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(land == input$land) %>%
            select(year, count1) %>%
            arrange(-count1) %>% 
            head(1)
        infoBox(
            "Highest trade in the year ", value = paste(h1$year," : ",h1$count1), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "maroon", fill = TRUE
        )
    })
    
    output$lowest <- renderInfoBox({
        l1 <- restructured_data %>%
            filter(dflag == input$rdtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(land == input$land) %>%
            select(year, count1) %>%
            arrange(count1) %>% 
            head(1)
        infoBox(
            "lowest trade in the year ", value = paste(l1$year,": ",l1$count1), icon = icon("thumbs-down", lib = "glyphicon"),
            color = "purple", fill = TRUE
        )
    })
    
    # plots for regionwise visualization
    
    
    output$rDistPlot <- renderPlot({
        restructured_data %>%
            filter(dflag == input$rdtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(land == input$land) %>%
            ggplot(aes(x=year, y=count1, fill=year)) + 
            geom_bar(stat="identity", width=.5) + 
            #labs(#title="Ordered Bar Chart", 
            #subtitle="Year vs TKM of goods", 
            #     caption="source: eurostats") + 
            theme(axis.text.x = element_text(angle=65, vjust=0.6))
    })
    
    output$rHBarPlot <- renderPlot({
        restructured_data %>%
            filter(dflag == input$rdtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(land == input$land) %>%
            ggplot(aes(x=year,y=count1,fill=year)) +
            geom_bar(position="dodge",stat="identity") + 
            coord_flip() #+
        #ggtitle("Regionwise transportation of goods by Rail")
        
    })
    
    output$rPiePlot <- renderPlotly({
        colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
        restructured_data %>%
            filter(dflag == input$rdtype) %>%
            #mutate(region = str_replace_all(tra_cov.unit.geo.time,"^TOTAL.*,","")) %>%
            mutate(year = as.numeric(str_replace_all(yr,"X",""))) %>%
            mutate(count1 = as.numeric(cnt)) %>%
            select(land, year, count1) %>%
            filter(land != "EU27" & land !="EU28") %>%
            filter(land == input$land) %>%
            plot_ly(labels = ~year, values = ~count1, type = "pie",
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE) %>%
            layout(#title = 'Regionwise transportation of goods by Rail',
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$rStackedPlot <- renderPlotly({
        
        stackeddata<-restructured_data %>%
            filter(land == input$land) %>%
            spread(dflag,cnt)
        
        if(! ("Rail" %in% colnames(stackeddata)))
        {
            stackeddata$Rail <- 0
        }
        if(! ("Road" %in% colnames(stackeddata)))
        {
            stackeddata$Road <- 0
        }
        if(! ("Sea" %in% colnames(stackeddata)))
        {
            stackeddata$Sea <- 0
        }
        if(! ("Inland" %in% colnames(stackeddata)))
        {
            stackeddata$Inland <- 0
        }
        
        
        plot_ly(stackeddata, x = ~yr, y = ~Rail, type = 'bar', name = 'Railway') %>%
            add_trace(y = ~Road, name = 'Roadway') %>%
            add_trace(y = ~Sea, name = 'Sea') %>%
            add_trace(y = ~Inland, name = 'Inland Water')%>% 
            layout(yaxis = list(title = 'Count'), barmode = 'stack')
        
    })
    
    output$overall <- renderTable({
        
        stackeddata1<-restructured_data %>%
            #filter(land == 'DE') %>%
            filter(land == input$land) %>%
            spread(dflag,cnt)
        
        if(! ("Rail" %in% colnames(stackeddata1)))
        {
            stackeddata1$Rail <- 0
        }
        if(! ("Road" %in% colnames(stackeddata1)))
        {
            stackeddata1$Road <- 0
        }
        if(! ("Sea" %in% colnames(stackeddata1)))
        {
            stackeddata1$Sea <- 0
        }
        if(! ("Inland" %in% colnames(stackeddata1)))
        {
            stackeddata1$Inland <- 0
        }
        
        
        srail<- sum(as.numeric(stackeddata1$Rail),na.rm = TRUE)
        sroad <- sum(as.numeric(stackeddata1$Road),na.rm = TRUE)
        ssea <- sum(as.numeric(stackeddata1$Sea),na.rm = TRUE)
        sinland <- sum(as.numeric(stackeddata1$Inland),na.rm = TRUE)
        
        type <- c("Rail","Road","Sea","Inland")
        total_wt <- c(srail,sroad,ssea,sinland)
        
        main_data<- data.frame(type,total_wt)
        
        main_data %>%  
            arrange(-total_wt)
    }, align='c'
    )
    
    isolate({updateTabItems(session, "tabs", "t1")})
}
shinyApp(ui, server)
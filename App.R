library(shinydashboard)
library(shiny)
library(leaflet)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(scales)
library(plotly)

Store<-read.csv("Store.csv")
Department<-read.csv("Department.csv")
Item<-read.csv("Item.csv")
Invoice_header<-read.csv("Invoice_header.csv")
Items_invoice<-read.csv("Items_invoice.csv")

Invoice_header$Date<-as.Date(Invoice_header$Date)

lat<-c(41.881832,19.432608,34.052235,40.730610,-23.533773,4.624335,20.659698,25.686613,25.761681,37.773972,45.508888,47.608013,32.779167,29.749907,-33.447487)
lon<-c(-87.623177,-99.133209,-118.243683,-73.935242,-46.625290,-74.063644,-103.349609,-100.316116,-80.191788,-122.431297,-73.561668,-122.335167,-96.808891,-95.358421,-70.673676)

Store$lat<-lat
Store$lon<-lon

DF_list<-list(Store,Department,Item,Invoice_header,Items_invoice)
names(DF_list)<-c("Store","Department","Item","Invoice_header","Items_invoice")
#=============================================UI=================================================
header <- dashboardHeader(title = "Retail store",
                          dropdownMenu(type="notifications",
                                       notificationItem(text=paste("Last update", max(Invoice_header$Date)), status="info"),
                                       badgeStatus="primary")
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    dateRangeInput("Select_date","Select date range", start=min(Invoice_header$Date),end=max(Invoice_header$Date)),
    menuItem(tagList(icon("shopping-basket"), "Overall"),tabName = "overall"),
    menuItem(tagList(icon("store"),"Store review"),tabName = "store"),
    menuItem(tagList(icon("file-download"),"Data downloads"),tabName = "downloads")
  )
)

body <- dashboardBody(
  tags$head(includeHTML("retail_demo_analytics.html")),
  tabItems(
    tabItem(tabName = "overall",
            fluidRow(valueBoxOutput("Total_revenues",width = 4),
                     valueBoxOutput("Total_invoices",width = 4),
                     valueBoxOutput("Sold_items",width = 4)),
            fluidRow(column(width = 6,
                            box(width = NULL,leafletOutput("Map"))),
                     column(width = 6,
                            box(width = NULL, plotlyOutput("Plot_1")))),
            fluidRow(column(width = 6,
                            box(width = NULL,plotlyOutput("Plot_2"))),
                     column(width = 6,
                            box(width = NULL,plotlyOutput("Plot_3")))
                     )),
    tabItem(tabName = "store",
            fluidRow(column(width = 6,
                            box(width = NULL, selectInput("Select_store","Select a store",
                                                          choices = Store$City,multiple = FALSE,
                                                          selected = "New York")))),
            fluidRow(column(width = 6,
                            box(width = NULL, plotlyOutput("Plot_4"))),
                     column(width = 6,
                            box(width = NULL, plotlyOutput("Plot_5")))),
            fluidRow(column(width = 6,
                            box(width = NULL,plotlyOutput("Plot_6"))),
                     column(width = 6,
                            box=NULL,plotlyOutput("Plot_7")))),
    tabItem(tabName = "downloads",
            fluidRow(column(width = 6,
                            box(width = NULL,selectInput("Select_table","Select a table to download",
                                                         choices=c("Store","Department","Item",
                                                                   "Invoice_header","Items_invoice"),
                                                         multiple = FALSE, selected = "Store"))),
                     column(width = 4,
                            box(width = NULL,downloadButton("Download_data","Download table")))),
            fluidRow(column(width = 8,
                            box(width = NULL,tableOutput("Table_1"))))
            )
  )

)

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin="black"
)

#============================================App server=====================================================================
server<-shinyServer(function(session, input, output) {
  
  #===========================Overall tab=====================
  Item_revenue<-reactive({
    Items_invoice %>% inner_join(Item, by=c("Code"="Code")) %>% 
      inner_join(Invoice_header, by=c("Invoice_ID"="Invoice_ID")) %>% 
      filter(Date>= input$Select_date[1] & Date<=input$Select_date[2])
  })
  
  output$Total_revenues<-renderValueBox({
    valueBox(value=paste0(round(sum(Item_revenue()$Price_USD)/1000,0),"k"),
             subtitle = "Total period revenues",icon=icon("dollar-sign"), color="blue"
             )
  })
  
  output$Total_invoices<-renderValueBox({
    valueBox(value=length(unique(Item_revenue()$Invoice_ID)),
             subtitle = "Billed invoices",icon=icon("file-invoice-dollar"), color="blue"
    )
  })
  
  output$Sold_items<-renderValueBox({
    valueBox(value=nrow(Item_revenue()),
             subtitle = "Sold items",icon=icon("shopping-cart"), color="blue"
    )
  })
  
  Store_summary<-reactive({
    Invoice_header %>% filter(Date>= input$Select_date[1] & Date<=input$Select_date[2]) %>%
      group_by(Store_ID) %>% summarise(Number_sales=n()) %>% 
      inner_join(Store, by=c("Store_ID"="Store_ID")) %>% arrange(desc(Number_sales)) %>% 
      top_n(10, Number_sales)
  })
  
  output$Map<-renderLeaflet({
    leaflet() %>% 
      addTiles()%>% 
      setView(lng = -80, lat = 10, zoom = 2) %>%
      addCircleMarkers(data = Store_summary(),
                       lng = ~ lon,
                       lat = ~ lat,
                       radius = ~ log(Number_sales+5000), 
                       color = "#004c91",
                       fillOpacity = 1,
                       stroke = FALSE,
                       label = ~ City,
                       popup = ~ paste0(City, ": ",Number_sales, " sales")
      )
  })
  
  Department_summary<-reactive({
    Items_invoice %>% inner_join(Invoice_header,by=c("Invoice_ID"="Invoice_ID")) %>% 
      filter(Date>= input$Select_date[1] & Date<=input$Select_date[2]) %>% 
      inner_join(Item, by=c("Code"="Code")) %>% 
      group_by(Depa_ID) %>% summarise(Number_sales=n()) %>% 
      inner_join(Department, by=c("Depa_ID"="Depa_ID")) %>% arrange(desc(Number_sales)) %>% head(10)
    
  })
  
  output$Plot_1<-renderPlotly({
    ggplotly(
      ggplot(data=Department_summary(), aes(x=Number_sales,y=reorder(Depa_name,Number_sales))) +
        geom_bar(stat ="identity",fill="#ffc220") +
        geom_text(aes(label=Number_sales), vjust=0.5, hjust=0.9,color="black", size=3.5) +
        scale_x_continuous(labels = comma) +
        labs(title = "Top 10 departments for the selected dates",
             x = "Number of sales",
             y = "Department") +
        theme_minimal()
    )
  })
  
  Item_summary<-reactive({
    Items_invoice %>% inner_join(Invoice_header,by=c("Invoice_ID"="Invoice_ID")) %>% 
      filter(Date>= input$Select_date[1] & Date<=input$Select_date[2]) %>% 
      group_by(Code) %>% summarise(Number_sales=n()) %>% 
      inner_join(Item, by=c("Code"="Code")) %>% arrange(desc(Number_sales)) %>% head(20)
  })
  
  output$Plot_2<-renderPlotly({
    ggplotly(
      ggplot(data=Item_summary(), aes(x=Number_sales,y=reorder(Name,Number_sales))) +
        geom_bar(stat ="identity",fill="#76c043") +
        geom_text(aes(label=Number_sales), vjust=0.5, hjust=0.9,color="black", size=3.5) +
        scale_x_continuous(labels = comma) +
        labs(title = "Top 20 selling items for the selected dates",
             x = "Number of sales",
             y = "Item name") +
        theme_minimal()
    )
  })
  
  Trend_summary<- reactive({
    Invoice_header %>% group_by(Date) %>% summarise(Number_sales=n())
  })
  
  output$Plot_3<-renderPlotly({
    ggplotly(
      ggplot(Trend_summary(), aes(x=Date, y=Number_sales)) +
        geom_point(color="#78b9e7",alpha=0.6) +
        geom_line(color="#007dc6") + 
        scale_x_date(date_breaks = "15 days", date_labels ="%d-%b") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1)) +
        labs(title="Sales per day",
             x="Day", y="Sales")
    )
  })
  
  #===========================Store tab=====================
  
  Hourly_store<-reactive({
    Invoice_header %>% filter(Date>= input$Select_date[1] & Date<=input$Select_date[2]) %>% 
      group_by(Store_ID, Local_hour) %>% summarise(Number_sales=n()) %>% 
      inner_join(Store, by=c("Store_ID","Store_ID")) %>% 
      filter(City %in% input$Select_store)
  })
  
  output$Plot_4<-renderPlotly({
    ggplotly(
      ggplot(Hourly_store(), aes(x=Local_hour, y=Number_sales)) +
        geom_point(color="#f47321",alpha=0.6) +
        geom_line(color="#f47321") + 
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1)) +
        labs(title=paste0("Total sales per hour for ",input$Select_store),
             x="Hour", y="Sales")
    )
  })
  
  Item_Store_summary<-reactive({
    Items_invoice %>% inner_join(Invoice_header,by=c("Invoice_ID"="Invoice_ID")) %>%
      filter(Date>= input$Select_date[1] & Date<=input$Select_date[2]) %>% 
      group_by(Store_ID, Code) %>% summarise(Number_sales=n()) %>% 
      inner_join(Item, by=c("Code"="Code")) %>% 
      inner_join(Store, by=c("Store_ID","Store_ID")) %>% 
      filter(City %in% input$Select_store) %>% 
      arrange(desc(Number_sales)) %>% head(20)
  })
  
  output$Plot_5<-renderPlotly({
    ggplotly(
      ggplot(data=Item_Store_summary(), aes(x=Number_sales,y=reorder(Name,Number_sales))) +
        geom_bar(stat ="identity",fill="#76c043") +
        geom_text(aes(label=Number_sales), vjust=0.5, hjust=0.9,color="black", size=3.5) +
        scale_x_continuous(labels = comma) +
        labs(title = paste0("Top 20 selling items for ", input$Select_store),
             x = "Number of sales",
             y = "Item name") +
        theme_minimal()
    )
  })
  
  Department_Store_summary<-reactive({
    Items_invoice %>% inner_join(Invoice_header,by=c("Invoice_ID"="Invoice_ID")) %>% 
      filter(Date>= input$Select_date[1] & Date<=input$Select_date[2]) %>% 
      inner_join(Item, by=c("Code"="Code")) %>% 
      group_by(Store_ID, Depa_ID) %>% summarise(Number_sales=n()) %>% 
      inner_join(Department, by=c("Depa_ID"="Depa_ID")) %>% 
      inner_join(Store, by=c("Store_ID","Store_ID")) %>% 
      filter(City %in% input$Select_store) %>% 
      arrange(desc(Number_sales)) %>% head(10)
  })
  
  output$Plot_6<-renderPlotly({
    ggplotly(
      ggplot(data=Department_Store_summary(), aes(x=Number_sales,y=reorder(Depa_name,Number_sales))) +
        geom_bar(stat ="identity",fill="#ffc220") +
        geom_text(aes(label=Number_sales), vjust=0.5, hjust=0.9,color="black", size=3.5) +
        scale_x_continuous(labels = comma) +
        labs(title = paste0("Top 10 departments for ",input$Select_store),
             x = "Number of sales",
             y = "Department") +
        theme_minimal()
    )
  })
  
  Trend_Store_summary<- reactive({
    Invoice_header %>% group_by(Store_ID, Date) %>% summarise(Number_sales=n()) %>% 
      inner_join(Store, by=c("Store_ID","Store_ID")) %>% filter(City %in% input$Select_store)
  })
  
  output$Plot_7<-renderPlotly({
    ggplotly(
      ggplot(Trend_Store_summary(), aes(x=Date, y=Number_sales)) +
        geom_point(color="#78b9e7",alpha=0.6) +
        geom_line(color="#007dc6") + 
        scale_x_date(date_breaks = "15 days", date_labels ="%d-%b") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1)) +
        labs(title=paste0("Sales per day for ", input$Select_store),
             x="Day", y="Sales")
    )
  })
  
  #===========================Download tab=====================
  
  Table_to_download<-reactive({
    if(input$Select_table=="Store")
    {
      DF_list$Store
    } else if(input$Select_table=="Department")
    {
      DF_list$Department
    } else if(input$Select_table=="Item")
    {
      DF_list$Item
    } else if(input$Select_table=="Invoice_header")
    {
      DF_list$Invoice_header
    } else
    {
      DF_list$Items_invoice
    }
    
    
  }) 
  
  output$Table_1<-renderTable({
    Table_to_download()
  })
  
  output$Download_data<-downloadHandler(
    filename = function() {
      paste(input$Select_table,"_Table.csv")
    },
    content = function(file)
    {
      write.csv(Table_to_download(),file,row.names = FALSE)
    }
  )
  

})

shinyApp(ui, server)


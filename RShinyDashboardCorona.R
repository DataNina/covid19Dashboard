############################################## Interactive Dashboard Corona #####################################

##### Preparation----



#Load libraries
library(shiny)
library(shinydashboard)
library(extrafont)


#Set default language to English
Sys.setlocale("LC_TIME", "English")


#Loading and running of data preparation file 
source("C:/Users/Nina/Documents/R/Corona/DatapreparationCorona.R")


#### Define colorlist for plot----

###############color list ####################################
n=length(unique(covid19data$country))
colorlist=rainbow(n, s=1, v=1, start=0, end=max(1,n-1)/n, alpha=0)
colorlist=substring(colorlist,1,7)
colorlist=sample(substr(colorlist,1,n))
colorList=as.data.frame(cbind(country=unique(covid19data$country),colorlist), stringsAsFactors = FALSE)

#Define symbol list for plot
###############color list ####################################
m=length(unique(covid19data$category))
symbollist=c("\u25B2", "\u25A0", "\u25CF", "\u25C6")
symbolList=as.data.frame(cbind(category=unique(covid19data$category),symbollist), stringsAsFactors = FALSE) %>% arrange(category)




#Define UI


# Dashboard ---------------------------------------------------------------
ui <- fluidPage(


  titlePanel("Covid-19 Dashboard"),
    
    sidebarPanel(
        style = "background-color: #78B9D9;",
        tags$style(
          type = 'text/css',
          ".selectize-input { font-size: 12pt; line-heigt: 13pt; },
          .selectize-dropdown { font-size: 12pt; line-height: 13pt; }"
        ),
        width = 4,
        
        
    checkboxGroupInput(
      inputId = "category",
      label = h3("Choose category"),
      choices = c("confirmed" = "confirmedCases",
                  "recovered" = "recoveredCases",
                  "active"="activeCases",
                  "deaths" = "deaths"),
      selected = "activeCases"),    
        
        
        
    selectInput( 
      inputId = "country",
      label = h3("Choose country"),
      choices = unique(covid19data$country),
      multiple = TRUE,
      selected = "Germany"),  
      
      
    selectInput(
      inputId = "xAxis",
      label = h3("Choose x-Axis"),
      choices = c("Date" = "date",
                  "Days since 100 Infections" ="Number100"),
      multiple=FALSE,
      selected = "Date"),   
      
   
   
   dateRangeInput(
     inputId = "dateX",
     label = h3("Select date"),
     format = "dd/mm/yyyy",
     start = min(covid19data$date),
     end = max(covid19data$date),
     min = min(covid19data$date),
     max = max(covid19data$date),
     separator = "to"
   )
   
   
    ) ,
   
  uiOutput("checkbox"),
  dataTableOutput("plotdata"),
  
  

  
  mainPanel (
    tabsetPanel(
      tabPanel("Plot-Cases", 
               plotOutput("mplot"), 
               width="100%"),
      
      tabPanel("Plot-Reproduction Number", 
               plotOutput("rplot"), 
               width="100%"),
      
      tabPanel("Plot-7-days incidence",
               plotOutput("iplot"),
               width="100%"),
      
      tabPanel("Data",
               tableOutput("table")),
      tabPanel("Datasource", "Row data regarding covid19 is provided by the John Hopkins University and can be downloaded via github (see https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data).
               Row data for the total population is provided by the World Bank and covers the year 2019.
               The reproduction rate is calcualted taking into account the new infections of today and the last seven days by divinding the sum of the second half and the first half of the values.
               The 7-days incidence per 100 000 inhabitants is calculated by using the sum of the new infections over the last seven days by diving through the total populatio multiplied by 100 000.
               The dashobard is automatically updated on a daily basis.")

 )
)
)




  
#Server
server <- function(input, output) {
  output$checkbox<- renderUI({
    if (is.null(input$country))
      return()
  }
  )
  
  

  
  # reactive operations
  data <- reactive({
    validate(
      # error message, if no input or wrong input is selected
      need(
        input$category != "",
        "At least one category must be chosen"
      ),
      need(
        input$country != "",
        "At least one country must be chosen"
      ),
      need(
        input$dateX[2] > input$dateX[1],
        "Error: end date before start date"
      )
)
  
  #filter data for plot dependent on reactive data input (cases)
  plotdata <- covid19data  %>% 
                    filter(category %in% input$category) %>%
                    filter(country %in% input$country)
  
  plotdata1 <- plotdata
  plotdata2 <- covid19data %>%
    filter(category=="confirmedCases") %>%
    filter(country %in% input$country)
 

  if (input$xAxis=="Number100") {
    plotdata1$x <- plotdata1$Number100
    plotdata2$x <- plotdata2$Number100
    x_label = "Days since 100 infections"
  }
  else {
    plotdata1 <- plotdata1 %>%
                  filter(date >= input$dateX[1] & date <= input$dateX[2])
    plotdata1$x <- plotdata1$date
    plotdata2 <- plotdata2 %>%
                  filter(date >= input$dateX[1] & date <= input$dateX[2])
    plotdata2$x <- plotdata2$date
    x_label = ""
  }   

  
 
  plotdata2$ReproductionNumber[is.infinite(plotdata2$ReproductionNumber)]=0
  plotdata2$ReproductionNumber[is.na(plotdata2$ReproductionNumber)]=0
  plotdata2$ReproductionNumber[is.nan(plotdata2$ReproductionNumber)]=0

  plotdata2$Incidence[is.infinite(plotdata2$Incidence)]=0
  plotdata2$Incidence[is.na(plotdata2$Incidence)]=0
  plotdata2$Incidence[is.nan(plotdata2$Incidence)]=0


  # filter colorList dependent on reactive data input
  colorlistplot <- colorList %>%
    filter(country %in% input$country) 
  
  #filter symbolList dependent on reactice data input
  symbollistplot <- symbolList %>%
    filter(category %in% input$category)

  
  # make list of reactive results to use for the outputs
  list(
    plotdata1 = plotdata1,
    plotdata2 = plotdata2,
    x_label = x_label,
    colorlistplot = colorlistplot,
    symbollistplot = symbollistplot
  )

  
  }

)
  
# outputs, taking results of the reactive data()
# build the plot
output$mplot <- renderPlot ({
 myplot <-ggplot(
    data = data()$plotdata1,
      aes(
        x=x,
        y = number,
        shape = category,
        color = country
      )
   
  ) +
    
    geom_point(size=4) +
    geom_line(lwd=1) +
    scale_color_manual(values=data()$colorlistplot$colorlist) +
    scale_shape_manual(values=data()$symbollistplot$symbollist) + 
    labs(y="Number", x=data()$x_label) + 
   
    scale_y_continuous(labels=scales::comma) +
    
    
    theme(legend.title = element_text(size = 18),
          legend.text = element_text(size = 14)) +
    
    ggtitle("Development of Covid-19") +
    theme(
      plot.title = element_text(
        family = "Arial",
        face = "bold",
        size = 20
      ),
      axis.text.x = element_text(
        angle = 0,
        family = "Arial",
        size = 14
      ),
      axis.text.y = element_text(
        angle = 0,
        family = "Arial",
        size = 14
      ),
      axis.title.x = element_text(
        size = 16,
        face = "bold",
        vjust = -1
      ),
      axis.title.y = element_text(
        size = 16,
        face = "bold",
        vjust = 2
      )
    )
 if (is.numeric(data()$plotdata1$x)) {
   myplot <- myplot + scale_x_continuous(limits=c(0, max(data()$plotdata1$Number100)))  
 }
 else {
   myplot <- myplot + scale_x_date(limits=c(input$dateX[1], input$dateX[2]),date_labels = "%d-%b-%Y") 
 }
 
    
    print(myplot)
},

height=600, width=1200

)

output$rplot <- renderPlot ({
  myplot2 <-ggplot(
    data = data()$plotdata2,
    aes(
      x=x,
      y = ReproductionNumber,
      color = country,
      fill = country
    )
    
  ) +
    geom_bar(stat="identity") + 
    scale_color_manual(values=data()$colorlistplot$colorlist) +
    scale_fill_manual(values=data()$colorlistplot$colorlist) +
    labs(y="Reproduction Number", x=data()$x_label) + 
    
    scale_y_continuous(labels=scales::comma) +
    
    
    theme(legend.title = element_text(size = 18),
          legend.text = element_text(size = 14)) +
    
    ggtitle("Development of Covid-19") +
    theme(
      plot.title = element_text(
        family = "Arial",
        face = "bold",
        size = 20
      ),
      axis.text.x = element_text(
        angle = 0,
        family = "Arial",
        size = 14
      ),
      axis.text.y = element_text(
        angle = 0,
        family = "Arial",
        size = 14
      ),
      axis.title.x = element_text(
        size = 16,
        face = "bold",
        vjust = -1
      ),
      axis.title.y = element_text(
        size = 16,
        face = "bold",
        vjust = 2
      )
    )

  if (is.numeric(data()$plotdata2$x)) {
    myplot2 <- myplot2 + scale_x_continuous(limits=c(0, max(data()$plotdata2$Number100)))  
  }
  else {
    myplot2 <- myplot2 + scale_x_date(date_labels = "%d-%b-%Y") 
  }
  
  
  
  print(myplot2)
},

height=600, width=1200

)



output$iplot <- renderPlot ({
  myplot3 <-ggplot(
    data = data()$plotdata2,
    aes(
      x=x,
      y = Incidence,
      color = country,
      fill = country
    )
    
  ) +
    
    geom_bar(stat="identity") + 
    scale_color_manual(values=data()$colorlistplot$colorlist) +
    scale_fill_manual(values=data()$colorlistplot$colorlist) +
    labs(y="7-days incidence per 100 000 inhabitants", x=data()$x_label) + 
    
    scale_y_continuous(labels=scales::comma) +
    
    
    theme(legend.title = element_text(size = 18),
          legend.text = element_text(size = 14)) +
    
    ggtitle("Development of Covid-19") +
    theme(
      plot.title = element_text(
        family = "Arial",
        face = "bold",
        size = 20
      ),
      axis.text.x = element_text(
        angle = 0,
        family = "Arial",
        size = 14
      ),
      axis.text.y = element_text(
        angle = 0,
        family = "Arial",
        size = 14
      ),
      axis.title.x = element_text(
        size = 16,
        face = "bold",
        vjust = -1
      ),
      axis.title.y = element_text(
        size = 16,
        face = "bold",
        vjust = 2
      )
    )
  
  if (is.numeric(data()$plotdata2$x)) {
    myplot3 <- myplot3 + scale_x_continuous(limits=c(0, max(data()$plotdata2$Number100)))  
  }
  else {
    myplot3 <- myplot3 + scale_x_date(date_labels = "%d-%b-%Y") 
  }
  
  
  
  print(myplot3)
},

height=600, width=1200

)



    #reactive table
    
    output$table <- renderTable({
      covid19data %>%
        as.data.frame() %>%
        filter(country %in% input$country) %>% filter(category %in% input$category) %>%
        arrange(country) %>%
        arrange(category) %>%
        mutate(date=format(date, "%Y-%m-%d")) %>%
        select(-c("TotalPop", "SevenDaySum", "FourDaySum")) %>%
        dplyr::rename("DaysSince100Infections"="Number100", "7-days incidence per 100 000 inhabitants"="Incidence")
}
)




}

shinyApp(ui=ui, server=server)

# End --------------------------------------------------------------------




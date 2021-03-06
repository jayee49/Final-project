require(shiny)
require(tidyverse)
require("shinyjs")
library(plotly)
library(ggmap)
library(rworldmap)
library(shinythemes)

country=read.csv(file="http://apps.who.int/gho/athena/data/xmart.csv?target=GHO/SDGTOBACCO&profile=crosstable&filter=COUNTRY:*&x-sideaxis=COUNTRY&x-topaxis=GHO;YEAR;SEX",header = F)
region=read.csv(file="http://apps.who.int/gho/athena/data/xmart.csv?target=GHO/SDGTOBACCO&profile=crosstable&filter=COUNTRY:-;REGION:*&x-sideaxis=REGION&x-topaxis=GHO;YEAR;SEX",header = F)
global=read.csv(file="https://raw.githubusercontent.com/TietuoZou/Data/master/123.csv")


global$Country = as.character(global$Country)
global$Indicator.short.code = as.character(global$Indicator.short.code)
global$Region = as.character(global$Region)

region = as.matrix.data.frame(region)
colnames(region) = region[3,]
region = as.data.frame(region[-(1:3),])
region$`WHO region` = as.character(region$`WHO region`)
region[,2:4] = as.numeric(as.matrix(region[,2:4]))

country = as.matrix.data.frame(country)
colnames(country) = country[3,]
country = as.data.frame(country[-(1:3),])
country$Country = as.character(country$Country)
country[,2:4] = as.numeric(as.matrix(country[,2:4]))

### find out the lost country 
global$Country[which(is.na(match(global$Country,country$Country)))] 
country$Country[which(is.na(match(country$Country,global$Country)))]

country$Country[which(country$Country == "Eswatini")] = "Swaziland"
country$Country[which(country$Country == "United Kingdom of Great Britain and Northern Ireland")] = "United Kingdom"

global.n.sex = merge(global,country,by.x = "Country")
colnames(global.n.sex) = c("Country", "Value.Regional", "Value.global","Number.of.Records",
                           "Value", "Indicator.short.code", "CODE", "Region", "Year",
                           "Both.sexes", "Male", "Female")
gender = global.n.sex %>% gather(Male,Female,Value, key = "gender", value = "gender.value")
gender[gender=="Value"]="Both"

list_choices <- append("World",unique(global.n.sex$Region)[!is.na(unique(global.n.sex$Region))])
names(list_choices) <- paste(append("World",unique(global.n.sex$Region)[!is.na(unique(global.n.sex$Region))]),"",sep="")


# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
                 theme = shinytheme("paper"),
                 
                 tabPanel("data",
                          fluidPage(
                            
                            column(12,align="center",
                                   titlePanel(h3("Age-standardized prevalence of tobacco smoking among persons 15 years and older (%),by WHO region, 2016"))),
                            
                            DT::dataTableOutput("tabplot")
                            
                            
                          ), # fluidPage
                          fluidPage(
                            plotOutput(outputId = "plot",
                                       click = "plot_click",
                                       brush = "plot_brush"),

                            column(11,align="center",
                                   tableOutput("info")),
                            column(11,align="center",
                                   tableOutput("brush_info")),
                            
                            plotOutput(outputId="barplot")
                          ) # fluidPage
                       ),# tabpanel
                 
                 tabPanel("gender differences",
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("select", label = h3("Gender Selection"), 
                                          choices = c("non-gender", "gender"),
                                          selected = 1)
                              
                            ), # sidebarPanel
                            mainPanel(
                              plotOutput(outputId = "boxplot")
                            ) # mainPanel
                          ) # sidebarLayout
                 ), # tabPanel
                 
                 #### Adding one more panel for Map
                 tabPanel("World Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("map_opt",label = h3("Gender"),
                                          choices = c("Both","Male","Female"),
                                          selected = 1) #selectInput
                            ), #sidebarPanel
                            mainPanel(
                              plotOutput(outputId ="worldmap",
                                         hover = "map2"),
                              verbatimTextOutput("infof")
                            ) #mainPanel
                          )# siderbarLayout
                 )# tabpanel
) # navbarPage


col_scale <- scale_colour_discrete(limits = unique(global.n.sex$Region))
col=c("Europe"="#3366cc","Africa"="#dc3912","Americas"="#ff9900","Eastern Mediterranean"="#109618","South-East Asia"="#990099","Western Pacific"="#0099c6")

server <- function(input, output, session) {
  
  output$tabplot=DT::renderDataTable({
    DT::datatable(global.n.sex %>% select(Country, Value, Male, Female, 
                                          Region,Value.Regional),
                  filter="top",options = list(
                    pageLength=5,width = 10,height = 20
                  )
    )
  })
  
  output$plot <- renderPlot({
    ggplot(global.n.sex, aes(x=Value, y=Value.Regional, colour = Region)) + 
      geom_point(size=3) +
      scale_color_manual(values=col,aesthetics = "colour")
  })
  
  output$info <- renderTable({
    nearPoints(global.n.sex 
               %>% select(Country, Region, Value, Male, Female, Value.Regional), 
               input$plot_click, threshold = 10, maxpoints = 1,
               addDist = F)
  },width = 800);
  
  ######################
  #### Adding barplot
  ######################
  
  output$barplot <- renderPlot({
    if(is.null(input$plot_click)) 
      return(NULL)
    if(is.na(nearPoints(global.n.sex %>% select(Country, Region, Value, Value.Regional), 
                        input$plot_click, threshold = 10, maxpoints = 1)$Country))
      return(NULL)
    
    else{
      ggplot(global.n.sex %>% filter(Region == nearPoints(global.n.sex %>% select(Country, Region, Value, Value.Regional), 
                                                          input$plot_click, threshold = 10, maxpoints = 1)$Region), 
             aes(x=Country, y=Value, fill=Region)) +
        scale_color_manual(values=col,aesthetics = c("fill"))+
        
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90)) +
        geom_hline(aes(yintercept=mean(Value)),linetype=5,col="grey") # change 2+
      
    } #else
    
  });#renderplot
  
  
  
  output$brush_info <- renderTable({
    brushedPoints(global.n.sex 
                  %>% select(Country, Region, Value, Male, Female, Value.Regional), 
                  input$plot_brush)
  },width = 800);  
  
  updateSelectInput(session, "select",
                    choices = c("non-gender", "gender"),
                    selected = head(c("non-gender", "gender"), 1)
  );
  
  output$boxplot <- renderPlot({
    
    if(input$select == "non-gender"){
      
        ggplot(global.n.sex, aes(x=Region, y=Value, fill=Region))  +
          geom_boxplot() 
    }
    
    else{
      
        ggplot(gender[-which(gender$gender=="Both"),], aes(x=gender, y=gender.value, fill=Region))  +
          geom_boxplot() 
      

    }
  });
  

  
  updateSelectInput(session, "map_opt",
                    choices = c("Both", "Male","Female"),
                    selected = head(c("Both", " Male"," Female"), 1)
  );
  
  output$worldmap <- renderPlot({
    
    if(input$map_opt == "Both"){
      joinCountryData2Map(global.n.sex,joinCode = "NAME",
                          nameJoinColumn = "Country") %>% 
        mapCountryData(nameColumnToPlot = "Both.sexes",
                       mapTitle = "Rate of Aged 15+ Currently Smoke for Both Gender")
      
    } # Both if
    
    if(input$map_opt == "Male"){
      joinCountryData2Map(global.n.sex,joinCode = "NAME",
                          nameJoinColumn = "Country") %>% 
        mapCountryData(nameColumnToPlot = "Male",
                       mapTitle = "Rate of Aged 15+ Currently Smoke for Male")
      
    } # Male if
    
    if(input$map_opt == "Female"){
      joinCountryData2Map(global.n.sex,joinCode = "NAME",
                          nameJoinColumn = "Country") %>% 
        mapCountryData(nameColumnToPlot = "Female",
                       mapTitle = "Rate of Aged 15+ Currently Smoke for Female")
      
    } # Female if
  }
  ); #renderplot
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)




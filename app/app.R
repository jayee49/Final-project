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
global.n.sex$Value.Regional=round(global.n.sex$Value.Regional,digits = 1)
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
                            
                            
                          ) # fluidPage
                 )# tabpanel
) # navbarPage

server <- function(input, output, session) {
  
  output$tabplot=DT::renderDataTable({
    DT::datatable(global.n.sex %>% select(Country, Value, Male, Female, 
                                          Region,Value.Regional),
                  filter="top",options = list(
                    pageLength=5,width = 10,height = 20
                  )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
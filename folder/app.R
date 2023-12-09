library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(shiny)
library(sf)
library(DT)
library(reshape2)

df <- read.csv("df.csv")
precinct <- read.csv("precinct_graph.csv")
UHF <- read.csv("UHF_graph.csv")

df_total <- filter(df, Name=="Fine Particulate Matter (PM2.5)")
df_total_grp <- group_by(df_total, Year, Borough, Name) 
df_total_sum <- summarize(df_total_grp, across(c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), sum))

ui <- fluidPage(
  navbarPage("NYC Air Pollution and Crime",
             
    #tab for overview page
    tabPanel("Overview",
      h1("Overview")
    ),
    
    #tab for borough crime rate page
    tabPanel("Borough crime",
      fluidRow(
        column(4,
          wellPanel(
            p(strong("You can adjust the Year")),
            sliderInput(inputId="borough_year", "Select Year", 2009, 2020, 2009, sep=""),
            selectInput(inputId="borough_gender", "Select Gender", c("Male","Female","All"), "All"),
            selectInput(inputId="borough_age", "Select Age Group", c("<18","18-24", "25-44", "45-64", "65+"), "<18"),
            selectInput(inputId="borough_type", "Select Type", c("Misdemeanor","Felony","Violation"), "Misdemeanor")
          ),
          wellPanel(
            p(strong("Summary")),
            p("placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph
              placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph 
              placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph placeholder paragraph")
          )
        ),

        column(8,
          wellPanel(
            tabsetPanel(
              tabPanel("Borough Map",
                       h4("NYC Borough Map", align="center"),
                       fluidRow(
                         column(6, plotOutput(outputId="borough_choro_map")),
                         column(6, plotOutput(outputId="precinct_choro_map"))
                       ),
                       br(),
                       DT::dataTableOutput(outputId="borough_map_table")
              ),
              tabPanel("Population Graph",
                       h4("Population Graph", align="center"),
                       plotlyOutput(outputId="population_bar"),
                       br()
              )
              
            )
          )
        )
      ),
    ),
    
    #tab for seasonal air pollutant and crime rate page
    tabPanel("Seasonal crime",
    ),
    
    #tab for
    tabPanel("third page",
    ),
    
    #tab for conclusion page
    tabPanel("Conclusion",
             
    )
  )
)

server <- function(input, output) {
  output$borough_choro_map <- renderPlot({
    borough_shape <- st_read("nybb.shp")
    mask <- c()
    if (input$borough_gender=="Male") { mask <- c(mask, "male")}
    if (input$borough_gender=="Female") { mask <- c(mask, "female") }
    if (input$borough_gender=="all") { mask <- c(mask, "male", "female")}
    filtered <- filter(df_total_sum, Year==input$borough_year)
    filtered <- filtered[, c("Borough", mask)]
    borough_df <- merge(fortify(borough_shape), filtered, by.x="BoroName", by.y="Borough", all.x=TRUE)
    borough_df <- mutate(borough_df, total=rowSums(borough_df[, mask, drop=TRUE]))
    p <- ggplot(fortify(borough_df)) + geom_sf(aes(fill=total)) + scale_fill_gradient(low = "yellow", high = "red") 
    return(p)
  })
  
  output$precinct_choro_map <- renderPlot({
    precinct_shape <- st_read("nycc.shp")
    mask <- c()
    if (input$borough_gender=="Male") { mask <- c(mask, "male")}
    if (input$borough_gender=="Female") { mask <- c(mask, "female") }
    if (input$borough_gender=="all") { mask <- c(mask, "male", "female")}
    filtered <- filter(precinct, Year==input$borough_year)
    filtered <- filtered[, c("ARREST_PRECINCT", mask)]
    precinct_df <- merge(fortify(precinct_shape), filtered, by.x="precinct", by.y="ARREST_PRECINCT", all.x=TRUE)
    precinct_df <- mutate(precinct_df, total=rowSums(precinct_df[, mask, drop=TRUE]))
    p <- ggplot(fortify(precinct_df)) + geom_sf(aes(fill=total)) + scale_fill_gradient(low = "yellow", high = "red") 
    return(p)
  })
  
  output$borough_map_table <- renderDataTable({
    filtered_borough <- filter(select(df_total_sum, -c("Name")), Year==input$borough_year)
    return(filtered_borough)
  })
  
  output$population_bar <- renderPlotly({
    filtered_year <- filter(df_total_sum, Year==input$borough_year)
    dfm <- melt(filtered_year[, c("Borough","male","female")], id.vars=1)
    p <- ggplot(dfm) + geom_bar(aes(x=Borough, y=value, fill=variable), stat = "identity",position = "dodge")
    return(p)
  })
}

shinyApp(ui = ui, server = server)
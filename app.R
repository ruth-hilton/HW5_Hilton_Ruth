
#HW5_Hilton_Ruth
#Shiny App: IMO Results Summaries

library(shiny)
library(shinydashboard)
library(tidyverse)
library(viridis)
library(plotly)
library(shinyWidgets)
library(reactable)
library(dplyr)

#data import and merge
imo.names <- read_delim("imo_country_codes.csv", delim = "\t", show_col_types = FALSE)
imo.codes <- read.csv("imo_results.csv")
imo <- merge(imo.codes, imo.names[, c("country", "countryname")], by="country")
imo.country <- read.csv("imo_full_by_country.csv")

#data selection for map
imo %>% select(country,rank) %>% group_by(country) 
winnerdata <- imo %>% filter(rank==1)
winnerdata[winnerdata == "UNK"] <- "GBR"
winnerdata[winnerdata == "GDR" | winnerdata == "GER"] <- "DEU"
winnerdata[winnerdata == "USS" | winnerdata == "CIS"] <- "RUS"
winnerdata[winnerdata == "CZS"] <- "CZE"
winnerdata[winnerdata == "YUG"] <- "SRB"
numberwins <- count(winnerdata, country)
colnames(numberwins) <- c("country", "wins")

ui <- dashboardPage(
  #theme color
  skin = "purple",  
  
  #define the title
  dashboardHeader(
    title="Select Display"
  ),
  
  #define the sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Highest Scorers per Year by Country", tabName = "scatter"),
      menuItem("Winner Count by Country Map", tabName = "map"),
      menuItem("Country Results Table", tabName = "table")
    )
  ),
  
  #define the body
  dashboardBody(
    tabItems(
      # first page
      tabItem(tabName = "scatter",
              h2("International Mathematics Oympiad"),
              h3("Highest Scorer per Year by Country"),
              h4("(1984-2017)"),
              textInput("in_country", "Enter Country Name Here:", value = "United States of America",
                        placeholder = "type full county name then press enter..."),
              box(plotlyOutput("p_scatter"), width = 10)
              
      ),
      # second page
      tabItem(tabName = "map",
              h2("International Mathematics Oympiad"),
              h3("Map: Number of Winners by Country"),
              h4("(1984-2017)"),
              h5("(hover over country to see exact number of winners)"),
              box(plotlyOutput("p_map"), width = 400, height = 500)
      ),
      # third page
      tabItem(tabName = "table",
              h2("International Mathematics Oympiad"),
              h3("Country Results Table"),
              h4("All-Time (1959-present)"),
              h5("(select column name to sort, or type your search into the input 
                 boxes below to filter by that value)"),
              box(reactableOutput("t_country"), width = 500)
              
      )
    )
  )
)

server <- function(input, output) {
  
  # --------------------------------------------------
  # table of country results data
  # --------------------------------------------------
  
  output$t_country <- renderReactable({
    t_country <- imo.country %>% select(country, firstyear, yearsparticipated, participants, gold, silver, bronze) %>%
      reactable(
        columns = list(
          country = colDef(name = "Country", align = "center"),
          firstyear = colDef(name = "First Year Participated", align = "center"),
          yearsparticipated = colDef(name = "Total Years Participated", align = "center"),
          participants = colDef(name = "Total Participants", align = "center", filterable = FALSE),
          gold = colDef(name = "Gold Medals Won", align = "center", filterable = FALSE),
          silver = colDef(name = "Silver Medals Won", align = "center", filterable = FALSE),
          bronze = colDef(name = "Bronze Medals Won", align = "center", filterable = FALSE)
        ),
        defaultSortOrder = "desc",
        defaultSorted = c("gold", "silver", "bronze"),
        highlight = TRUE,
        outlined = TRUE,
        filterable = TRUE
      )
    })
  
  # --------------------------------------------------
  # graph of highest scoring individuals each year by country
  # --------------------------------------------------
  output$p_scatter <- renderPlotly({
    
    in_country <- input$in_country
    
    p_scatter <- ggplot(imo %>% 
                          filter(countryname == in_country) %>% 
                          group_by(year) %>% 
                          slice(which.max(total)),
                        aes(x = year, 
                            y = total,
                            color = rank,
                            text1 = firstname,
                            text2 = lastname)) +
      geom_point(shape = 16) +
      theme_minimal() +
      scale_color_viridis() +
      xlab(NULL) + 
      ylab("Total Point Score (0-42)") + 
      ggtitle("Highest Scoring Individual at International Mathematics Olympiad <br> Competition Years 1984-2017") +
      scale_x_continuous(limits = c(1987, 2017))
    ggplotly(p_scatter, tooltip = c("text1", "text2", "y", "color")) %>% 
      layout(hovermode = "x")
  })
  
  # --------------------------------------------------
  # winners country map 
  # --------------------------------------------------
  output$p_map <- renderPlotly({
    
    #create base map geometry
    g <- list(
      scope = 'world',
      projection = list(type = "natural earth")
    )
    
    #create world map with plotly 
    map <- plot_geo(numberwins, locationmode = 'world')
    
    #add choropleth and trace features
    map <- map %>% add_trace(
      z = ~wins, text = "winners", locations = ~country,
      color = ~wins, colorscale = 'Viridis', reversescale = TRUE
    )
    
    #modify formatting and labels
    map <- map %>% colorbar(title = "Number of Winners")
    map <- map %>% layout(
      title = 'Number of International Mathematics Competition Winners<br>since 1984 by Country of Origin',
      geo = g
    )
    map
  })
}

shinyApp(ui, server)
library(shiny)
library(tidyverse)
library(dplyr)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(DT)
library(sf)
library(viridis)
library(htmltools)
library(htmlwidgets)
library(bslib)
library(thematic)
library(shinythemes)
library(shinyglide)
library(plotly)
library(ggplot2)
library(sass)
#library(shinyWidgets)
library(rgeos)
#library(anytime)
#library(shinymaterial)
#thematic::thematic_shiny(font = "auto")

#theme <- bslib::bs_theme(version = 5, bootswatch = "darkly")
source("global.R")
#mapbox <- Sys.getenv("MAPBOX_API_KEY")
attribution <- "© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> <strong><a"

ui <- fluidPage(
  #theme = bslib::bs_theme(version = 5, bootswatch = "slate"),
  theme = bslib::bs_add_rules(
    bslib::bs_theme(version = 5, bootswatch = "slate",
                    base_font = font_google("Nunito Sans"),
                    "h1-font-size" = "0.1rem",
                    "headings-font-weight" = "100 !default"
    ) |>
      bs_add_rules("h1 { text-transform: uppercase; letter-spacing: 1px;}"),
    sass::as_sass("table.dataTable tbody tr.active td {
             color: black !important;
             box-shadow: inset 0 0 0 9999px #87AECC !important;}"
    )
  ), #"#08F7FE" "#87AECC"
  # tags$head(
  #   tags$style(HTML("
  #     /* Customize the height of datatable rows */
  #     table.dataTable tbody th,
  #     table.dataTable tbody td {
  #       height: 5px; /* Adjust the height value as needed */
  #     }
  #   "))
  # ),
  titlePanel("Banned Book Directory"),
  
  tabsetPanel(
    id = "nav",
    tabPanel("Banned Books in US Schools",
             sidebarLayout(
               sidebarPanel(
                 plotlyOutput("barChart"),
                 plotlyOutput("plotlyOutput1")
               ),
               mainPanel(
                 leafletOutput("map1"),
                 actionButton("resetButton1", "Reset"),
                 span(actionButton("reopenButton", "Slideshow"),
                      style = "position:absolute;right:1em;"),
                 DT::dataTableOutput("table1")
               )
             )),
    tabPanel("Banned Books in US Prisons",
             sidebarLayout(
               sidebarPanel(
                 plotlyOutput("barChart2"),
                 div(
                   style = "height: 500px; overflow-y: scroll; overflow-x: scroll;",
                   plotlyOutput("plotlyOutput2")
                 )
               ),
               mainPanel(
                 leafletOutput("map2"),
                 actionButton("resetButton2", "Reset"),
                 span(actionButton("reopenButton2", "Slideshow"),
                      style = "position:absolute;right:1em;"),
                 DT::dataTableOutput("table2")
               )
             ))
  ),
  
  myModal <- modalDialog(
    id = "slideshowModal",
    title = htmlOutput("title"),
    footer = NULL,
    size = "xl",
    easyClose = TRUE,
    fade = TRUE,
    glide(
      id = "myglide",
      screen(
        column(
          width = 12,
          align = "center",
          div(style = "height: 100px;"),
          tags$img(src = "BANNED_BOOKS.jpg", style = "max-width: 100%; max-height: 100%;"),
          div(style = "height: 50px;"),
          tags$span(
            HTML("This project seeks to raise awareness about book banning in the US. It also specifically 
            focuses on what type of content is being banned and where from a local level by using a form of 
                 unsupervised machine learning (topic modeling)."),
            style = "font-size: 18px; text-align: center; padding: 20px 15px 10px; display: inline-block; color: white"
          )
        )
      ),
      screen(
        column(
          width = 12,
          align = "center",
          tags$img(src = "PEN.jpg", style = "max-width: 80%; max-height: 80%;"),
          tags$h4(
            HTML("<b>What is a school book ban and where does this data come from?</b>")
          ),
          tags$q(
            "PEN America defines a school book ban as any action taken against a book based on its content 
            and as a result of parent or community challenges, administrative decisions, or in response to 
            direct or threatened action by lawmakers or other governmental officials, that leads to a previously 
            accessible book being either completely removed from availability to students, or where access to a 
            book is restricted or diminished."
          ),
          style = "color:white",
          div(style = "height: 25px;"),
          tags$q(
            "PEN America records book bans through publicly available data on district or school websites, news 
            sources, Public Records Requests, and school board minutes. The data presented here is limited. The 
            true magnitude of book banning in the 2022-23 school year is unquestionably much higher."
          ),
          tags$ul(
            tags$li(
              style = "list-style-type: none;",
              tags$a(
                href = "https://pen.org/report/banned-in-the-usa-state-laws-supercharge-book-suppression-in-schools/",
                target = "_blank",
                HTML("https://pen.org/report/banned-in-the-usa-state-laws-supercharge-book-suppression-in-schools/"),
                style = "color: lightblue"
              )
            ),
            div(style = "height: 25px;"),
            tags$p("INDEX OF SCHOOL BOOK BANS"),
            tags$li(
              style = "list-style-type: none;",
              tags$a(
                href = "https://pen.org/banned-book-list-2021-2022/",
                target = "_blank",
                HTML("https://pen.org/banned-book-list-2021-2022/"),
                style = "color: lightblue"
              )
            ),
            tags$li(
              style = "list-style-type: none;",
              tags$a(
                href = "https://pen.org/index-of-school-book-bans-2022/",
                target = "_blank",
                HTML("https://pen.org/index-of-school-book-bans-2022/"),
                style = "color: lightblue"
              )
            )
          )
        )
      ),
      screen(
        column(
          width = 12,
          align = "center",
          div(style = "display: flex; justify-content: center; align-items: center;",
              tags$img(src = "content1.jpg", style = "max-width: 80%; max-height: 60%;"),
          ),
          tags$h4(
            HTML("<b>What type of content is being banned in schools?</b>"),
            style = "font-size: 20px; text-align: center; padding: 20px 15px 10px; display: inline-block; color:white"
          ),
          div(style = "display: flex; justify-content: center; align-items: center;",
              tags$img(src = "content2.jpg", style = "max-width: 80%; max-height: 60%;"),
          )
        )
      ),
      screen(
        column(
          width = 12,
          align = "center",
          tags$img(src = "seahorsebook.jpg", style = "max-width: 80%; max-height: 100%;"),
          tags$h4(
            HTML("<b>Who is behind the banning?</b>"),
            style = "color: white"
          ),
          tags$q(
            "PEN America has identified at least 50 groups involved in pushing for book bans 
            across the country operating at the national, state or local levels. Of those 50 
            groups, eight have local or regional chapters that, between them, number at least 
            300 in total;….Most of these groups (including chapters) appear to have formed 
            since 2021 (73 percent, or 262)."
          ),
          style = "color: white",
          div(style = "height: 25px;"),
          tags$q(
            "The nature of this movement is not one of isolated challenges to books by parents 
            in different communities; rather, it is an organized effort by advocacy groups and 
            state politicians with the ultimate aim of limiting access to certain stories, 
            perspectives, and information."
          ),
          div(style = "height: 25px;"),
          tags$q(
            "In Williamson County, Tennessee, a challenge from a local Moms For Liberty chapter 
            resulted in only one title banned from the curriculum, but six books were ascribed 
            “instructional adjustments.” These six books were not banned in their entirety–and 
            therefore are not listed in the Index–but wound up with restrictions placed on specific 
            pages. They can read Sea Horse: The Shyest Fish in the Sea by Chris Butterworth to students, 
            but they cannot display pages 12-13."
          ),
          tags$ul(
            tags$li(
              style = "list-style-type: none;",
              tags$a(
                href = "https://pen.org/banned-in-the-usa/",
                target = "_blank",
                HTML("https://pen.org/banned-in-the-usa/"),
                style = "color: lightblue"
              )
            )
          )
        )
      ),
      screen(
        column(
          width = 12,
          align = "center",
          tags$img(src = "school_bans.jpg", style = "max-width: 100%; max-height: 100%;"),
          tags$span(
            HTML("Between July 2021 and Dec. 2022, there were a total of:<br>",
                 "- 4009 individual book bans<br>",
                 "- 2261 unique titles<br>",
                 "- in 190 school districts<br>",
                 "- in 37 states"),
            div(style = "height: 25px;"),
            tags$span(
              HTML("As of June 30th, 2022, the amount of districts with bans (138 school districts in 32 states) 'represented
                   5,049 schools with a combined enrollment of nearly 4 million students.'")
            ),
            style = "font-size: 18px; text-align: center; padding: 20px 15px 10px; display: inline-block; color: white"
          )
        )
      ),
      screen(
        column(
          width = 12,
          align = "center",
          #plotlyOutput("plot"),
          tags$iframe(src = "books.html", width = "100%", height = "415px"),
          div(style = "height: 50px;"),
          plotlyOutput("plot2")
        )
      ),
      screen(
        column(
          width = 12,
          align = "center",
          tags$img(src = "marshall.jpg", style = "max-width: 100%; max-height: 100%;"),
          div(style = "height: 25px;"),
          tags$span(
            HTML("This project also takes a look at books and publications banned in US prisons.<br>",
                 "The Marshall Project is a nonprofit, online journalism organization that focuses on 
                 issues related to criminal justice in the United States.<br>",
                 "Thus far, they have put together a list of 55,278 books/publications that are banned in prisons from 19 states."),
            style = "color: white"
          ),
          tags$ul(
            tags$li(
              style = "list-style-type: none;",
              tags$a(
                href = "https://www.themarshallproject.org/2022/12/21/prison-banned-books-list-find-your-state",
                target = "_blank",
                HTML("https://www.themarshallproject.org/2022/12/21/prison-banned-books-list-find-your-state"),
                style = "color: lightblue"
              )
            ),
            div(style = "height: 25px;"),
            tags$p("Data Source:"),
            style = "color: white",
            tags$li(
              style = "list-style-type: none;",
              tags$a(
                href = "https://observablehq.com/@themarshallproject/prison-banned-books",
                target = "_blank",
                HTML("https://observablehq.com/@themarshallproject/prison-banned-books"),
                style = "color: lightblue"
              )
            )
          )
        )
      ),
      screen(
        column(
          width = 12,
          align = "center",
          tags$img(src = "bertopic.jpg", style = "max-width: 100%; max-height: 100%;"),
          tags$ul(
            tags$li(
              style = "list-style-type: none;",
              tags$a(
                href = "https://maartengr.github.io/BERTopic/index.html",
                target = "_blank",
                HTML("https://maartengr.github.io/BERTopic/index.html"),
                style = "color: lightblue"
              )
            ),
            div(style = "height: 25px;"),
            tags$span(
              HTML("Steps Taken:<br>",
                   "- Grabbed the individual book descriptions from the Google Books API<br>",
                   "- Fed the descriptions into BERTopic and extracted topics from them<br>",
                   "- Used the generated topics to gain insight by visualizing the geographic distribution of content banning"),
              style = "color:white"
            ),
            tags$div(
              style = "height: 25px;"
            ),
            tags$h5(
              HTML("Disclaimer:<br>"),
              tags$p(
                "Due to the nature of the project, there will be some degree of error in this analysis. The topic model is only as good as the data that it's fed. Sometimes the wrong book description is grabbed, which can affect the results. I've tried to ensure that most are correct, but due to the large number of titles, it's a very tedious process."
              ),
              style = "color:white"
            )
            
          )
        )
      ),
      
      screen(
        column(
          width = 12,
          align = "center",
          tags$img(src = "words.jpg", style = "max-width: 100%; max-height: 100%;"),
          tags$h1(
            style = "color: white; font-size: 24px; text-transform: none;",
            "Top Banned Topics in Prison"
          )
        )
      ),
      screen(
        column(
          width = 12,
          align = "center",
          div(style = "height: 100px;"),
          tags$iframe(src = "prison_topics_over_time.html", width = "80%", height = "500px", style = "margin-left: 50px;"),
          tags$h1(
            style = "color: white; font-size: 24px; text-transform: none;",
            "What's Trending in Prison Reading Material?"
          ),
        )
      )
    )
  )
)

server <- function(input, output) {
  #bs_themer()
  
  # Define reactive dfs from imported csv files
  schools <- reactive({
    schools_final
  })
  
  p <- reactive({
    p1
  })
  
  df <- reactive({
    df_prison
  })
  
  df2 <- reactive({
    df2_school
  })
  
  # Reset clickedState when the corresponding tab is not active
  observeEvent(input$nav, {
    if (input$nav != "Banned Books in US Schools") {
      click$clickedState <- NULL
    }
  })
  
  observeEvent(input$nav, {
    if (input$nav != "Banned Books in US Prisons") {
      click$clickedState <- NULL
    }
  })
  
  # Show info modal with button click
  observeEvent(input$reopenButton, {
    showModal(myModal)
  })
  
  output$plot2 <- renderPlotly({
    # Get the top ten authors by count
    top_authors <- schools()$Author %>%
      table() %>%
      sort(decreasing = TRUE) %>%
      head(10) %>%
      names()
    
    # Filter the dataframe to include only the top authors
    filtered_schools <- schools() %>%
      filter(Author %in% top_authors)
    
    # Calculate the total number of mentions, unique titles, and unique states per author
    author_counts <- filtered_schools %>%
      group_by(Author) %>%
      summarise(Total_Mentions = n(),
                Unique_States = n_distinct(State),
                Unique_Titles = n_distinct(Title)) %>%
      arrange(desc(Total_Mentions))
    
    # Custom margin for title
    mrg <- list(l = 50, r = 50,
                b = 50, t = 50)
    
    # Create the grouped bar chart using Plotly
    plot_ly(author_counts, x = ~reorder(Author, -Total_Mentions), y = ~Total_Mentions) %>%
      add_trace(name = "Total Bans", type = "bar", 
                marker = list(color = "#8CDED9FF", 
                              line = list(color = '#08F7FE', 
                                          width = 1))) %>%
      add_trace(y = ~Unique_States, name = "Total States", type = "bar", 
                marker = list(color = "#FF4E50", 
                              line = list(color = '#FF4E50', 
                                          width = 1))) %>%
      add_trace(y = ~Unique_Titles, name = "Unique Titles", type = "bar", 
                marker = list(color = "#FFA500", 
                              line = list(color = '#FFC900', 
                                          width = 1))) %>%
      
      layout(barmode = "group", font = list(color = '#FFFFFF'),
             xaxis = list(title = "Author", tickangle = 45, tickfont = list(size = 11)),
             yaxis = list(title = "Count"),
             paper_bgcolor = "#212946",
             plot_bgcolor = "#212946",
             margin = mrg,
             hovermode = "x",
             title = "Top Ten Authors by Individual Bans, Unique Titles, and Total States")
  })
  
  
  
  click <- reactiveValues(clickedState = NULL)
  # Choropleth map for banned school books
  output$map1 <- renderLeaflet({
    leaflet(df_sf2) %>%
      addTiles(mapbox, attribution) %>%
      addPolygons(
        fillColor = ~pal2(df2()$State_Count),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels2,
        layerId = as.character(df_sf2$State),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>% 
      addLegend(
        pal = pal2,
        values = ~schools()$State_Count,
        opacity = 0.9,
        title = "Bans",
        position = "bottomright"
      ) %>% setView(lat = 38.3283, lng = -98.5795, zoom = 4) %>% 
      addResetMapButton()
  })
  
  originalData1 <- reactiveVal()
  
  filteredData1 <- reactiveVal()
  
  # Update the original filtered data when the app starts
  observe({
    originalData1(schools()[, c(
      "State", "State_Count", "Title", "Author", "description", "Topic", "published_date", "publisher", "categories",
      "Type.of.Ban", "District", "Date.of.Challenge.Removal",  
      "Origin.of.Challenge")
    ])
  })
  
  # Update the filtered data based on the selected state in the map
  observe({
    clickedState <- input$map1_shape_click$id
    
    if (is.null(clickedState)) {
      # No state selected, show the original data
      filteredData1(originalData1())
    } else {
      # Filter datatable based on selected state in the map
      filteredData1(originalData1()[originalData1()$State %in% clickedState, c(
        "State", "State_Count", "Title", "Author", "description", "Topic", "published_date", "publisher", "categories",
        "Type.of.Ban", "District", "Date.of.Challenge.Removal",  
        "Origin.of.Challenge")
      ])
    }
  })
  
  # Render the datatable for banned school books
  output$table1 <- DT::renderDataTable({
    data <- filteredData1()
    
    colnames(data) <- c(
      "State", "Total Bans", "Title", "Author", "Description", "Topic", "Publication Date", "Publisher", "Category",
      "Type of Ban", "District", "Date Banned", 
      "Origin of Challenge"
    )
    DT::datatable(
      cbind(' ' = '&oplus;', data), escape = -2,
      options = list(scrollX = TRUE, scrollY = "300px",
                     columnDefs = list(
                       list(visible = FALSE, targets = 6),
                       list(orderable = FALSE, className = 'details-control', targets = 1)
                     )
      ), # Places the description in a hidden/expandable row
      callback = JS(" 
  table.column(6).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:black; padding: .5em;\"> Description: ' +
            d[6] + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
      ))
  })
  
  # Reset datatable when the "Reset" button is clicked
  observeEvent(input$resetButton1, {
    click$clickedState <- NULL
    filteredData1(originalData1())
  })
  
  observeEvent(input$map1_shape_click$id,{
    click$clickedState <- input$map1_shape_click$id
  })
  
  # Render the sidebar with the plotly bar chart
  output$barChart <- renderPlotly({
    #clickedState <- input$map1_shape_click$id
    
    if (is.null(click$clickedState)) {
      # Count the occurrences of each state
      counts <- table(schools()$District)
      
      # Convert to df
      counts_df <- as.data.frame(counts, stringsAsFactors = FALSE)
      colnames(counts_df) <- c("District", "Count")
      
      # Sort the data frame by count in descending order
      counts_df <- counts_df[order(-counts_df$Count), ]
      
      # Select the top ten states
      top_ten <- head(counts_df, 10)
      
      # Update the district names in top_ten using gsub
      top_ten$District <- gsub("Central York School District", "Central York School District (PA)", top_ten$District)
      top_ten$District <- gsub("North East Independent School District", "North East Independent School District (TX)", top_ten$District)
      top_ten$District <- gsub("Collierville Schools", "Collierville Schools (TN)", top_ten$District)
      top_ten$District <- gsub("Frisco Independent School District", "Frisco Independent School District (TX)", top_ten$District)
      top_ten$District <- gsub("Wentzville School District", "Wentzville School District (MO)", top_ten$District)
      top_ten$District <- gsub("Duval County Public Schools", "Duval County Public Schools (FL)", top_ten$District)
      top_ten$District <- gsub("Indian River County School District", "Indian River County School District (FL)", top_ten$District)
      top_ten$District <- gsub("Escambia County Public Schools", "Escambia County Public Schools (FL)", top_ten$District)
      top_ten$District <- gsub("Granbury Independent School District", "Granbury Independent School District (TX)", top_ten$District)
      top_ten$District <- gsub("Beaufort County School District", "Beaufort County School District (SC)", top_ten$District)
      
      # Create the horizontal bar chart using plot_ly
      plot_ly(data = top_ten, y = ~District, x = ~Count, type = "bar", orientation = "h", #%>% 
              marker = list(color = '#00FFFB')) %>%  
        #line = list(color = '#00FFFB', width = 1))) %>%
        #textfont = list(color = '#FFFFFF')) %>%  #4D7593 #143E5C
        layout(paper_bgcolor='#272b30', plot_bgcolor ='#272b30', font = list(color = '#FFFFFF'),
               yaxis = list(title = "", categoryorder = "total ascending"),
               xaxis = list(title = "", gridcolor = 'rgba(255, 255, 255, 0.5)'),
               showlegend = FALSE,
               title = "Districts With Most Book Bans",
               margin = mrg,
               hoverlabel = list(font = list(color = '#000'))
        )
    } else if (!is.null(click$clickedState)) {
      filteredData <- schools()[schools()$State %in% click$clickedState, ]
      
      # Calculate the count for each district
      counts <- table(filteredData$District)
      
      # Convert counts table to a data frame
      counts_df <- as.data.frame(counts, stringsAsFactors = FALSE)
      colnames(counts_df) <- c("District", "Count")
      
      # Order the data based on the count in descending order
      counts_df <- counts_df[order(-counts_df$Count), ]
      
      # Choose number of districts to show
      if (nrow(counts_df) > 16) {
        counts_df <- counts_df[1:16, ]
      }
      
      # Get the state name from the first row of filtered data
      stateName <- unique(filteredData$State)[1]
      
      # Custom margin for title
      mrg <- list(l = 50, r = 50,
                  b = 50, t = 50)
      
      # Horizontal bar chart
      plot_ly(data = counts_df, y = ~District, x = ~Count, type = "bar", orientation = 'h',
              marker = list(color = '#00FFFB')) %>% 
        #line = list(color = '#FF00FF', width = 1))) %>%
        #textfont = list(color = '#FFFFFF')) %>%
        layout(paper_bgcolor='#272b30', plot_bgcolor ='#272b30', font = list(color = '#FFFFFF'),
               xaxis = list(title = "Count", gridcolor = 'rgba(255, 255, 255, 0.5)'),
               yaxis = list(title = "", categoryorder = "total ascending"),
               title = paste("Number of Books Banned by",stateName, "District"),
               margin = mrg,
               showlegend = FALSE,
               hoverlabel = list(font = list(color = '#000'))
        )
    }
  })
  
  # Render the sidebar with the plotly top ten topics chart
  output$plotlyOutput1 <- renderPlotly({
    #clickedState <- input$map1_shape_click$id
    
    if (is.null(click$clickedState)) {
      
      top_ten_topics <- table(schools()$Topic)
      
      # Convert to df
      counts_df2 <- as.data.frame(top_ten_topics, stringsAsFactors = FALSE)
      colnames(counts_df2) <- c("Topic", "Count")
      
      # Sort in descending order
      counts_df2 <- counts_df2[order(-counts_df2$Count), ]
      
      # Select top ten topics
      top_ten2 <- head(counts_df2, 10)
      
      plot_ly(data = top_ten2, x = ~Count, y = ~Topic, type = "bar", #%>% 
              marker = list(color = '#FFA500')) %>%  #2C5985FF
        #line = list(color = '#FFA500', width = 1))) %>% 
        #textfont = list(color = '#FFFFFF')) %>%
        layout(paper_bgcolor='#272b30', plot_bgcolor ='#272b30', font = list(color = '#FFFFFF'),
               xaxis = list(title = "", gridcolor = 'rgba(255, 255, 255, 0.5)'),
               yaxis = list(title = "", categoryorder = "total ascending"),
               showlegend = FALSE,
               title = "Top Identified Topics in Schools",
               margin = mrg,
               hoverlabel = list(font = list(color = '#000000'))
        )
    } else {
      # sort by click
      filteredData <- schools()[schools()$State == click$clickedState, ]
      top_ten_topics <- table(filteredData$Topic)
      
      
      counts_df2 <- as.data.frame(top_ten_topics, stringsAsFactors = FALSE)
      colnames(counts_df2) <- c("Topic", "Count")
      
      counts_df2 <- counts_df2[order(-counts_df2$Count), ]
      
      top_ten2 <- head(counts_df2, 10)
      
      # Get the state name from the first row of filtered data
      stateName <- unique(filteredData$State)[1]
      
      plot_ly(data = top_ten2, x = ~Count, y = ~Topic, type = "bar",
              marker = list(color = '#FFA500')) %>% #2C5985FF
        #line = list(color = '#FFA500', width = 1))) %>% 
        #textfont = list(color = '#FFFFFF')) %>%
        layout(paper_bgcolor='#272b30', plot_bgcolor ='#272b30', font = list(color = '#FFFFFF'),
               xaxis = list(title = "", gridcolor = 'rgba(255, 255, 255, 0.5)'),
               yaxis = list(title = "", categoryorder = "total ascending"),
               title = paste("Identified Topics Banned in",stateName, "Schools"),
               margin = mrg,
               showlegend = FALSE,
               hoverlabel = list(font = list(color = '#000'))
        )
    }
  })
  
  # Render the choropleth map for prison books
  output$map2 <- renderLeaflet({
    leaflet(df_sf) %>%
      addTiles(mapbox, attribution) %>%
      addPolygons(
        fillColor = ~pal(df()$State_Count),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.9,
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        layerId = as.character(df_sf$state_arc),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~p()$State_Count,
        opacity = 0.9,
        title = "Bans",
        position = "bottomright"
      ) %>%
      setView(lat = 38.3283, lng = -98.5795, zoom = 4) %>% 
      addResetMapButton() 
  })
  
  # Reactive value to store the original filtered data
  originalData2 <- reactiveVal()
  
  # Reactive value to store the filtered data
  filteredData <- reactiveVal()
  
  # Update the original filtered data when the app starts
  observe({
    originalData2(p()[, c(
      "state_arc", "State_Count", "publication", "topic_description",
      "author", "Name", "date", "reason", "publication_date",
      "publisher", "categories"
    )])
  })
  
  # Update the filtered data based on the selected state in the map
  observe({
    clickedState <- input$map2_shape_click$id
    
    if (is.null(clickedState)) {
      # No state selected, show the original data
      filteredData(originalData2())
    } else {
      # Filter datatable based on selected state in the map
      filteredData(originalData2()[originalData2()$state_arc %in% clickedState, c(
        "state_arc", "State_Count", "publication", "topic_description",
        "author", "Name", "date", "reason", "publication_date",
        "publisher", "categories"
      )])
    }
  })
  
  # Datatable for prison books
  output$table2 <- DT::renderDataTable({
    data <- filteredData()
    
    colnames(data) <- c(
      "State", "Total Banned Books", "Title", "Description", "Author",
      "Topic", "Date Banned", "Reason For Ban",
      "Publication Date", "Publisher", "Category"
    )
    # Check to see if description field is populated and adds blank column if so
    data$' ' <- ifelse(!is.na(data$Description), "&oplus;", "")
    
    DT::datatable(
      data[, c(' ', names(data))], escape = -2,
      options = list(scrollX = TRUE, scrollY = "300px",
                     columnDefs = list(
                       list(visible = FALSE, targets = c(5,13)),
                       list(orderable = FALSE, className = 'details-control', targets = 1)
                     ) 
      ), # hides description in hidden row that can be expanded
      callback = JS("
  table.column(4).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:black; padding: .5em;\"> Description: ' +
            d[5] + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
      ))
  })
  
  # Show info modal with button click
  observeEvent(input$reopenButton2, {
    showModal(myModal)
  })
  #values <- reactiveValues(chart = NULL)
  # Reset datatable when the "Reset" button is clicked
  observeEvent(input$resetButton2, {
    click$clickedState <- NULL
    filteredData(originalData2())
    #output$barChart2 <- renderPlotly({m})
  })
  
  observeEvent(input$map2_shape_click$id,{
    click$clickedState <- input$map2_shape_click$id
  })
  
  # Render the sidebar with the plotly bar chart
  output$barChart2 <- renderPlotly({
    #click$clickedState <- input$map2_shape_click$id
    
    if (is.null(click$clickedState)) {
      # Show state_counts over the entire dataset
      counts3 <- table(p()$state_arc, format(as.Date(p()$date), "%Y"))
      counts_df3 <- as.data.frame(counts3)
      state_counts <- counts_df3[counts_df3$Var1 %in% unique(p()$state_arc), ]
      
      if (nrow(state_counts) == 0) {
        # No dates available for any state
        plot_ly(x = NA, y = NA) %>% hide_colorbar() %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = ""),
            showlegend = TRUE,
            annotations = list(
              text = "No data available",
              xref = "paper",  
              yref = "paper",
              x = 0.5,
              y = 0.5,
              showarrow = FALSE,
              font = list(size = 20)
            )
          )
      } else {
        
        total_counts <- aggregate(Freq ~ Var2, data = state_counts, FUN = sum)
        
        plot_ly(data = total_counts, x = ~Var2, y = ~Freq, type = "scatter",
                mode = "lines", fill = "tozeroy", fillcolor = 'rgba(0, 255, 251, 0.1)',
                line = list(color = '#00FFFB', width = 2)) %>%
          layout(paper_bgcolor='#272b30', plot_bgcolor ='#272b30', font = list(color = '#FFF'),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Total Bans"),
                 showlegend = FALSE,
                 title = 'Prison Ban Timeline',
                 margin = mrg,
                 hoverlabel = list(font = list(color = '#000'))
          )
        
      }
    } else {
      #clickedState <- click$clickedState
      # Filtered data for the selected state
      filteredData <- p()[p()$state_arc %in% click$clickedState, ]
      
      # Extract year from the "date" column
      filteredData$Year <- format(as.Date(filteredData$date, format = "%Y-%m-%d"), "%Y")
      
      # Check if the state is Iowa and modify the date format
      if (click$clickedState == "IA") {
        filteredData$Year <- substr(filteredData$date, 1, 4)
      }
      
      # Count the occurrences of each year per state
      counts3 <- table(filteredData$state_arc, filteredData$Year)
      
      # Convert counts table to a data frame
      counts_df3 <- as.data.frame(counts3)
      
      # Filter the data frame for the selected state
      state_counts <- subset(counts_df3, Var1 == click$clickedState)
      
      if (nrow(state_counts) == 0) {
        # No dates available for the selected state
        plot_ly(x = NA, y = NA) %>% hide_colorbar() %>% 
          #marker = list(color = '#000000', 
          #line = list(color = '#000000', width = 1)),
          #textfont = list(color = '#FFFFFF')) %>%
          layout(paper_bgcolor='#272b30', plot_bgcolor ='#272b30', font = list(color = '#FFF'),
                 xaxis = list(title = ""),
                 yaxis = list(title = ""),
                 showlegend = TRUE,
                 annotations = list(
                   text = "No data available",
                   xref = "paper",  # Set the x-coordinate reference to the plot's coordinate system
                   yref = "paper",
                   x = 0.5,
                   y = 0.5,
                   showarrow = FALSE,
                   font = list(size = 20)
                 )
          )
      } else {
        stateName <- unique(filteredData$state_arc)[1]
        # Create the bar chart using plot_ly
        plot_ly(data = state_counts, x = ~Var2, y = ~Freq, type = "scatter",
                mode = "lines", fill = "tozeroy", fillcolor = 'rgba(0, 255, 251, 0.1)',
                line = list(color = '#00FFFB', width = 2)) %>%
          #line = list(color = '#000000', width = 1)),
          #textfont = list(color = '#FFFFFF')) %>%
          layout(paper_bgcolor='#272b30', plot_bgcolor ='#272b30', font = list(color = '#FFF'),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Total Bans"),
                 title = paste(stateName,"Prison Ban Timeline"),
                 margin = mrg,
                 showlegend = FALSE
          ) 
      }
    }
  })
  
  # Render the sidebar with the plotly top ten topics chart
  output$plotlyOutput2 <- renderPlotly({
    #clickedState <- input$map2_shape_click$id
    
    if (is.null(click$clickedState)) {
      #if (is.null(values$chart)) {  
      top_ten_topics <- table(p()$Name)
      
      # Convert to df
      counts_df4 <- as.data.frame(top_ten_topics, stringsAsFactors = FALSE)
      colnames(counts_df4) <- c("Topic", "Count")
      
      # Sort in descending order
      counts_df4 <- counts_df4[order(-counts_df4$Count), ]
      
      plot_ly(data = counts_df4, x = ~Count, y = ~Topic, type = "bar", 
              marker = list(color = '#FFA500')) %>%
        layout(
          paper_bgcolor = '#272b30',
          plot_bgcolor = '#272b30', font = list(color = '#FFF'),
          xaxis = list(title = "", gridcolor = 'grey', side = 'top'),
          yaxis = list(title = "", categoryorder = "total ascending"),
          title = 'Identified Topics in Prison',
          margin = mrg,
          showlegend = FALSE,
          height = 3600,  # Set a fixed height
          margin = mrg,  # Adjust margins for better display
          autosize = FALSE,  # Disable auto-sizing
          config = list(scrollZoom = FALSE),  # Disable auto-sizing
          hoverlabel = list(font = list(color = '#000'))
          )
      #}
      #values$chart
      
    } else {
      # sort by click
      filteredData <- p()[p()$state_arc == click$clickedState, ]
      top_ten_topics <- table(filteredData$Name)
      
      counts_df4 <- as.data.frame(top_ten_topics, stringsAsFactors = FALSE)
      colnames(counts_df4) <- c("Topic", "Count")
      
      counts_df4 <- counts_df4[order(-counts_df4$Count), ]
      
      stateName <- unique(filteredData$state_arc)[1]
      
      totalTopics <- sum(counts_df4$Count)
      
      height <- ifelse(totalTopics <= 100, 500, ifelse(totalTopics <= 500, 1800, 3600))  # Set height based on condition
      
      plot_ly(data = counts_df4, x = ~Count, y = ~Topic, type = "bar",
              marker = list(color = '#FFA500')) %>%
        layout(
          paper_bgcolor = '#272b30',
          plot_bgcolor = '#272b30', font = list(color = '#FFF'),
          xaxis = list(title = "", gridcolor = 'grey', side = 'top'),
          yaxis = list(title = "", categoryorder = "total ascending"),
          title = paste("Identified Topics Banned in", stateName, "Prisons"),
          showlegend = FALSE,
          height = height,  # Set a fixed height
          margin = mrg, #list(l = 50, r = 50, b = 100, t = 50),  # Adjust margins for better display
          autosize = FALSE,  # Disable auto-sizing
          config = list(scrollZoom = FALSE)  # Disable auto-sizing
        )
      
    }
  })
  
}

shinyApp(ui, server)
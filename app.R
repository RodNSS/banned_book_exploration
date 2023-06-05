library(shiny)
library(leaflet)
library(DT)
library(sf)
library(viridis)
library(htmltools)
library(htmlwidgets)
library(bslib)
library(thematic)
library(shinythemes)
library(anytime)
#thematic::thematic_shiny(font = "auto")

#theme <- bslib::bs_theme(version = 5, bootswatch = "darkly")

#click <- reactiveValues(clickedState = NULL)

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
             box-shadow: inset 0 0 0 9999px orange !important;}"
    )),
  # tags$head(
  #   tags$style(HTML("
  #     /* Customize the height of datatable rows */
  #     table.dataTable tbody th,
  #     table.dataTable tbody td {
  #       height: 5px; /* Adjust the height value as needed */
  #     }
  #   "))
  # ),
  titlePanel("Banned Books"),
  
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
                 DT::dataTableOutput("table2")
               )
             ))
  )
)


server <- function(input, output) {
  bs_themer()
  
  click <- reactiveValues(clickedState = NULL)
  # Choropleth map for banned school books
  output$map1 <- renderLeaflet({
    leaflet(df_sf2) %>%
      addTiles(mapbox) %>%
      addPolygons(
        fillColor = ~pal2(df2$State_Count),
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
        values = ~schools$State_Count,
        opacity = 0.9,
        title = "Bans",
        position = "bottomright"
      ) %>% setView(lat = 38.3283, lng = -98.5795, zoom = 4)
  })
  
  originalData1 <- reactiveVal()
  
  filteredData1 <- reactiveVal()
  
  # Update the original filtered data when the app starts
  observe({
    originalData1(schools[, c(
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
    return '<div style=\"background-color:black; padding: .5em;\"> Topic: ' +
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
      counts <- table(schools$State)
      
      # Convert to df
      counts_df <- as.data.frame(counts, stringsAsFactors = FALSE)
      colnames(counts_df) <- c("State", "Count")
      
      # Sort the data frame by count in descending order
      counts_df <- counts_df[order(-counts_df$Count), ]
      
      # Select the top ten states
      top_ten <- head(counts_df, 10)
      
      # Create the horizontal bar chart using plot_ly
      plot_ly(data = top_ten, y = ~State, x = ~Count, type = "bar", orientation = "h") %>%
        #marker = list(color = '#000000', 
        #line = list(color = '#000000', width = 1)),
        #textfont = list(color = '#FFFFFF')) %>%
        layout(paper_bgcolor='#7a8288', plot_bgcolor ='#7a8288', #font = list(color = '#000000'),
               yaxis = list(title = "State", categoryorder = "total ascending"),
               xaxis = list(title = "Count"),
               showlegend = FALSE
        )
    } else if (!is.null(click$clickedState)) {
      filteredData <- schools[schools$State %in% click$clickedState, ]
      
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
      plot_ly(data = counts_df, y = ~District, x = ~Count, type = "bar", orientation = 'h') %>%
        #marker = list(color = '#000000', 
        #line = list(color = '#000000', width = 1)),
        #textfont = list(color = '#FFFFFF')) %>%
        layout(paper_bgcolor='#7a8288', plot_bgcolor ='#7a8288', #font = list(color = '#000000'),
               xaxis = list(title = "Count"),
               yaxis = list(title = "", categoryorder = "total ascending"),
               title = paste("Banned Books by",stateName, "District"),
               
               margin = mrg,
               showlegend = FALSE
        )
    }
  })
  
  # Render the sidebar with the plotly top ten topics chart
  output$plotlyOutput1 <- renderPlotly({
    #clickedState <- input$map1_shape_click$id
    
    if (is.null(click$clickedState)) {
      
      top_ten_topics <- table(schools$Topic)
      
      # Convert to df
      counts_df2 <- as.data.frame(top_ten_topics, stringsAsFactors = FALSE)
      colnames(counts_df2) <- c("Topic", "Count")
      
      # Sort in descending order
      counts_df2 <- counts_df2[order(-counts_df2$Count), ]
      
      # Select top ten topics
      top_ten2 <- head(counts_df2, 10)
      
      plot_ly(data = top_ten2, x = ~Count, y = ~Topic, type = "bar") %>%
        #marker = list(color = '#000000', 
        #line = list(color = '#000000', width = 1)),
        #textfont = list(color = '#FFFFFF')) %>%
        layout(paper_bgcolor='#7a8288', plot_bgcolor ='#7a8288', #font = list(color = '#000000'),
               xaxis = list(title = "Count"),
               yaxis = list(title = "Topic",categoryorder = "total ascending"),
               showlegend = FALSE
        )
    } else {
      # sort by click
      filteredData <- schools[schools$State == click$clickedState, ]
      top_ten_topics <- table(filteredData$Topic)
      
      
      counts_df2 <- as.data.frame(top_ten_topics, stringsAsFactors = FALSE)
      colnames(counts_df2) <- c("Topic", "Count")
      
      counts_df2 <- counts_df2[order(-counts_df2$Count), ]
      
      top_ten2 <- head(counts_df2, 10)
      
      plot_ly(data = top_ten2, x = ~Count, y = ~Topic, type = "bar") %>% 
        #marker = list(color = '#000000', 
        #line = list(color = '#000000', width = 1)),
        #textfont = list(color = '#FFFFFF')) %>%
        layout(paper_bgcolor='#7a8288', plot_bgcolor ='#7a8288', #font = list(color = '#000000'),
               xaxis = list(title = "Count"),
               yaxis = list(title = "Topic", categoryorder = "total ascending"),
               showlegend = FALSE
        )
    }
  })
  
  # Render the choropleth map for prison books
  output$map2 <- renderLeaflet({
    leaflet(df_sf) %>%
      addTiles(mapbox) %>%
      addPolygons(
        fillColor = ~pal(df$State_Count),
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
        values = ~p$State_Count,
        opacity = 0.9,
        title = "Bans",
        position = "bottomright"
      ) %>%
      setView(lat = 38.3283, lng = -98.5795, zoom = 4)
  })
  
  # Reactive value to store the original filtered data
  originalData2 <- reactiveVal()
  
  # Reactive value to store the filtered data
  filteredData <- reactiveVal()
  
  # Update the original filtered data when the app starts
  observe({
    originalData2(p[, c(
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
    return '<div style=\"background-color:black; padding: .5em;\"> Topic: ' +
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
      counts <- table(p$state_arc, format(as.Date(p$date), "%Y"))
      counts_df <- as.data.frame(counts)
      state_counts <- counts_df[counts_df$Var1 %in% unique(p$state_arc), ]
      
      if (nrow(state_counts) == 0) {
        # No dates available for any state
        plot_ly(x = NA, y = NA) %>% hide_colorbar() %>%
          layout(
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
        
        total_counts <- aggregate(Freq ~ Var2, data = state_counts, FUN = sum)
        # Create the bar chart using plot_ly
        plot_ly(data = total_counts, x = ~Var2, y = ~Freq, type = "bar") %>%
          #marker = list(color = '#000000', 
          #line = list(color = '#000000', width = 1)),
          #textfont = list(color = '#FFFFFF')) %>%
          layout(paper_bgcolor='#7a8288', plot_bgcolor ='#7a8288', #font = list(color = '#000000'),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Count"),
                 showlegend = FALSE
          ) 
      }
    } else {
      #clickedState <- click$clickedState
      # Filtered data for the selected state
      filteredData <- p[p$state_arc %in% click$clickedState, ]
      
      # Extract year from the "date" column
      filteredData$Year <- format(as.Date(filteredData$date, format = "%Y-%m-%d"), "%Y")
      
      # Check if the state is Iowa and modify the date format
      if (click$clickedState == "IA") {
        filteredData$Year <- substr(filteredData$date, 1, 4)
      }
      
      # Count the occurrences of each year per state
      counts <- table(filteredData$state_arc, filteredData$Year)
      
      # Convert counts table to a data frame
      counts_df <- as.data.frame(counts)
      
      # Filter the data frame for the selected state
      state_counts <- subset(counts_df, Var1 == click$clickedState)
      
      if (nrow(state_counts) == 0) {
        # No dates available for the selected state
        plot_ly(x = NA, y = NA) %>% hide_colorbar() %>% 
          #marker = list(color = '#000000', 
          #line = list(color = '#000000', width = 1)),
          #textfont = list(color = '#FFFFFF')) %>%
          layout(paper_bgcolor='#7a8288', plot_bgcolor ='#7a8288', #font = list(color = '#000000'),
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
        # Create the bar chart using plot_ly
        plot_ly(data = state_counts, x = ~Var2, y = ~Freq, type = "bar") %>%
          #marker = list(color = '#000000', 
          #line = list(color = '#000000', width = 1)),
          #textfont = list(color = '#FFFFFF')) %>%
          layout(paper_bgcolor='#7a8288', plot_bgcolor ='#7a8288', #font = list(color = '#000000'),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Count"),
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
      top_ten_topics <- table(p$Name)
      
      # Convert to df
      counts_df2 <- as.data.frame(top_ten_topics, stringsAsFactors = FALSE)
      colnames(counts_df2) <- c("Topic", "Count")
      
      # Sort in descending order
      counts_df2 <- counts_df2[order(-counts_df2$Count), ]
      
      plot_ly(data = counts_df2, x = ~Count, y = ~Topic, type = "bar") %>%
        layout(
          paper_bgcolor = '#7a8288',
          plot_bgcolor = '#7a8288',
          xaxis = list(title = "Count"),
          yaxis = list(title = "Topic", categoryorder = "total ascending"),
          showlegend = FALSE,
          height = 3600,  # Set a fixed height
          margin = list(l = 50, r = 50, b = 100, t = 50),  # Adjust margins for better display
          autosize = FALSE,  # Disable auto-sizing
          config = list(scrollZoom = FALSE)  # Disable auto-sizing
        )
      #}
      #values$chart
      
    } else {
      # sort by click
      filteredData <- p[p$state_arc == click$clickedState, ]
      top_ten_topics <- table(filteredData$Name)
      
      counts_df2 <- as.data.frame(top_ten_topics, stringsAsFactors = FALSE)
      colnames(counts_df2) <- c("Topic", "Count")
      
      counts_df2 <- counts_df2[order(-counts_df2$Count), ]
      
      plot_ly(data = counts_df2, x = ~Count, y = ~Topic, type = "bar") %>%
        layout(
          paper_bgcolor = '#7a8288',
          plot_bgcolor = '#7a8288',
          xaxis = list(title = "Count"),
          yaxis = list(title = "Topic", categoryorder = "total ascending"),
          showlegend = FALSE,
          height = 3600,  # Set a fixed height
          margin = list(l = 50, r = 50, b = 100, t = 50),  # Adjust margins for better display
          autosize = FALSE,  # Disable auto-sizing
          config = list(scrollZoom = FALSE)  # Disable auto-sizing
        )
      
    }
  })
  
}

shinyApp(ui, server)
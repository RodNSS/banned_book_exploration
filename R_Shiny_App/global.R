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
library(rgeos)

p1 <- read_csv("prison_without_index2.csv")
df_prison <- read.csv("p.csv")
df2_school <- read_csv("school_map.csv")
schools_final <- read.csv("school_final2.csv")

mapbox <- Sys.getenv("MAPBOX_API_KEY")
attribution <- "© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> <strong><a"

# Noticed some of the labels were too long for plotting so I shortened them.
p1$Name <- gsub("good press_world literature_originally published_digicat publishing", "good press_world literature", p1$Name)

p1$Name <- gsub("uncle john_john bathroom_national geographic_bathroom readers", "uncle john_bathroom readers", p1$Name)

p1$Name <- gsub("spectrum series_artists_year review_invaluable resource", "spectrum series_artists", p1$Name)

p1$Name <- gsub("magic mushrooms_mescaline_psychedelic drugs_mind", "magic mushrooms_mescaline_psychedelic drugs", p1$Name)

p1$Name <- gsub("walking dead_zombie apocalypse_robert kirkman_rick grimes", "walking dead_zombie apocalypse", p1$Name)

p1$Name <- gsub("clint adams_million gunsmith_books print_salt springs", "clint adams_million gunsmith_salt springs", p1$Name)

schools_final$Topic <- gsub("weir_won easy funny_navigate trials elementary_laughing weir", "weir_laughing weir", schools_final$Topic)

# convert the geometry column from prison df to an sf object
df_sf <- st_as_sf(df_prison, wkt = "geometry")

# Define palette for prison map
values <- c(4632, 1424, 1659, 20202, 28, 431, 134, 99, 974, 6902, 374, 600, 232, 1780, 68, 1509, 9396, 2553, 2281)

# pal <- colorNumeric(
#   palette = c("#FFC685FF", "#F8B059FF", "#F59839FF", "#F07D26FF", "#E46520FF", "#CD5622FF", "#B54A23FF", "#9E3D22FF"),#viridis(8),
#   domain = values
# )

pal <- colorNumeric(
  palette = c("#F9FFAFFF", "#C3EBA6FF", "#90D4A0FF", "#5EBC9DFF", "#2EA399FF", "#048893FF", "#166C88FF", "#2E4F79FF"),#viridis(8),
  domain = values
)

# Define labels for prison map
labels <- sprintf(
  "<strong>%s</strong><br/>%g total bans",
  df_prison$state_arc, df_prison$State_Count
) %>% lapply(htmltools::HTML)

# convert the geometry column from school df to an sf object
df_sf2 <- st_as_sf(df2_school, wkt = "geometry")

# Define the custom bins and corresponding colors
bins <- c(1, 10, 25, 50, 200, 400, 900, 1200, 1250)
#colors <- c("#FFC685FF","#F8B059FF", "#F59839FF", "#F07D26FF", "#E46520FF", "#CD5622FF", "#B54A23FF", "#9E3D22FF")
colors <- c("#F9FFAFFF", "#C3EBA6FF", "#90D4A0FF", "#5EBC9DFF", "#2EA399FF", "#048893FF", "#166C88FF", "#2E4F79FF") 
#colors <- c("#ECD999FF", "#EDC97EFF", "#EFB865FF", "#F2A552FF", "#F49049FF", "#F57A4AFF", "#F56055FF", "#EF4868FF")

# Create a color palette with custom bins
pal2 <- colorBin(
  palette = colors,
  domain = values,
  bins = bins
)

# Create a new column for labels based on State_Count value
df2_school$Label <- ifelse(df2_school$State_Count == 1,
                    paste0("<strong>", df2_school$State, "</strong><br/>", df2_school$State_Count, " book banned<br/>", df2_school$Title),
                    paste0("<strong>", df2_school$State, "</strong><br/>", df2_school$State_Count, " total bans"))

# Define labels for school map
labels2 <- sprintf("%s", df2_school$Label) %>% lapply(htmltools::HTML)

# Custom margin for plot titles
mrg <- list(l = 50, r = 50,
            b = 50, t = 50)

# Slideshow
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
        height = "50%",
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
        tags$img(src = "seahorse.jpg", style = "max-width: 80%; max-height: 100%;"),
        div(style = "height: 25px;"),
        tags$h4(
          HTML("<b>Who is behind the banning?</b>"),
          style = "color: white"
        ),
        div(style = "height: 25px;"),
        tags$q(
          "PEN America has identified at least 50 groups involved in pushing for book bans
            across the country operating at the national, state or local levels. Of those 50
            groups, eight have local or regional chapters that, between them, number at least
            300 in total;….Most of these groups (including chapters) appear to have formed
            since 2021 (73 percent, or 262)."
        ),
        style = "color: white",
        div(style = "height: 50px;"),
        tags$q(
          "The nature of this movement is not one of isolated challenges to books by parents
            in different communities; rather, it is an organized effort by advocacy groups and
            state politicians with the ultimate aim of limiting access to certain stories,
            perspectives, and information."
        ),
        div(style = "height: 50px;"),
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
              "Due to the nature of the project, there will be some degree of error in this analysis. The topic model is only 
              as good as the data that it's fed. Sometimes the wrong book description is grabbed, which can affect the results. 
              I've tried to ensure that most are correct, but due to the large number of titles, it's a very tedious process."
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
        ),
        tags$p(
          style = "color: white; font-size: 16px; text-align: justify;",
          class = "text-center",
          "This is one of BERTopic’s built-in visualizations. The words shown are based on a ",
          tags$a(
            href = "https://maartengr.github.io/BERTopic/api/ctfidf.html",
            target = "_blank",
            style = "color: white; text-decoration: underline;",
            "c-TF-IDF"
          ),
          " score which considers both term frequency as well as the importance of each term within the topic."
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
        )
      )
    ),
    screen(
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 90vh;",
        column(
          width = 12,
          align = "center",
          tags$h1(
            style = "color: white; font-size: 24px; text-transform: none;",
            "Acknowledgements:"
          ),
          tags$p(
            style = "color: white; font-size: 16px; text-align: center;",
            "My name is Rod Miller and this app was part of my final capstone project for Nashville Software School's Data Science program. I was honored to be a member of ",
            tags$a(
              href = "https://nss-data-science-cohort-6.github.io/",
              target = "_blank",
              style = "color: white; text-decoration: underline;",
              "Data Science Cohort 6!"
            ),
            "I would like to thank my instructors Michael Holloway, Neda Taherkhani, and Rohit Venkat for their feedback and suggestions in helping me complete this project."
          ),
          tags$p(
            style = "color: white; font-size: 16px; text-align: center;",
            tags$a(
              href = "https://github.com/RodNSS/banned_book_exploration",
              target = "_blank",
              style = "color: white; text-decoration: underline;",
              "Github"
            ),
            " - ",
            tags$a(
              href = "https://www.linkedin.com/in/connectwithrod",
              target = "_blank",
              style = "color: white; text-decoration: underline;",
              "LinkedIn"
            )
          )
        )
      )
    )
  )
)
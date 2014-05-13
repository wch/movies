library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

helpPopup <- function(title, content, placement = "top", trigger = "hover") {

  if (!(placement %in% c('right', 'top', 'left', 'bottom'))) {
    stop("placement must be one of 'right', 'top', 'left', or 'bottom'.")
  }
  if (!(trigger %in% c('click', 'hover', 'focus', 'manual'))) {
    stop("trigger must be one of 'click', 'hover', 'focus', or 'manual'.")
  }

  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#",
      class = "btn btn-link",
      `data-toggle` = "popover",
      title = title,
      `data-content` = content,
      `data-animation` = TRUE,
      `data-placement` = placement,
      `data-trigger` = trigger,

      tags$i(class="icon-question-sign")
    )
  )
}

helpTooltip <- function(text, placement = "top") {

  if (!(placement %in% c('right', 'top', 'left', 'bottom'))) {
    stop("placement must be one of 'right', 'top', 'left', or 'bottom'.")
  }

  tagList(
    singleton(
      tags$head(
        # This part needs to be
        tags$script("$(function() { $(\"[data-toggle='tooltip']\").tooltip(); })")
      )
    ),
    tags$a(
      href = "#",
      class = "btn btn-link",
      `data-toggle` = "tooltip",
      `data-placement` = placement,
      title = text,

      tags$i(class="icon-question-sign")
    )
  )
}

shinyUI(fluidPage(
  titlePanel("Movie explorer"),
  fluidRow(
    column(3,
      wellPanel(
        h4("Filter"),
        sliderInput("reviews", "Minimum number of reviews on Rotten Tomatoes",
          10, 300, 80, step = 10),
        helpPopup("help title", "help content"),
        helpTooltip("help content"),
        sliderInput("year", "Year released", 1940, 2014, value = c(1970, 2014)),
        sliderInput("oscars", "Minimum number of Oscar wins (all categories)",
          0, 4, 0, step = 1),
        sliderInput("boxoffice", "Dollars at Box Office (millions)",
          0, 800, c(0, 800), step = 1),
        selectInput("genre", "Genre (a movie can have multiple genres)",
          c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
            "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
            "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
            "Short", "Sport", "Thriller", "War", "Western")
        ),
        textInput("director", "Director name contains (e.g., Miyazaki)"),
        textInput("cast", "Cast names contains (e.g. Tom Hanks)")
      ),
      wellPanel(
        selectInput("xvar", "X-axis variable", axis_vars, selected = "Meter"),
        selectInput("yvar", "Y-axis variable", axis_vars, selected = "Reviews"),
        tags$small(paste0(
          "Note: The Tomato Meter is the proportion of positive reviews",
          " (as judged by the Rotten Tomatoes staff), and the Numeric rating is",
          " a normalized 1-10 score of those reviews which have star ratings",
          " (for example, 3 out of 4 stars)."
        ))
      )
    ),
    column(9,
      ggvisOutput("plot1"),
      wellPanel(
        span("Number of movies selected:",
          textOutput("n_movies")
        )
      )
    )
  )
))

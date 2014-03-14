library(ggvis)
library(dplyr)
library(RSQLite)
library(RSQLite.extfuns)

# Set up handles to database tables on app start
db <- src_sqlite("movies.db")
omdb <- tbl(db, "omdb")
tomatoes <- tbl(db, "tomatoes")

# Join tables, filtering out those with <10 reviews, and select specified columns
# TODO: Rename columns like Rating.x, after dplyr issue #317 is fixed
all_movies <- inner_join(omdb, tomatoes, by = "ID") %.%
  filter(Reviews >= 10) %.%
  select(ID, imdbID, Title, Year, Rating.x, Runtime, Genre, Released,
    Director, Writer, imdbRating, imdbVotes, Language, Country, Oscars,
    Rating.y, Meter, Reviews, Fresh, Rotten, userMeter, userRating, userReviews,
    BoxOffice, Production)


shinyServer(function(input, output, session) {

  # Filter the movies, returning a data frame
  movies <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    reviews <- input$reviews
    oscars <- input$oscars
    minyear <- input$year[1]
    maxyear <- input$year[2]
    minboxoffice <- input$boxoffice[1] * 1e6
    maxboxoffice <- input$boxoffice[2] * 1e6

    # Apply filters
    m <- all_movies %.%
      filter(
        Reviews >= reviews,
        Oscars >= oscars,
        Year >= minyear,
        Year <= maxyear,
        BoxOffice >= minboxoffice,
        BoxOffice <= maxboxoffice
      ) %.%
      arrange(Oscars)

    # Optional: filter by genre
    if (input$genre != "All") {
      genre <- paste0("%", input$genre, "%")
      m <- m %.% filter(Genre %like% genre)
    }
    # Optional: filter by director
    if (!is.null(input$director) && input$director != "") {
      director <- paste0("%", input$director, "%")
      m <- m %.% filter(Director %like% director)
    }
    # Optional: filter by cast member
    if (!is.null(input$cast) && input$cast != "") {
      cast <- paste0("%", input$cast, "%")
      m <- m %.% filter(Cast %like% cast)
    }


    m <- as.data.frame(m)

    # Add column which says whether the movie won any Oscars
    # Be a little careful in case we have a zero-row data frame
    m$has_oscar <- character(nrow(m))
    m$has_oscar[m$Oscars == 0] <- "No"
    m$has_oscar[m$Oscars >= 1] <- "Yes"
    m
  })

  # Function for generating tooltip text
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)

    # Pick out the movie with this ID
    all_movies <- movies()
    movie <- all_movies[all_movies$ID == x$ID, ]

    paste0("<b>", movie$Title, "</b><br>",
      movie$Year, "<br>",
      "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
    )
  }

  # A reactive expression with the ggvis plot
  rgv <- reactive({
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    p <- props(x = prop(as.name(input$xvar)), y = prop(as.name(input$yvar)))

    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]

    ggvis(movies, p) +
      mark_point(props(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5,
        stroke = ~has_oscar, key := ~ID)) +
      tooltip(movie_tooltip) +
      guide_axis("x", title = xvar_name) +
      guide_axis("y", title = yvar_name) +
      guide_legend(stroke = "stroke", title = "Won Oscar",
        values = c("Yes", "No")) +
      dscale("stroke", type = "nominal", domain = c("Yes", "No"),
        range = c("orange", "#aaa")) +
      opts(width = 600, height = 500, renderer = "canvas", duration = 0)
  })

  observe_ggvis(rgv, "plot1", session)

  output$n_movies <- renderText({ nrow(movies()) })
})

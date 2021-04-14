library(ggplot2)
library(stringr)
library(scales)
library(lubridate)
library(stats)
library(gdata)
library(htmlwidgets)
library(shiny)
library(shinydashboard)
library(plotly)
library(Rcpp)
library(tseries)

server <- function(input, output, session) {
  ts_table <- reactive({
    if (is.null(input$fil)) {
      return(NULL)
    } else {
      df <- read.csv(input$fil$datapath, sep = ",")
      
      # Checking if data consists only of two columns
      if (ncol(df) != 2) {
        stop("Incorrect number of columns. Check input file.")
      }
      
      # Renaming columns for calculation purposes
      if (is.numeric(df[, 1])) {
        colnames(df) <- c("Value", "Date")
      } else {
        colnames(df) <- c("Date", "Value")
      }
      
      # Checking if the date format is correct
      dateFormat <- function(mydate, date.format = "%Y-%m-%d") {
        tryCatch(
          !any(is.na(as.Date(
            mydate, date.format
          ))),
          error = function(err) {
            FALSE
          }
        )
      }
      
      # Changing the date format
      if (dateFormat(df$Date) == FALSE) {
        # if then notation is m/d/y
        if (length(unique(str_sub(df2$Date, 1, 2))) > 12) {
          df$Date <- dmy(df2$Date)
        }
        # if then notation is d/m/y
        else {
          df$Date <- mdy(df2$Date)
        }
      }
      # if the format was correct
      else {
        df$Date <- as.Date(df$Date)
      }
      
      df$Index <- seq.int(nrow(df)) # adding indices
      df <- df[c(3, 1, 2)]
    }
    
  })
  
  plot_function <- function(type, df) {
    if (type == "diff") {
      x_var <- df[, "Date"][-1]
      y_var <- diff(df[, "Value"])
    } else {
      x_var <- df[, "Date"]
      y_var <- df[, "Value"]
    }
    
    ggplot(data.frame(x_var, y_var), aes(x = x_var, y = y_var, group = 1)) + geom_line() +
      scale_x_date(breaks = seq(
        min(ts_table()$Date),
        max(ts_table()$Date),
        35 * nrow(df) / input$x_tick
      )) +
      scale_y_continuous(breaks = pretty_breaks(n = input$y_tick)) +
      theme_minimal() +
      theme(axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 0.5
      ))
  }
  
  ts_ggplotly <- reactive({
    if (is.null(input$fil)) {
      return(NULL)
    }
    df <- ts_table()
    
    # Plotting the range based on calendat input
    df <- df[df$Date >= input$dtr[1] & df$Date <= input$dtr[2],]
    ggplotly(plot_function("rw", df))
  })
  
  ts_diff_ggplotly <- reactive({
    if (is.null(input$fil)) {
      return(NULL)
    }
    df <- ts_table()
  
    # Plotting the range based on calendat input
    df <- df[df$Date >= input$dtr[1] & df$Date <= input$dtr[2],]
    ggplotly(plot_function("diff", df))
  })
  
  # Adding title and labels to plot (after submitting)
  plot_org_title <- eventReactive({
    input$add_org_title
    ts_ggplotly()
  }, {
    ts_ggplotly() %>% layout(
      title = input$plot_org_title,
      xaxis = list(title = paste0("\n", input$x_org_axis)),
      yaxis = list(title = input$y_org_axis)
    )
  })
  plot_diff_title <- eventReactive({
    input$add_diff_title
    ts_diff_ggplotly()
  }, {
    ts_diff_ggplotly() %>% layout(
      title = input$plot_diff_title,
      xaxis = list(title = paste0("\n", input$x_diff_axis)),
      yaxis = list(title = input$y_diff_axis)
    )
  })
  
  # Slider with number of obs depending on the lenght of data
  output$slider_obs <- renderUI({
    n_obs <- length(ts_table()$Date)
    
    sliderInput(
      "index",
      HTML("Observations to show in data table:"),
      min = 0,
      max = n_obs,
      value = c(1, n_obs),
      step = 1
    )
  })
  
  # Calendar specifying the time range of plot
  # (min and max values are boundaries specific to dataset)
  output$dates <- renderUI({
    minval <- min(ts_table()$Date)
    maxval <- max(ts_table()$Date)
    
    dateRangeInput(
      'dtr',
      label = "Which interval to plot:",
      start = minval,
      end = maxval,
      min = minval,
      max = maxval,
      format = "yyyy-mm-dd",
      startview = "decade",
      weekstart = 0,
      language = "en",
      separator = "to",
      width = "300px",
      autoclose = TRUE
    )
  })
  
  # Creates decomposition plot for whole time series
  decomposition <- reactive({
    if (is.null(input$fil)) {
      return(NULL)
    }
    df_decomp <- ts(
      data = ts_table()$Value,
      start = c(year(first(ts_table()$Date)), month(first(ts_table()$Date))),
      end = c(year(last(ts_table()$Date)), month(last(ts_table()$Date))),
      frequency = 12
    )
    
    plot(stl(df_decomp, s.window = "periodic"))
  })
  
  # Descriptive statistics for 'Date' 
  descriptive_D <- reactive({
    if (is.null(input$fil)) {
      return("No data.")
    }
    Date <- ts_table()$Date
    Date <- as.factor(Date)
    n <- length(Date)
    
    dateDesc <- data.frame(N = n,
                           start = Date[1],
                           first_quarter = Date[n/4],
                           second_quarter = Date[n/2],
                           third_quarter = Date[3*n/4],
                           end = Date[n])
  })
  
  # Descriptive statistics for 'Value'
  descriptive_V <- reactive({
    if (is.null(input$fil)) {
      return("No data.")
    }
    Value <- ts_table()$Value
    
    cppFunction('DataFrame valueDesc(NumericVector x) {
               
              int n_x;
              double sum_x, mean_x, min_x, median_x, max_x, sd_x;
               
              n_x = x.size();
              sum_x = sum(x);
              mean_x = mean(x);
              min_x = min(x);
              median_x = median(x);
              max_x = max(x);
              sd_x = sd(x);
              
              DataFrame result = DataFrame::create(_["N"] = n_x,
                                                   _["Sum"] = sum_x,
                                                   _["Mean"] = mean_x,
                                                   _["Min"] = min_x,
                                                   _["Median"] = median_x,
                                                   _["Max"] = max_x,
                                                   _["Sd"] = sd_x
              );
                                        
              return(result);
            }')
    
   valueDesc(Value)
  })
  
 
  # Redndering tables with descriptive statistics
  output$descD <- renderTable({
    descriptive_D()
  })
  output$descV <- renderTable({
    descriptive_V()
  })
  
  # Basic stationarity testing
  output$test <- renderTable({
    if (is.null(input$fil)) {
      return("No data.")
    }
    Value <- ts_table()$Value
    t(as.data.frame(unlist(adf.test(Value))))
  })
  
  
  
  # Creates a subsample of the whole dataset based on slider input
  # Shows after pressing the button
  obs_table <- eventReactive({
    input$show_obs
    ts_table()
  }, {
    df <- ts_table()[input$index[1]:input$index[2], ]
    df
  })
  
  # Renders plot without labels title unless they are provided and submitted
  output$plt_org <- renderPlotly({
    plot_org_title()
  })
  output$plt_diff <- renderPlotly({
    plot_diff_title()
  })
  
  # Creates a button enabling to download the current plot in different sizes
  output$SavePlot <- downloadHandler(
    filename = function() {
      paste("plot", "html", sep = ".")
    },
    content = function(file) {
      if (input$sct == "Original Time Serie") {
        plt <- plot_org_title()
      } else if (input$sct == "Difference transforme Time Serie") {
        plt <- plot_diff_title()
      }
      saveWidget(plt, file)
    }
  )
  
  # Renders table with observations
  output$tab <- DT::renderDataTable({
    obs_table()
  }, rownames = FALSE, options = list(
    pageLength = 20,
    autoWidth = TRUE,
    lengthMenu = c(10, 20, 30, 50, 100, 200)
  ))
  
  # Rendering decomposition plots
  output$decomp <- renderPlot({
    decomposition()
  })
}

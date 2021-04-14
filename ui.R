ui_new <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Time series", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      fileInput(
        inputId = "fil",
        label = "Choose one csv from data subfolder:",
        multiple = FALSE,
        accept = c(".csv", ".txt"),
        width = "300px",
        buttonLabel = icon("folder"),
        placeholder = "Upload time series file..."
      ),
      uiOutput("slider_obs"),
      actionButton("show_obs", "Show observations"),
      uiOutput("dates"),
      sliderInput(
        inputId = "x_tick",
        label = "Increase tick frequency on x axis:",
        min = 5,
        max = 30,
        value = 5,
        step = 1
      ),
      sliderInput(
        inputId = "y_tick",
        label = "Increase tick frequency on y axis:",
        min = 5,
        max = 30,
        value = 5,
        step = 1
      ),
      menuItem(
        "Original TS Plot",
        textInput("plot_org_title", "Plot title:"),
        textInput("x_org_axis", "Change x axis label:"),
        textInput("y_org_axis", "Change y axis label:"),
        actionButton("add_org_title", "Add title and labels")
      ),
      menuItem(
        "Differences TS Plot",
        textInput("plot_diff_title", "Plot title:"),
        textInput("x_diff_axis", "Change x axis label:"),
        textInput("y_diff_axis", "Change y axis label:"),
        actionButton("add_diff_title", "Add title and labels")
      )
    )
  ),
  dashboardBody(tabsetPanel(
    type = "tab",
    tabPanel("Data Table", DT::dataTableOutput("tab")),
    tabPanel(
      "Summary",
      HTML("<h4><b> Date </b></h4>"),
      tableOutput("descD"),
      HTML("<h4><b> Values </b></h4>"),
      tableOutput("descV"),
      HTML("<h4><b> Basic stationarity ADF Test </b></h4>"),
      tableOutput("test"),
      HTML("p-value below significance level indicates stationarity. Further testing needed (e.g. autocorrelation).")
    ), 
    
    tabPanel(
      "Plots",
      plotlyOutput("plt_org"),
      plotlyOutput("plt_diff"),
      selectInput(
        inputId = "sct",
        label = "Type of plot to save:",
        selected = "Original Time Serie",
        multiple = FALSE,
        choices = list("Original Time Serie", "Difference transformed Time Serie"),
        size = 3,
        selectize = FALSE
      ),
      downloadButton("SavePlot", "Save Plot")
    ),
    tabPanel("Decomposition", plotOutput("decomp", height = "800px"))
  ))
)

library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(tools)

#-----------------------------
# 📁 경로 설정
#-----------------------------
IMG_DIR <- "C:/iOrganoAssay/microscopy"
SEG_DIR <- "C:/iOrganoAssay/segmentation"
MET_DIR <- "C:/iOrganoAssay/metrics"

# 🔥 로컬 파일 표시 핵심
addResourcePath("img", IMG_DIR)
addResourcePath("seg", SEG_DIR)

#-----------------------------
# 📊 metric 읽기
#-----------------------------
read_metric_vector <- function(path, metric){

  if(!file.exists(path)) return(NULL)

  if(metric == "area"){
    df <- read_excel(path, sheet = "Outline Area (µm²)")
  } else if(metric == "perimeter"){
    df <- read_excel(path, sheet = "Perimeter (µm)")
  } else {
    df <- read_excel(path, sheet = "Circularity")
  }

  if(!("Frame 0" %in% colnames(df))) return(NULL)

  df[["Frame 0"]]
}

#-----------------------------
# UI
#-----------------------------
ui <- fluidPage(

  titlePanel("iOrganoAssay | Analysis"),

  sidebarLayout(

    sidebarPanel(

      h4("1. Select metafile"),
      fileInput("meta", "Upload metadata Excel file"),

      hr(),

      h4("2. Category condition"),
      selectInput("mice", "Mice", choices = NULL),
      selectInput("passage", "Passage", choices = NULL),
      selectInput("day", "Day", choices = NULL),
      selectInput("well", "Microwell", choices = NULL),

      actionButton("apply", "Apply"),

      hr(),

      h4("3. Metric 선택"),
      selectInput("metric", "Metric",
                  choices = c("area", "perimeter", "circularity"))
    ),

    mainPanel(

      h4("Microscopy Images"),
      uiOutput("images"),

      hr(),

      h4("Segmentation Images"),
      uiOutput("seg_images"),

      hr(),

      h4("Mean / Median Plot"),
      plotOutput("meanPlot", height = "300px"),

      hr(),

      h4("Violin Plot"),
      plotOutput("violinPlot", height = "400px")
    )
  )
)

#-----------------------------
# SERVER
#-----------------------------
server <- function(input, output, session){

  #-----------------------------
  # metadata
  #-----------------------------
  meta <- reactive({
    req(input$meta)
    read_excel(input$meta$datapath)
  })

  #-----------------------------
  # dropdown (All 포함)
  #-----------------------------
  observe({

    df <- meta()

    updateSelectInput(session, "mice",
                      choices = c("All", unique(df$mice)))

    updateSelectInput(session, "passage",
                      choices = c("All", unique(df$passage)))

    updateSelectInput(session, "day",
                      choices = c("All", unique(df$day)))

    updateSelectInput(session, "well",
                      choices = c("All", unique(df$microwell)))
  })

  #-----------------------------
  # filtering
  #-----------------------------
  filtered <- eventReactive(input$apply, {

    df <- meta()

    if(input$mice != "All") df <- df %>% filter(mice == input$mice)
    if(input$passage != "All") df <- df %>% filter(passage == input$passage)
    if(input$day != "All") df <- df %>% filter(day == input$day)
    if(input$well != "All") df <- df %>% filter(microwell == input$well)

    df %>% arrange(day)
  })

  #-----------------------------
  # 🔬 RAW 이미지 (Grid)
  #-----------------------------
  output$images <- renderUI({

    df <- filtered()
    req(df)

    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:15px;",

      lapply(df$filename, function(f){

        tags$div(
          style = "text-align:center;",

          tags$p(f, style="font-size:10px;"),

          tags$img(
            src = file.path("img", f),
            style = "width:120px;"
          )
        )
      })
    )
  })

  #-----------------------------
  # 🧬 SEG 이미지 (Grid)
  #-----------------------------
  output$seg_images <- renderUI({

    df <- filtered()
    req(df)

    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:15px;",

      lapply(df$filename, function(f){

        tags$div(
          style = "text-align:center;",

          tags$p(f, style="font-size:10px;"),

          tags$img(
            src = file.path("seg", f),
            style = "width:120px;"
          )
        )
      })
    )
  })

  #-----------------------------
  # 📈 Mean + Median plot
  #-----------------------------
  output$meanPlot <- renderPlot({

    df <- filtered()
    req(df)

    df$day_num <- as.numeric(str_extract(df$day, "\\d+"))

    df$value <- sapply(df$filename, function(f){

      path <- file.path(MET_DIR,
                        paste0(file_path_sans_ext(f), "_metrics.xlsx"))

      v <- read_metric_vector(path, input$metric)

      if(is.null(v)) return(NA)

      mean(v, na.rm = TRUE)
    })

    ggplot(df, aes(x = day_num, y = value,
                   color = microwell, group = microwell)) +

      geom_point(size = 3) +
      geom_line() +

      # 🔥 median 추가
      stat_summary(fun = median,
                   geom = "point",
                   shape = 18,
                   size = 4,
                   color = "blue") +

      scale_x_continuous(
        breaks = df$day_num,
        labels = df$day
      ) +

      labs(x = "Day", y = "Mean / Median") +
      theme_minimal()
  })

  #-----------------------------
  # 🎻 Violin plot
  #-----------------------------
  output$violinPlot <- renderPlot({

    df <- filtered()
    req(df)

    all_data <- data.frame()

    for(i in 1:nrow(df)){

      f <- df$filename[i]

      path <- file.path(MET_DIR,
                        paste0(file_path_sans_ext(f), "_metrics.xlsx"))

      v <- read_metric_vector(path, input$metric)

      if(!is.null(v)){

        tmp <- data.frame(
          value = v,
          day = df$day[i],
          microwell = df$microwell[i]
        )

        all_data <- rbind(all_data, tmp)
      }
    }

    ggplot(all_data, aes(x = day, y = value,
                         fill = microwell)) +

      geom_violin(trim = FALSE, alpha = 0.5) +
      geom_jitter(width = 0.1, size = 1, alpha = 0.4) +

      labs(x = "Day", y = input$metric) +
      theme_minimal()
  })
}

#-----------------------------
# 실행
#-----------------------------
shinyApp(ui, server)

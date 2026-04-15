library(shiny)
library(readxl)
library(dplyr)
library(tools)
library(ggplot2)
library(magick)

#----------------------------- 
# 고정 경로 (🔥 핵심)
#-----------------------------
IMG_DIR <- "C:/iOrganoAssay/microscopy"
SEG_DIR <- "C:/iOrganoAssay/segmentation"
MET_DIR <- "C:/iOrganoAssay/metrics"

#-----------------------------
# tif → png 변환
#-----------------------------
convert_to_png <- function(path) {
  if (!file.exists(path)) return(NULL)
  
  img <- image_read(path)
  tmp <- tempfile(fileext = ".png")
  image_write(img, tmp)
  
  tmp
}

#-----------------------------
# Day parsing (Day1 → 1)
#-----------------------------
parse_day <- function(x) {
  as.numeric(gsub("[^0-9]", "", x))
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
      uiOutput("filter_ui"),
      
      actionButton("apply", "Apply"),
      
      hr(),
      
      h4("3. Metric 선택"),
      selectInput("metric", "Metric",
                  choices = c("area", "perimeter", "circularity"))
    ),
    
    mainPanel(
      
      h4("Images"),
      uiOutput("image_ui"),
      
      hr(),
      
      h4("Metric Plot"),
      plotOutput("plot")
    )
  )
)

#-----------------------------
# SERVER
#-----------------------------
server <- function(input, output, session) {
  
  #-----------------------------
  # metadata load
  #-----------------------------
  metadata <- reactive({
    
    req(input$meta)
    
    df <- read_excel(input$meta$datapath)
    
    df <- df %>%
      mutate(
        metric_file = paste0(file_path_sans_ext(filename), "_metrics.xlsx"),
        metric_path = file.path(MET_DIR, metric_file),
        day_num = parse_day(day)
      )
    
    df
  })
  
  #-----------------------------
  # filter UI
  #-----------------------------
  output$filter_ui <- renderUI({
    
    df <- metadata()
    
    tagList(
      selectInput("mice", "Mice",
                  choices = c("All", unique(df$mice))),
      
      selectInput("passage", "Passage",
                  choices = c("All", unique(df$passage))),
      
      selectInput("day", "Day",
                  choices = c("All", unique(df$day))),
      
      selectInput("microwell", "Microwell",
                  choices = c("All", unique(df$microwell))),
      
      selectInput("drug", "Drug",
                  choices = c("All", unique(df$drug)))
    )
  })
  
  #-----------------------------
  # filtering
  #-----------------------------
  filtered <- eventReactive(input$apply, {
    
    df <- metadata()
    
    if (input$mice != "All")
      df <- df %>% filter(mice == input$mice)
    
    if (input$passage != "All")
      df <- df %>% filter(passage == input$passage)
    
    if (input$day != "All")
      df <- df %>% filter(day == input$day)
    
    if (input$microwell != "All")
      df <- df %>% filter(microwell == input$microwell)
    
    if (input$drug != "All")
      df <- df %>% filter(drug == input$drug)
    
    df <- df %>% arrange(day_num)
    
    df
  })
  
  #-----------------------------
  # image display
  #-----------------------------
  output$image_ui <- renderUI({
    
    df <- filtered()
    req(nrow(df) > 0)
    
    tagList(
      lapply(1:nrow(df), function(i) {
        
        file <- df$filename[i]
        
        raw_path <- file.path(IMG_DIR, file)
        seg_path <- file.path(SEG_DIR, file)
        
        raw_png <- convert_to_png(raw_path)
        seg_png <- convert_to_png(seg_path)
        
        tags$div(
          style = "display:inline-block; margin:10px;",
          
          tags$p(file),
          
          tags$img(src = raw_png, height = "120px"),
          tags$br(),
          tags$img(src = seg_png, height = "120px")
        )
      })
    )
  })
  
  #-----------------------------
  # metric 읽기
  #-----------------------------
  #read_metric <- function(path, metric) {
    
   # if (!file.exists(path)) return(NA)
    
   #  df <- read_excel(path)
    
   #  if (!(metric %in% colnames(df))) return(NA)
    
   #  df[[metric]][1]
   #  }
  
  read_metric <- function(path, metric) {
    
    if (!file.exists(path)) return(NA)
    
    # metric에 따라 sheet 선택
    sheet_name <- switch(metric,
                         area = "Outline Area (µm²)",
                         perimeter = "Perimeter (µm)",
                         circularity = "Circularity")
    
    df <- read_excel(path, sheet = sheet_name)
    
    # "Frame 0" 컬럼 존재 확인
    if (!("Frame 0" %in% colnames(df))) return(NA)
    
    values <- df[["Frame 0"]]
    
    # 👉 대표값 선택 (여기 중요)
    return(mean(values, na.rm = TRUE))
  }
  
  
  
  #-----------------------------
  # plot
  #-----------------------------
  output$plot <- renderPlot({
    
    df <- filtered()
    req(nrow(df) > 0)
    
    values <- sapply(df$metric_path, read_metric, metric = input$metric)
    
    df$value <- values
    df$order <- seq_len(nrow(df))
    
    ggplot(df, aes(x = order, y = value)) +
      geom_point(size = 3) +
      geom_line() +
      labs(
        x = "Image order (Day)",
        y = input$metric
      ) +
      theme_minimal(base_size = 15)
  })
}

#-----------------------------
# RUN
#-----------------------------
shinyApp(ui, server)



library(shiny)
library(readxl)
library(dplyr)
library(tools)
library(ggplot2)

#-----------------------------
# 🔥 고정 경로
#-----------------------------
IMG_DIR <- "C:/iOrganoAssay/microscopy"
SEG_DIR <- "C:/iOrganoAssay/segmentation"
MET_DIR <- "C:/iOrganoAssay/metrics"

#IMG_DIR <- "E:/Dropbox/@Manuscript 원고, 연구 topic/^mIO drug treatment_DSS TERM 조직재생학회지/!data deposit 데이터 논문/data 1/iOrganoAssay/microscopy"
#SEG_DIR <- "E:/Dropbox/@Manuscript 원고, 연구 topic/^mIO drug treatment_DSS TERM 조직재생학회지/!data deposit 데이터 논문/data 1/iOrganoAssay/segmentation"
#MET_DIR <- "E:/Dropbox/@Manuscript 원고, 연구 topic/^mIO drug treatment_DSS TERM 조직재생학회지/!data deposit 데이터 논문/data 1/iOrganoAssay/metrics"


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
      
      h4("Segmentation"),
      uiOutput("seg_ui"),
      
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
  
  # 🔥 이미지 폴더를 웹에서 접근 가능하게 설정
  addResourcePath("img", IMG_DIR)
  addResourcePath("seg", SEG_DIR)
  
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
  # 이미지 display (🔥 핵심 수정 완료)
  #-----------------------------
  output$image_ui <- renderUI({
    
    df <- filtered()
    req(nrow(df) > 0)
    
    tagList(
      lapply(1:nrow(df), function(i) {
        
        file <- df$filename[i]
        
        tags$div(
          style = "display:inline-block; margin:10px;",
          
          tags$p(file),
          
          tags$img(src = file.path("img", file), height = "120px"),
          tags$br(),
          # tags$img(src = file.path("seg", file), height = "120px")
        )
      })
    )
  })
  
  
  #-----------------------------
  # segmentation 이미지 display
  #-----------------------------
  
  
  output$seg_ui <- renderUI({
    
    df <- filtered()
    req(nrow(df) > 0)
    
    tagList(
      lapply(1:nrow(df), function(i) {
        
        file <- df$filename[i]
        
        tags$div(
          style = "display:inline-block; margin:10px;",
          
          tags$p(file),
          
          #tags$img(src = file.path("img", file), height = "120px"),
          #tags$br(),
          tags$img(src = file.path("seg", file), height = "120px")
        )
      })
    )
  })
  
  
  
  
  
  #-----------------------------
  # metric 읽기 (Outline / Frame 0)
  #-----------------------------
  read_metric <- function(path, metric) {
    
    if (!file.exists(path)) return(NA)
    
    sheet_name <- switch(metric,
                         area = "Outline Area (µm²)",
                         perimeter = "Perimeter (µm)",
                         circularity = "Circularity")
    
    df <- read_excel(path, sheet = sheet_name)
    
    if (!("Frame 0" %in% colnames(df))) return(NA)
    
    values <- df[["Frame 0"]]
    
    return(mean(values, na.rm = TRUE))
  }

 
  
  #-----------------------------
  # plot (🔥 완전 업그레이드)
  #-----------------------------
  output$plot <- renderPlot({
    
    df <- filtered()
    req(nrow(df) > 0)
    
    values <- sapply(df$metric_path, read_metric, metric = input$metric)
    df$value <- values
    
    #-----------------------------
    # 🔥 시간축 생성 (핵심)
    #-----------------------------
    df <- df %>%
      mutate(
        day_num = as.numeric(gsub("[^0-9]", "", day)),
        
        # label (표시용)
        x_label = ifelse(
          is.na(treatment),
          day,
          paste0(day, treatment)
        )
      ) %>%
      arrange(day_num)
    
    #-----------------------------
    # 🔥 plot
    #-----------------------------
    ggplot(df, aes(x = day_num, y = value, color = microwell)) +
      
      # 점
      geom_point(size = 3) +
      
      # 선 (microwell 별 연결)
      geom_line(aes(group = microwell), linewidth = 1) +
      
      # x축을 실제 날짜 기반으로
      scale_x_continuous(
        breaks = df$day_num,
        labels = df$x_label
      ) +
      
      labs(
        x = "Day (real time scale)",
        y = input$metric,
        color = "Microwell"
      ) +
      
      theme_minimal(base_size = 15)
  })
  
  
   
  
}

#-----------------------------
# RUN
#-----------------------------
shinyApp(ui, server)

library(dplyr)
library(stringr)
library(tools)
library(openxlsx)
library(magick)

#-----------------------------
# 🔧 경로 설정
#-----------------------------
#BASE_DIR <- "C:/your_raw_data"

BASE_DIR <- "E:/Dropbox/@Manuscript 원고, 연구 topic/^mIO drug treatment_DSS TERM 조직재생학회지/!data deposit 데이터 논문/data 1/AIVIA4"

OUT_DIR  <- "C:/iOrganoAssay"

IMG_DIR <- file.path(OUT_DIR, "1.Microscopy")
SEG_DIR <- file.path(OUT_DIR, "2.Segmentation")
MET_DIR <- file.path(OUT_DIR, "3.Metrics")

dir.create(IMG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SEG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MET_DIR, recursive = TRUE, showWarnings = FALSE)

#-----------------------------
# 📊 metadata 저장용
#-----------------------------
meta_list <- list()

#-----------------------------
# 🔁 폴더 순회
#-----------------------------
day_folders <- list.dirs(BASE_DIR, recursive = FALSE)

for (folder in day_folders) {
  
  folder_name <- basename(folder)
  
  # 예: "N6p4 d08t1"
  parts <- str_split(folder_name, " ")[[1]]
  
  mp      <- parts[1]
  day_raw <- parts[2]
  
  #-----------------------------
  # mice / passage
  #-----------------------------
  mice    <- str_extract(mp, "[A-Z]\\d+")
  passage <- str_extract(mp, "p\\d+")
  
  #-----------------------------
  # day / treatment 분리 (🔥 핵심 수정)
  #-----------------------------
  day <- str_extract(day_raw, "d\\d+")
  
  if (str_detect(day_raw, "t\\d+")) {
    treatment <- str_extract(day_raw, "t\\d+")
  } else {
    treatment <- NA   # 🔥 핵심 변경
  }
  
  cat("📁 Processing:", folder_name, "\n")
  
  #-----------------------------
  # RAW 이미지
  #-----------------------------
  img_files <- list.files(
    folder,
    pattern = "\\.(tif|tiff)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  #-----------------------------
  # segmentation
  #-----------------------------
  seg_folder <- list.dirs(folder, recursive = FALSE, full.names = TRUE) %>%
    .[str_detect(., "AIVIA")]
  
  seg_files <- list.files(
    seg_folder,
    pattern = "\\.(tif|tiff|png|jpg|jpeg)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  #-----------------------------
  # metrics
  #-----------------------------
  met_folder <- list.dirs(folder, recursive = FALSE, full.names = TRUE) %>%
    .[str_detect(., "Spreadsheet")]
  
  met_files <- list.files(
    met_folder,
    pattern = "\\.xlsx$",
    full.names = TRUE
  )
  
  #-----------------------------
  # 정렬
  #-----------------------------
  img_files <- sort(img_files)
  seg_files <- sort(seg_files)
  met_files <- sort(met_files)
  
  #-----------------------------
  # 처리
  #-----------------------------
  for (i in seq_along(img_files)) {
    
    raw <- img_files[i]
    seg <- seg_files[i]
    met <- met_files[i]
    
    well <- paste0("W", i)
    
    new_name <- paste(mice, passage, well, day, sep = "_")
    
    img_out <- file.path(IMG_DIR, paste0(new_name, ".png"))
    seg_out <- file.path(SEG_DIR, paste0(new_name, ".png"))
    met_out <- file.path(MET_DIR, paste0(new_name, "_metrics.xlsx"))
    
    # RAW → PNG
    try({
      img <- image_read(raw)
      img <- image_resize(img, "512x512")
      image_write(img, img_out, format = "png")
    }, silent = TRUE)
    
    # SEG → PNG
    try({
      img2 <- image_read(seg)
      img2 <- image_resize(img2, "512x512")
      image_write(img2, seg_out, format = "png")
    }, silent = TRUE)
    
    # metrics 복사
    file.copy(met, met_out, overwrite = TRUE)
    
    # metadata 생성
    meta_list[[length(meta_list) + 1]] <- data.frame(
      filename   = paste0(new_name, ".png"),
      mice       = mice,
      passage    = passage,
      microwell  = well,
      day        = day,
      treatment  = treatment,   # 🔥 NA or tX
      drug       = "NA",
      stringsAsFactors = FALSE
    )
  }
}

#-----------------------------
# 📊 저장
#-----------------------------
metadata <- bind_rows(meta_list)

write.xlsx(metadata, file.path(OUT_DIR, "0.metadata.xlsx"))

cat("\n✅ 완료: treatment 정확히 반영된 metadata 생성 완료\n")

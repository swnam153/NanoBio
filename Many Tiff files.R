library(readxl)

# 1. metadata 읽기
data <- read_excel("C:/raw/dataset sheet.xlsx")

# 2. 파일명 생성
data$filename <- sprintf(
  "%s_%s_Day%s_%s_%s",
  data$number,
  data$`organoid id`,
  data$Day,
  data$Microwell,
  data$Channel
)

data$filename <- gsub("[/\\\\:*?\"<>|]", "_", data$filename)

# 3. TIFF 파일 목록
files <- list.files(
  "C:/raw",
  pattern = "\\.tif$",
  full.names = TRUE
)

files <- sort(files)

# 4. output 폴더 생성
output_dir <- "C:/images/output1/"
dir.create(output_dir, showWarnings = FALSE)

# 5. rename pipeline
for(i in seq_along(files)){

  newname <- paste0(output_dir, data$filename[i], ".tif")

  file.copy(files[i], newname)

}

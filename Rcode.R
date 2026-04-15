library(EBImage)

img <- readImage("C:/raw/1_Lng_LVCC Merged.tif")
display(img)


library(readxl)
data <- read_excel("C:/raw/dataset sheet.xlsx")

data$filename <- paste(
  data$`organoid id`,
  paste0("Day", data$Day),
  data$Microwell,
  data$Channel,
  sep="_"
)

for(i in 1:nrow(data)){
  newname <- paste0("C:/images/", data$filename[i], ".tif")
  writeImage(img, newname)
}


install.packages("readxl")
library(readxl)
organoid <- read_excel("C:/raw/dataset sheet.xlsx")
plot(organoid$area, organoid$circularity)



plot(organoid$area, organoid$circularity,
     xlab="Area",
     ylab="Circularity",
     pch=19,
     col="blue")
abline(h=2.0, col="red", lwd=2)



plot(organoid$area, organoid$circularity,
     pch=16,
     cex=1.5,
     xlab="Organoid Area",
     ylab="Circularity",
     main="Organoid Morphology Analysis")



barplot(organoid$area,
        names.arg=organoid$`organoid id`,
        col="skyblue",
        ylab="Area")



install.packages("writexl")
library(writexl)
write_xlsx(organoid,
           "C:/images/organoid_dataset.xlsx")



tiff("C:/images/Area_Circularity_plot.tiff",
     width = 2000,
     height = 2000,
     res = 300)
plot(organoid$area, organoid$circularity,
     pch = 16,
     col = "blue",
     xlab = "Organoid Area",
     ylab = "Circularity",
     main = "Organoid Morphology")
dev.off()

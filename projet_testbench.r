# definition du working directory a l'emplecament du fichier
setwd("G:/Mon Drive/Master2/LSTAT2110 - Analyse des données/Projet")


library(FactoMineR)
library(corrplot)
library(pander)


### original study link : http://jse.amstat.org/v11n2/datasets.heinz.html

# download.file("https://www.openintro.org/book/statdata/bdims.csv", destfile = "bdims.csv")

data.initial <- read.csv("bdims.csv")

### optional depending on the processus
# initial.data$sex[which(initial.data$sex == 0)] <- "F"
# initial.data$sex[which(initial.data$sex == 1)] <- "M"

# skelettal diameter/girth measurements (in cm) and other measurements

real.names <- c("Biacromial diameter",
               "Biiliac diameter",
               "Bitrochanteric diameter",
               "Chest depth between spine and sternum at nipple level,mid-expiration",
               "Chest diameter at nipple level,mid-expiration",
               "Elbow diameter, sum of two elbows",
               "Wrist diameter, sum of two wrists",
               "Knee diameter, sum of two knees",
               "Ankle diameter, sum of two ankles",
               "Shoulder girth over deltoid muscles",
               "Chest girth, nipple line in males and just above breast tissue in females, mid-expiration",
               "Waist girth, narrowest part of torso below the rib cage, average of contracted and relaxed position",
               "Navel (or\"Abdominal\") girth at umbilicus and iliac crest, iliac crest as a landmark",
               "Hip girth at level of bitrochanteric diameter",
               "Thigh girth below gluteal fold, average of right and left girths",
               "Bicep girth, flexed, average of right and left girths",
               "Forearm girth, extended, palm up, average of right and left girths",
               "Knee girth over patella, slightly flexed position, average of right and left girths",
               "Calf maximum girth, average of right and left girths",
               "Ankle minimum girth, average of right and left girths",
               "Wrist minimum girth, average of right and left girths",
               "Age (years)",
               "Weight (kg)",
               "Height (cm)",
               "Gender"
               )

# names(data.initial) <- real.names

### we'll focus on particular measurements only

# data <- subset(data.initial, select = c(6, 7, 8, 9, 18, 20, 21, 22, 23, 24, 25))
# data <- subset(data.initial, select = -c(2,3,22))
data <- data.initial

data.quantitative <- subset(data, select = -c(length(data)))

summary(data)


res <- PCA(data.quantitative, graph=TRUE)
plot.PCA(res, choix = "var", axes=c(1, 3))


normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data.normalized <- as.data.frame(lapply(data.quantitative, normalize))
cp <- corrplot(cor(data.normalized), method = "number")

tmp <- cor(data)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0

# Above two commands can be replaced with 
# tmp[!lower.tri(tmp)] <- 0


data.quantitative.simplified <- data.quantitative[, !apply(tmp, 2, function(x) any(abs(x) > 0.8, na.rm = TRUE))]

res.simplified <- PCA(data.new, graph=TRUE)

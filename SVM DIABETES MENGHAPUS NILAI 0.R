# Memuat data
diabetes <- read.csv("C:/Users/Asus/Documents/PROJEK CCIT SEMESTER 3/DAP/diabetes.csv")

# Menghapus baris dengan nilai 0 di kolom tertentu (tanpa mengisi ulang)
diabetes_cleaned <- diabetes[
  diabetes$Pregnancies != 0 &
    diabetes$Glucose != 0 &
    diabetes$BloodPressure != 0 &
    diabetes$SkinThickness != 0 &
    diabetes$Insulin != 0 &
    diabetes$BMI != 0, 
]

# Menampilkan dimensi data setelah pembersihan
cat("Dimensi data setelah menghapus nilai 0:", dim(diabetes_cleaned), "\n")

# Melihat standar deviasi
sd_with_zero <- sapply(diabetes_cleaned[, 1:8], function(col) {
  if (is.numeric(col)) {
    return(sd(col, na.rm = TRUE))
  }
})

# Menampilkan hasil
cat("\nStandar Deviasi:\n")
print(sd_with_zero)

# TRAIN & TEST
library(caTools)
set.seed(123)

# Membagi data menjadi train dan test
sample <- sample.split(diabetes_cleaned$Outcome, SplitRatio = 0.80)
train <- subset(diabetes_cleaned, sample == TRUE)
test <- subset(diabetes_cleaned, sample == FALSE)

# Memastikan kolom Outcome bertipe faktor
train$Outcome <- as.factor(train$Outcome)
test$Outcome <- as.factor(test$Outcome)

# Menampilkan dimensi data latih dan uji
cat("Dimensi Data Latih:", dim(train), "\n")
cat("Dimensi Data Uji:", dim(test), "\n")

# Melakukan klasifikasi dengan SVM
library(e1071)
svm_linear <- svm(Outcome ~ ., data = train, kernel = "linear", scale = TRUE)
svm_radial <- svm(Outcome ~ ., data = train, kernel = "radial", scale = TRUE)
svm_polynomial <- svm(Outcome ~ ., data = train, kernel = "polynomial", scale = TRUE)

# Menampilkan ringkasan model
cat("Ringkasan Model SVM Linear:\n")
summary(svm_linear)

cat("\nRingkasan Model SVM Radial:\n")
summary(svm_radial)

cat("\nRingkasan Model SVM Polynomial:\n")
summary(svm_polynomial)

# Melakukan prediksi pada data uji
predictions_linear <- predict(svm_linear, test)
predictions_radial <- predict(svm_radial, test)
predictions_polynomial <- predict(svm_polynomial, test)

# Menghitung Confusion Matrix untuk setiap model
confusion_matrix_linear <- table(Predicted = predictions_linear, Actual = test$Outcome)
confusion_matrix_radial <- table(Predicted = predictions_radial, Actual = test$Outcome)
confusion_matrix_polynomial <- table(Predicted = predictions_polynomial, Actual = test$Outcome)

# Menampilkan Confusion Matrix dan Akurasi
cat("\nConfusion Matrix SVM Linear:\n")
print(confusion_matrix_linear)

cat("\nConfusion Matrix SVM Radial:\n")
print(confusion_matrix_radial)

cat("\nConfusion Matrix SVM Polynomial:\n")
print(confusion_matrix_polynomial)

# Menghitung akurasi
accuracy_linear <- sum(diag(confusion_matrix_linear)) / sum(confusion_matrix_linear)
accuracy_radial <- sum(diag(confusion_matrix_radial)) / sum(confusion_matrix_radial)
accuracy_polynomial <- sum(diag(confusion_matrix_polynomial)) / sum(confusion_matrix_polynomial)

cat("\nAkurasi Model SVM Linear:", accuracy_linear, "\n")
cat("Akurasi Model SVM Radial:", accuracy_radial, "\n")
cat("Akurasi Model SVM Polynomial:", accuracy_polynomial, "\n")


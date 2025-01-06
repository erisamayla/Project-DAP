# Menyimpan data dan melihat data
diabetes <- read.csv("C:/Users/Asus/Documents/PROJEK CCIT SEMESTER 3/DAP/diabetes.csv")

# Cek apakah terdapat nilai 0
# Memasukkan nilai median kolom SkinThickness ke variabel (menggantikan nilai yang 0)
any(diabetes$SkinThickness == 0)
med_skt <- median(diabetes$SkinThickness[diabetes$SkinThickness != 0], na.rm = TRUE)
med_skt
diabetes$SkinThickness[diabetes$SkinThickness == 0] <- med_skt
diabetes$SkinThickness <- round(diabetes$SkinThickness, 1)
any(diabetes$SkinThickness == 0)

# Cek apakah terdapat nilai 0
# Memasukkan nilai median kolom BloodPressure ke variabel (menggantikan nilai yang 0)
any(diabetes$BloodPressure == 0)
med_bld <- median(diabetes$BloodPressure[diabetes$BloodPressure != 0], na.rm = TRUE)
med_bld
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- med_bld
diabetes$BloodPressure <- round(diabetes$BloodPressure, 1)
any(diabetes$BloodPressure == 0)

# Cek apakah terdapat nilai 0
# Memasukkan nilai mean kolom Glucose ke variabel (menggantikan nilai yang 0)
any(diabetes$Glucose == 0)
med_glc <- median(diabetes$Glucose[diabetes$Glucose != 0], na.rm = TRUE)
med_glc
diabetes$Glucose[diabetes$Glucose == 0] <- med_glc
diabetes$Glucose <- round(diabetes$Glucose, 1)
any(diabetes$Glucose == 0)

# Cek apakah terdapat nilai 0
# Memasukkan nilai mean kolom BMI ke variabel (menggantikan nilai yang 0)
any(diabetes$BMI == 0)
med_BMI <- median(diabetes$BMI[diabetes$BMI != 0], na.rm = TRUE)
med_BMI
diabetes$BMI[diabetes$BMI == 0] <- med_BMI
diabetes$BMI <- round(diabetes$BMI, 1)
any(diabetes$BMI == 0)

# Cek apakah terdapat nilai 0
# Memasukkan nilai mean kolom BMI ke variabel (menggantikan nilai yang 0)
any(diabetes$Insulin == 0)
med_isn <- median(diabetes$Insulin[diabetes$Insulin != 0], na.rm = TRUE)
med_isn
diabetes$Insulin[diabetes$Insulin == 0] <- med_isn
diabetes$Insulin <- round(diabetes$Insulin, 1)
any(diabetes$Insulin == 0)

# Melihat standar deviasi
sd_with_median <- sapply(diabetes[, 1:8], function(col) {
  if (is.numeric(col)) {
    return(sd(col, na.rm = TRUE))
  }
})

# Menampilkan Hasil
cat("\nStandar Deviasi Setelah Filling (dengan mean):\n")
print(sd_with_median)

# Memasukkan Fungsi Normalisasi Min-Max ke Variabel "normalize"
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Menerapkan Fungsi "normalize ke beberapa kolom Data diabetes
diabetes$Glucose <- normalize(diabetes$Glucose)
diabetes$BloodPressure <- normalize(diabetes$BloodPressure)
diabetes$SkinThickness <- normalize(diabetes$SkinThickness)
diabetes$Insulin <- normalize(diabetes$Insulin)
diabetes$DiabetesPedigreeFunction <- normalize(diabetes$DiabetesPedigreeFunction)
diabetes$Age <- normalize(diabetes$Age)
diabetes$BMI <- normalize(diabetes$BMI)
diabetes$Pregnancies <- normalize(diabetes$Pregnancies)
diabetes$Outcome <- normalize(diabetes$Outcome)

# Cek Normalisasi
range(diabetes$Glucose)
range(diabetes$BloodPressure)
range(diabetes$SkinThickness)
range(diabetes$Insulin)
range(diabetes$DiabetesPedigreeFunction)
range(diabetes$Age)
range(diabetes$BMI)
range(diabetes$Pregnancies)
range(diabetes$Outcome)

# Cek Korelasi
correlation_diabetes <- cor(diabetes[1:9])
correlation_diabetes

# Menambahkan angka ke dalam kotak heatmap
library(ggcorrplot)
ggcorrplot(
  correlation_diabetes,
  lab = TRUE,                  # Menampilkan angka pada heatmap
  lab_size = 3,                # Ukuran angka
  colors = c("blue", "white", "red"), # Skema warna (opsional)
  title = "Heatmap Korelasi Diabetes", # Judul heatmap
  ggtheme = theme_minimal()    # Tema visualisasi
)

# TRAIN & TEST
# Mengatur seed
library(caTools)
set.seed(123)

# Membagi data menggunakan sample.split
sample <- sample.split(diabetes$Outcome, SplitRatio = 0.80)

# Membagi data menjadi data latih dan data uji
train <- subset(diabetes, sample == TRUE)
head(train)
test <- subset(diabetes, sample == FALSE)
head(test)

# Menampilkan dimensi data
cat("Dimensi Data Latih:", dim(train), "\n")
cat("Dimensi Data Uji:", dim(test), "\n")

# Memastikan kolom Outcome bertipe faktor
train$Outcome <- as.factor(train$Outcome)
test$Outcome <- as.factor(test$Outcome)

# Melakukan klasifikasi dengan SVM dengan kernel linear, radial, dan polynomial
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

# Menampilkan confusion matrix dan akurasi untuk setiap model
cat("\nConfusion Matrix untuk Model SVM Linear:\n")
print(confusion_matrix_linear)

cat("\nConfusion Matrix untuk Model SVM Radial:\n")
print(confusion_matrix_radial)

cat("\nConfusion Matrix untuk Model SVM Polynomial:\n")
print(confusion_matrix_polynomial)

# Menghitung akurasi untuk setiap model
accuracy_linear <- sum(diag(confusion_matrix_linear)) / sum(confusion_matrix_linear)
accuracy_radial <- sum(diag(confusion_matrix_radial)) / sum(confusion_matrix_radial)
accuracy_polynomial <- sum(diag(confusion_matrix_polynomial)) / sum(confusion_matrix_polynomial)

cat("\nAkurasi Model SVM Linear:", accuracy_linear, "\n")
cat("Akurasi Model SVM Radial:", accuracy_radial, "\n")
cat("Akurasi Model SVM Polynomial:", accuracy_polynomial, "\n")


# PLOT SVM 
# Memilih dua fitur utama untuk plot
svm_polynomial <- svm(Outcome ~ Age + Pregnancies, data = train, kernel = "polynomial", scale = TRUE)

# Plot model SVM untuk dua fitur
plot(svm_polynomial, train, Age ~ Pregnancies)

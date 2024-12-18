# Project-DAP
Analisis Prediksi Diabetes Menggunakan Metode Support Vector Machine pada Dataset PIMA Indian

# Menyimpan data dan melihat data
diabetes <- read.csv("C:/Users/Asus/Documents/PROJEK CCIT SEMESTER 3/DAP/diabetes.csv")
View(diabetes)
head(diabetes)
dim(diabetes)
str(diabetes)

# Melihat nilai 0 dalam data
any(diabetes$Pregnancies == 0)
any(diabetes$Glucose == 0)
any(diabetes$BloodPressure == 0)
any(diabetes$SkinThickness == 0)
any(diabetes$Insulin == 0)
any(diabetes$BMI == 0)
any(diabetes$DiabetesPedigreeFunction == 0)
any(diabetes$Age == 0)
any(diabetes$Outcome == 0)

# Cek nilai NA
anyNA(diabetes)

# Distribusi data
hist(diabetes$Pregnancies)
hist(diabetes$Glucose)
hist(diabetes$BloodPressure)
hist(diabetes$SkinThickness)
hist(diabetes$Insulin)
hist(diabetes$BMI)
hist(diabetes$DiabetesPedigreeFunction)
hist(diabetes$Age)
hist(diabetes$Outcome)

# Melihat outliers
boxplot(diabetes)

# Menghitung batas IQR untuk setiap kolom numerik dan menghapus outliers
remove_outliers <- function(diabetes) {
  # Menggunakan dataset asli untuk menyimpan hasil pembersihan
  cleaned_data <- diabetes
  
  # Loop untuk setiap kolom dalam dataset
  for (col in names(cleaned_data)) {
    # Memeriksa apakah kolom bertipe numerik
    if (is.numeric(cleaned_data[[col]])) {
      # Menghitung kuartil pertama (Q1) dan kuartil ketiga (Q3)
      Q1 <- quantile(cleaned_data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(cleaned_data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Menentukan batas bawah dan batas atas untuk outliers
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      # Menghapus baris yang memiliki nilai di luar batas outliers
      cleaned_data <- cleaned_data[cleaned_data[[col]] >= lower_bound & cleaned_data[[col]] <= upper_bound, ]
    }
  }
  
  return(cleaned_data)
}

# Menerapkan fungsi pada dataset
diabetes_cleaned <- remove_outliers(diabetes)

# Memeriksa nilai 0
any(diabetes_cleaned$Glucose == 0)
any(diabetes_cleaned$BloodPressure == 0)
any(diabetes_cleaned$BMI == 0)
any(diabetes_cleaned$SkinThickness == 0)
any(diabetes_cleaned$Insulin == 0)

# Memasukkan nilai median kolom SkinThickness ke variabel (menggantikan nilai yang 0)
med_skt <- median(diabetes_cleaned$SkinThickness[diabetes_cleaned$SkinThickness != 0], na.rm = TRUE)
med_skt
diabetes_cleaned$SkinThickness[diabetes_cleaned$SkinThickness == 0] <- med_skt
diabetes_cleaned$SkinThickness <- round(diabetes_cleaned$SkinThickness, 1)
any(diabetes_cleaned$SkinThickness == 0)

# Memasukkan nilai median kolom Insulin ke variabel (menggantikan nilai yang 0)
med_ins <- median(diabetes_cleaned$Insulin[diabetes_cleaned$Insulin != 0], na.rm = TRUE)
med_ins
diabetes_cleaned$Insulin[diabetes_cleaned$Insulin == 0] <- med_ins
diabetes_cleaned$Insulin <- round(diabetes_cleaned$Insulin, 1)
any(diabetes_cleaned$Insulin == 0)

# Melihat data
View(diabetes_cleaned)
boxplot(diabetes_cleaned)

# Memasukkan Fungsi Normalisasi Min-Max ke Variabel "normalize"
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Menerapkan Fungsi "normalize ke beberapa kolom Data diabetes
diabetes_cleaned$Glucose <- normalize(diabetes_cleaned$Glucose)
diabetes_cleaned$BloodPressure <- normalize(diabetes_cleaned$BloodPressure)
diabetes_cleaned$SkinThickness <- normalize(diabetes_cleaned$SkinThickness)
diabetes_cleaned$Insulin <- normalize(diabetes_cleaned$Insulin)
diabetes_cleaned$DiabetesPedigreeFunction <- normalize(diabetes_cleaned$DiabetesPedigreeFunction)
diabetes_cleaned$Age <- normalize(diabetes_cleaned$Age)
diabetes_cleaned$BMI <- normalize(diabetes_cleaned$BMI)
diabetes_cleaned$Pregnancies <- normalize(diabetes_cleaned$Pregnancies)
diabetes_cleaned$Outcome <- normalize(diabetes_cleaned$Outcome)

# Cek Normalisasi
range(diabetes_cleaned$Glucose)
range(diabetes_cleaned$BloodPressure)
range(diabetes_cleaned$SkinThickness)
range(diabetes_cleaned$Insulin)
range(diabetes_cleaned$DiabetesPedigreeFunction)
range(diabetes_cleaned$Age)
range(diabetes_cleaned$BMI)
range(diabetes_cleaned$Pregnancies)
range(diabetes_cleaned$Outcome)

# Cek Korelasi
correlation_diabetes <- cor(diabetes_cleaned[1:9])
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
sample <- sample.split(diabetes_cleaned$Outcome, SplitRatio = 0.80)

# Membagi data menjadi data latih dan data uji
train <- subset(diabetes_cleaned, sample == TRUE)
head(train)
test <- subset(diabetes_cleaned, sample == FALSE)
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

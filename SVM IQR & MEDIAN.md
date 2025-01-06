# Analisis Prediksi Diabetes Menggunakan Metode Support Vector Machine pada Dataset PIMA Indian

## Latar Belakang
Support Vector Machine (SVM) adalah algoritma supervised learning yang digunakan untuk klasifikasi dan regresi. SVM bekerja dengan menemukan hyperplane terbaik yang memisahkan data ke dalam kategori yang berbeda. Algoritma ini sangat efektif untuk dataset berdimensi tinggi dan cocok untuk berbagai jenis kernel seperti linear, radial, dan polynomial.

Pada analisis ini, SVM diterapkan pada dataset diabetes untuk memprediksi kemungkinan diabetes berdasarkan fitur medis. Berikut adalah penjelasan langkah-langkah analisis yang dilakukan.

## Dataset Project

https://bit.ly/dataset-project-DAP-kel8

Data yang digunakan diambil dari platform Kaggle dengan judul Pima Indians Diabetes Database. Data ini memuat berbagai metrik kesehatan seperti gula darah (glucose), tekanan darah (blood pressure), ketebalan kulit trisep (skin thickness), hormon gula darah (insulin), indeks massa tubuh (BMI), jumlah kehamilan (pregnancies), usia (age), indikator riwayat diabetes (diabetes pedigree function), dan variabel target yang menentukan seseorang diabetes atau tidak (outcome).

## Exploratory Data Analysis (EDA)

### Menyimpan dan Melihat Data

```
diabetes <- read.csv("C:/Users/Asus/Documents/PROJEK CCIT SEMESTER 3/DAP/diabetes.csv")
View(diabetes)
head(diabetes)
dim(diabetes)
str(diabetes)
```

Penjelasan:
- read.csv() digunakan untuk membaca dataset.
- View() untuk menampilkan data secara tabel.
- head() untuk melihat beberapa baris pertama data.
- dim() menunjukkan ukuran data (baris dan kolom).
- str() memberikan rincian struktur dataset.

Nantinya akan terdapat output sebagai berikut:

![menyimpan dan melihat data](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/output%20menyimpan%20dan%20melihat%20data.png)

### Melihat Nilai 0 dalam Data dan Nilai NA

Tujuan melakukan proses ini yakni untuk memastikan bahwa dataset tidak mengandung nilai 0 atau nilai yang tidak valid karena keduanya dapat memengaruhi hasil analisis atau model yang dibuat.

```
any(diabetes$Pregnancies == 0)
any(diabetes$Glucose == 0)
any(diabetes$BloodPressure == 0)
any(diabetes$SkinThickness == 0)
any(diabetes$Insulin == 0)
any(diabetes$BMI == 0)
any(diabetes$DiabetesPedigreeFunction == 0)
any(diabetes$Age == 0)
any(diabetes$Outcome == 0)

anyNA(diabetes)
```

![Melihat nilai 0 dan nilai NA](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/cek%20nilai%200%20dan%20NA.png)

Penjelasan:
- any() memeriksa keberadaan nilai tertentu.
- anyNA() untuk mendeteksi nilai yang hilang (NA).

### Eksplorasi Data
Saat eksplorasi data kami melakukan beberapa visualisasi untuk memahami pola, distribusi, dan hubungan dalam dataset. Berikut penjelasannya:

#### 1. Menggunakan histogram
```
hist(diabetes$Pregnancies)
hist(diabetes$Glucose)
hist(diabetes$BloodPressure)
hist(diabetes$SkinThickness)
hist(diabetes$Insulin)
hist(diabetes$BMI)
hist(diabetes$DiabetesPedigreeFunction)
hist(diabetes$Age)
hist(diabetes$Outcome)
```
![Melihat distribusi data memakai histogram](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/distribusi%20data%20(2).png)

#### 2. Menggunakan density plot
```
# Membuat density plot untuk semua variabel
# Normalisasi Min-Max untuk semua kolom
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

diabetes_normalized <- diabetes %>%
  mutate(across(c(Pregnancies, Glucose, BloodPressure, SkinThickness, 
                  Insulin, BMI, DiabetesPedigreeFunction, Age), normalize))

diabetes_long_normalized <- diabetes_normalized %>%
  pivot_longer(cols = c(Pregnancies, Glucose, BloodPressure, SkinThickness, 
                        Insulin, BMI, DiabetesPedigreeFunction, Age, Outcome),
               names_to = "Variable", values_to = "Value")

ggplot(diabetes_long_normalized, aes(x = Value, color = Variable, fill = Variable)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Combined Density Plot (Normalized with Adjusted Scale)",
    x = "Normalized Value",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +  
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5))    
```

![Melihat density plot campuran](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/density%20plot%20full%20kolom.png)

```
# Membuat density plot untuk setiap variabel
ggplot(diabetes, aes(x = Pregnancies)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot - Pregnancies", x = "Pregnancies", y = "Density") +
  theme_minimal()

ggplot(diabetes, aes(x = Glucose)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Density Plot - Glucose", x = "Glucose", y = "Density") +
  theme_minimal()

ggplot(diabetes, aes(x = BloodPressure)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot - Blood Pressure", x = "Blood Pressure", y = "Density") +
  theme_minimal()

ggplot(diabetes, aes(x = SkinThickness)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Density Plot - Skin Thickness", x = "Skin Thickness", y = "Density") +
  theme_minimal()

ggplot(diabetes, aes(x = Insulin)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Density Plot - Insulin", x = "Insulin", y = "Density") +
  theme_minimal()

ggplot(diabetes, aes(x = BMI)) +
  geom_density(fill = "pink", alpha = 0.5) +
  labs(title = "Density Plot - BMI", x = "BMI", y = "Density") +
  theme_minimal()

ggplot(diabetes, aes(x = DiabetesPedigreeFunction)) +
  geom_density(fill = "cyan", alpha = 0.5) +
  labs(title = "Density Plot - Diabetes Pedigree Function", x = "Diabetes Pedigree Function", y = "Density") +
  theme_minimal()

ggplot(diabetes, aes(x = Age)) +
  geom_density(fill = "yellow", alpha = 0.5) +
  labs(title = "Density Plot - Age", x = "Age", y = "Density") +
  theme_minimal()

ggplot(diabetes, aes(x = Outcome)) +
  geom_density(fill = "gray", alpha = 0.5) +
  labs(title = "Density Plot - Outcome", x = "Outcome", y = "Density") +
  theme_minimal()
```
![Melihat density plot grid](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/DENSITY%20PLOT.png)

#### 3. Menggunakan box plot
```
boxplot(diabetes)
```

![Melihat blox plot](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/boxplot%20diabes%20new%20no%20revisi.png)

### Melihat Standar Deviasi

Standar deviasi berguna untuk membantu memahami seberapa tersebarnya data, menilai apakah data konsisten atau bervariasi, serta mengidentifikasi pola atau anomali dalam data. Pemeriksaan standar deviasi dilakukan sebanyak dua kali, yakni sebelum data diisi dan sesudah data diisi.

```
sd_after_filling <- sapply(diabetes[, 1:8], function(col) {
  if (is.numeric(col)) {
    return(sd(col, na.rm = TRUE))
  }
})

cat("\nStandar Deviasi Dataset Utuh (masih ada nilai 0):\n")
print(sd_after_filling)
```

![Melihat sd before filling data](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/sd%20before%20filling%20data.png)

### Fill Data

Dalam dataset yang dianalisis, terdapat kolom seperti Glucose (gula darah), Blood Pressure (tekanan darah), Skin Thickness (ketebalan lipatan kulit trisep), Insulin (hormon pengatur gula darah), dan BMI (Body Mass Index). Beberapa nilai pada kolom-kolom tersebut tercatat sebagai 0.

Nilai-nilai 0 tersebut kemungkinan besar merupakan missing values atau kesalahan dalam pengisian data. Oleh karena itu, nilai-nilai tersebut perlu ditangani dengan metode imputasi atau strategi lainnya. Pada project ini kami melakukan imputasi nilai 0 menggunakan Interquartile Range (IQR) dan Median. Berikut penjelasannya.

#### 1. Penggunaan interquartile range (IQR)

```
remove_outliers <- function(diabetes) {
  cleaned_data <- diabetes
  
  for (col in names(cleaned_data)) {
    if (is.numeric(cleaned_data[[col]])) {
      Q1 <- quantile(cleaned_data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(cleaned_data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      cleaned_data <- cleaned_data[cleaned_data[[col]] >= lower_bound & cleaned_data[[col]] <= upper_bound, ]
    }
  }
  
  return(cleaned_data)
}

diabetes_cleaned <- remove_outliers(diabetes)
```

Alasan menggunakan IQR adalah untuk mengatasi sebaran data yang skew. Berdasarkan plot distribusi, terlihat bahwa beberapa kolom memiliki distribusi condong ke kanan atau ke kiri (skewed). Salah satu penyebab skewness ini adalah keberadaan nilai 0, yang secara statistik tidak valid pada beberapa kolom. Oleh karena itu, nilai 0 pada kolom-kolom tersebut diisi menggunakan IQR untuk menangani skewness.

Setelah menggunakan IQR, selanjutnya adalah memeriksa nilai 0 pada data. Ini dilakukan untuk memastikan data sudah terisi.

```
any(diabetes_cleaned$Glucose == 0)
any(diabetes_cleaned$BloodPressure == 0)
any(diabetes_cleaned$BMI == 0)
any(diabetes_cleaned$SkinThickness == 0)
any(diabetes_cleaned$Insulin == 0)
```

Setelah proses pemeriksaan terhadap data, ternyata masih ada data yang memiliki nilai 0, berikut output dari kode diatas.

![memeriksa nilai 0 after IQR](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/memeriksa%20nilai%20after%20IQR.png)

Ternyata nilai SkinThickness dan Insulin masih terdapat nilai 0. Selanjutnya kami mengisi data dua kolom tersebut dengan median.

#### 2. Penggunaan median

Proses imputasi nilai 0 dengan median memastikan distribusi data tetap stabil tanpa bias dari nilai ekstrim.

```
med_skt <- median(diabetes_cleaned$SkinThickness[diabetes_cleaned$SkinThickness != 0], na.rm = TRUE)
med_skt
diabetes_cleaned$SkinThickness[diabetes_cleaned$SkinThickness == 0] <- med_skt
diabetes_cleaned$SkinThickness <- round(diabetes_cleaned$SkinThickness, 1)
any(diabetes_cleaned$SkinThickness == 0)

med_ins <- median(diabetes_cleaned$Insulin[diabetes_cleaned$Insulin != 0], na.rm = TRUE)
med_ins
diabetes_cleaned$Insulin[diabetes_cleaned$Insulin == 0] <- med_ins
diabetes_cleaned$Insulin <- round(diabetes_cleaned$Insulin, 1)
any(diabetes_cleaned$Insulin == 0)
```

![output median](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/isi%20dengan%20median.png)


### Pemeriksaan Standar Deviasi Setelah Filling Data

```
sd_after_filling <- sapply(diabetes_cleaned[, 1:8], function(col) {
  if (is.numeric(col)) {
    return(sd(col, na.rm = TRUE))
  }
})

cat("\nStandar Deviasi Setelah Filling (dengan nilai 0 diisi):\n")
print(sd_after_filling)
```

![Standar Deviasi After Filling Data](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/sd%20after%20filling.png)

Terdapat perbedaan antara output standar deviasi sebelum dan sesudah fill data, perbedaan ini menunjukkan bahwa proses filling data dapat memperbaiki distribusi data dengan mengurangi penyimpangan yang disebabkan oleh nilai-nilai 0. Dataset yang telah diimputasi lebih siap digunakan untuk analisis lebih lanjut.

### Menerapkan Normalisasi ke dalam Data

```
diabetes_cleaned$Glucose <- normalize(diabetes_cleaned$Glucose)
diabetes_cleaned$BloodPressure <- normalize(diabetes_cleaned$BloodPressure)
diabetes_cleaned$SkinThickness <- normalize(diabetes_cleaned$SkinThickness)
diabetes_cleaned$Insulin <- normalize(diabetes_cleaned$Insulin)
diabetes_cleaned$DiabetesPedigreeFunction <- normalize(diabetes_cleaned$DiabetesPedigreeFunction)
diabetes_cleaned$Age <- normalize(diabetes_cleaned$Age)
diabetes_cleaned$BMI <- normalize(diabetes_cleaned$BMI)
diabetes_cleaned$Pregnancies <- normalize(diabetes_cleaned$Pregnancies)
diabetes_cleaned$Outcome <- normalize(diabetes_cleaned$Outcome)
```

Proses normalisasi sudah dilakukan diatas, sekarang saatnya menerapkan fungsi "normalize" ke beberapa kolom dalam data diabetes.

### Memeriksa Normalisasi

```
range(diabetes_cleaned$Glucose)
range(diabetes_cleaned$BloodPressure)
range(diabetes_cleaned$SkinThickness)
range(diabetes_cleaned$Insulin)
range(diabetes_cleaned$DiabetesPedigreeFunction)
range(diabetes_cleaned$Age)
range(diabetes_cleaned$BMI)
range(diabetes_cleaned$Pregnancies)
range(diabetes_cleaned$Outcome)
```

![memerikas normalisasi](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/cek%20normalisasi.png)

Proses ini bertujuan untuk memastikan bahwa semua kolom dalam dataset memiliki skala yang sama melalui proses normalisasi.

### Cek Korelasi

```
correlation_diabetes <- cor(diabetes_cleaned[1:9])
correlation_diabetes
```

Melakukan korelasi membantu memahami hubungan antara variabel dan memilih fitur penting. Korelasi dilakukan karena dapat berguna untuk memahami faktor-faktor yang memengaruhi kemungkinan seseorang memiliki diabetes.

Berikut hasil dari korelasi menggunakan heatmap plot.

```
library(ggcorrplot)
ggcorrplot(
  correlation_diabetes,
  lab = TRUE,                  # Menampilkan angka pada heatmap
  lab_size = 3,                # Ukuran angka
  colors = c("blue", "white", "red"), # Skema warna (opsional)
  title = "Heatmap Korelasi Diabetes", # Judul heatmap
  ggtheme = theme_minimal()    # Tema visualisasi
)
```

![hasil korelasi menggunakan heatmap plot](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/heatmap%20plot%20korelasi.png)


## TRAIN & TEST

```
library(caTools)
set.seed(123)

sample <- sample.split(diabetes_cleaned$Outcome, SplitRatio = 0.80)

train <- subset(diabetes_cleaned, sample == TRUE)
head(train)
test <- subset(diabetes_cleaned, sample == FALSE)
head(test)

cat("Dimensi Data Latih:", dim(train), "\n")
cat("Dimensi Data Uji:", dim(test), "\n")

train$Outcome <- as.factor(train$Outcome)
test$Outcome <- as.factor(test$Outcome)
```

![train and test](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/train%20and%20test.png)

Bagian ini digunakan untuk membagi dataset menjadi dua bagian: data latih (train) dan data uji (test) dengan rasio 80:20. Data latih akan digunakan untuk melatih model, sedangkan data uji akan digunakan untuk menguji performa model.

Dimensi data latih dan data uji ditampilkan untuk memverifikasi pembagian. Kolom Outcome pada kedua dataset dikonversi menjadi tipe factor untuk mempermudah proses modelling. Setelah ini, data siap digunakan untuk membangun dan menguji model.


## SVM Modelling

```
library(e1071)
svm_linear <- svm(Outcome ~ ., data = train, kernel = "linear", scale = TRUE)
svm_radial <- svm(Outcome ~ ., data = train, kernel = "radial", scale = TRUE)
svm_polynomial <- svm(Outcome ~ ., data = train, kernel = "polynomial", scale = TRUE)

cat("Ringkasan Model SVM Linear:\n")
summary(svm_linear)

cat("\nRingkasan Model SVM Radial:\n")
summary(svm_radial)

cat("\nRingkasan Model SVM Polynomial:\n")
summary(svm_polynomial)
```

Setelah membagi data tahap selanjutnya adalah membangun 3 model SVM dengan kernel yang berbeda (Linear, Radial, dan Polynomial). Tujuannya adalah untuk mengevaluasi mana yang paling sesuai dengan data.

Berikut output dari ringkasan model linear:

![Ringkasan Model SVM Linear](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/ringkasan%20svm%20linear.png)

Berikut output dari ringkasan model radial:

![Ringkasan Model SVM Radial](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/ringkasan%20svm%20radial.png)

Berikut output dari ringkasan model polynomial:

![Ringkasan Model SVM Polynomial](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/ringkasan%20svm%20polynomial.png)


## Prediction

```
predictions_linear <- predict(svm_linear, test)
predictions_radial <- predict(svm_radial, test)
predictions_polynomial <- predict(svm_polynomial, test)
```

Berikutnya adalah melakukan prediksi menggunakan model SVM yang telah dibuat. predict() digunakan untuk membuat prediksi menggunakan model SVM linear yang sebelumnya telah dilatih. Data yang digunakan untuk prediksi adalah dataset test, yang berisi data uji (data yang tidak digunakan untuk melatih model). Prediksi nilai Outcome (0 atau 1) untuk setiap baris dalam dataset test berdasarkan model. Hasil disimpan dalam variabel predictions_linear. 


## Evaluasi Model

Bagian ini digunakan untuk mengevaluasi performa model SVM (Linear, Radial, dan Polynomial) menggunakan:

- Confusion Matrix: Memberikan detail jumlah prediksi benar dan salah.
- Akurasi: Menunjukkan seberapa akurat model dalam memprediksi data uji.

### 1. Confusion Matrix

```
confusion_matrix_linear <- table(Predicted = predictions_linear, Actual = test$Outcome)
confusion_matrix_radial <- table(Predicted = predictions_radial, Actual = test$Outcome)
confusion_matrix_polynomial <- table(Predicted = predictions_polynomial, Actual = test$Outcome)

cat("\nConfusion Matrix untuk Model SVM Linear:\n")
print(confusion_matrix_linear)

cat("\nConfusion Matrix untuk Model SVM Radial:\n")
print(confusion_matrix_radial)

cat("\nConfusion Matrix untuk Model SVM Polynomial:\n")
print(confusion_matrix_polynomial)
```

Berikut output confusion matrix dari model linear:

![Confusion Matrix Linear](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/confusion%20matrix%20linear.png)

Berikut output confusion matrix dari model radial:

![Confusion Matrix Radial](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/confusion%20matrix%20radial.png)

Berikut output confusion matrix dari model polynomial:

![Confusion Matrix Polynomial](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/confusion%20matrix%20polynomial.png)

### 2. Accuracy

```
accuracy_linear <- sum(diag(confusion_matrix_linear)) / sum(confusion_matrix_linear)
accuracy_radial <- sum(diag(confusion_matrix_radial)) / sum(confusion_matrix_radial)
accuracy_polynomial <- sum(diag(confusion_matrix_polynomial)) / sum(confusion_matrix_polynomial)

cat("\nAkurasi Model SVM Linear:", accuracy_linear, "\n")
cat("Akurasi Model SVM Radial:", accuracy_radial, "\n")
cat("Akurasi Model SVM Polynomial:", accuracy_polynomial, "\n")
```

Berikut output akurasi yang dihasilkan oleh ketiga model:

![Output akurasi dari ketiga model](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/hasil%20akurasi.png)

## Visualization

```
svm_polynomial <- svm(Outcome ~ Age + Pregnancies, data = train, kernel = "polynomial", scale = TRUE)

plot(svm_polynomial, train, Age ~ Pregnancies)
```

Sebelum melakukan visualisasi, kami lebih dulu memilih dua fitur utama untuk plot. Fitur ini diambil dengan cara melihat heatmap plot hasil korelasi. Jadi fitur tersebut diambil berdasarkan kolom yang memiliki korelasi yang besar. Berikut visualisasi dari ketiga model.

Visualisasi model linear:

![Plot visualisasi Linear](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/plot%20linear/KUMPULAN%20PLOT%20SVM%20LINEAR.png)

Visualisasi model radial:

![Plot Visualisasi Radial](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/plot%20radial/KUMPULAN%20PLOT%20SVM%20RADIAL.png)

Visualisasi model polynomial:

![Plot Visualisasi Polynomial](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/plot%20polynomial/KUMPULAN%20PLOT%20SVM%20POLYNOMIAL.png)


## Kesimpulan

Support Vector Machines (SVM) dapat digunakan untuk menganalisa dan memprediksi hasil diagnosis diabetes dengan akurasi yang cukup baik. 

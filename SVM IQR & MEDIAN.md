# Analisis Prediksi Diabetes Menggunakan Metode Support Vector Machine pada Dataset PIMA Indian

## Latar Belakang
Support Vector Machine (SVM) adalah algoritma supervised learning yang digunakan untuk klasifikasi dan regresi. SVM bekerja dengan menemukan hyperplane terbaik yang memisahkan data ke dalam kategori yang berbeda. Algoritma ini sangat efektif untuk dataset berdimensi tinggi dan cocok untuk berbagai jenis kernel seperti linear, radial, dan polynomial.

Pada analisis ini, SVM diterapkan pada dataset diabetes untuk memprediksi kemungkinan diabetes berdasarkan fitur medis. Berikut adalah penjelasan langkah-langkah analisis yang dilakukan.

## Dataset project

Pima Indians Diabetes Database:

https://bit.ly/dataset-project-DAP-kel8

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

# Terapkan normalisasi pada semua kolom kecuali kolom 'Outcome' jika tipe faktor
diabetes_normalized <- diabetes %>%
  mutate(across(c(Pregnancies, Glucose, BloodPressure, SkinThickness, 
                  Insulin, BMI, DiabetesPedigreeFunction, Age), normalize))

# Ubah ke format panjang
diabetes_long_normalized <- diabetes_normalized %>%
  pivot_longer(cols = c(Pregnancies, Glucose, BloodPressure, SkinThickness, 
                        Insulin, BMI, DiabetesPedigreeFunction, Age, Outcome),
               names_to = "Variable", values_to = "Value")

# Buat density plot gabungan setelah normalisasi
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
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +  # Rentang X (0-1) dengan interval 0.1
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5))    # Rentang Y (0-5) dengan interval 0.5
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


#### Melihat Standar Deviasi

Standar deviasi berguna untuk membantu memahami seberapa tersebar data, menilai apakah data konsisten atau bervariasi, serta mengidentifikasi pola atau anomali dalam data. Pemeriksaan standar deviasi dilakukan sebanyak dua kali, yakni sebelum masuk ke dalam filling data dan sesudah data diisi.

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

Nilai-nilai 0 tersebut kemungkinan besar merupakan missing values atau kesalahan dalam pengisian data. Oleh karena itu, nilai-nilai tersebut perlu ditangani dengan metode imputasi atau strategi lainnya.


#### Penggunaan Interquartile Range (IQR)

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

Setelah menggunakan IQR, selanjutnya adalah memeriksa nilai 0 pada data. Ini dilakukan untuk memastika data sudah terisi.

```
any(diabetes_cleaned$Glucose == 0)
any(diabetes_cleaned$BloodPressure == 0)
any(diabetes_cleaned$BMI == 0)
any(diabetes_cleaned$SkinThickness == 0)
any(diabetes_cleaned$Insulin == 0)
```

Setelah proses pemeriksaan terhadap data, ternyata masih ada data yang memiliki nilai 0, berikut output dar kode diatas.

![memeriksa nilai 0 after IQR](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/memeriksa%20nilai%20after%20IQR.png)

Ternyata nilai SkinThickness dan Insulin masih terdapat nilai 0. Selanjutnya kami mengisi data dua kolom tersebut dengan median.

#### Penggunaan Median

```
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
```

![output median](https://raw.githubusercontent.com/erisamayla/Project-DAP/refs/heads/main/assets/isi%20dengan%20median.png)

Setelah memastikan data bebas dari outlier signifikan, kolom Skin Thickness dan Insulin dapat diisi dengan median. Proses ini memastikan distribusi data tetap stabil tanpa bias dari nilai ekstrim.

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

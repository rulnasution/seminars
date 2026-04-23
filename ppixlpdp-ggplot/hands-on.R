# =========================================================
# MATERI HANDS-ON ggplot2
# BAGIAN A. CONTOH LENGKAP
# BAGIAN B. LATIHAN
# Semua komentar dalam bahasa Indonesia
# =========================================================

# =========================================================
# BAGIAN A. CONTOH LENGKAP
# =========================================================

# -------------------------
# 1. Package
# -------------------------

# hapus komen jika perlu diinstall 
# install.packages(c("ggplot2", "dplyr", "scales")) 

library(ggplot2)
library(dplyr)
library(scales)

# -------------------------
# 2. Theme yang lebih bersih
# -------------------------
# Prinsip storytelling with data yang dipakai di sini:
# 1. Judul harus menyampaikan pesan, bukan hanya menyebut jenis grafik.
# 2. Kurangi elemen yang tidak perlu.
# 3. Gunakan warna seperlunya.
# 4. Urutkan kategori jika itu membantu perbandingan.
# 5. Tambahkan anotasi bila memang membantu pembaca.

theme_swd <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# =========================================================
# DATA PREPARATION
# =========================================================

data <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/refs/heads/main/data/sovi_data.csv")

data <- data %>%
  mutate(
    DISTRICTCODE = as.character(DISTRICTCODE),
    first_digit = substr(DISTRICTCODE, 1, 1),
    PULAU = case_when(
      first_digit %in% c("1", "2") ~ "Sumatera",
      first_digit == "3" ~ "Jawa",
      first_digit == "5" ~ "Bali dan Nusa Tenggara",
      first_digit == "6" ~ "Kalimantan",
      first_digit == "7" ~ "Sulawesi",
      first_digit %in% c("8", "9") ~ "Maluku dan Papua",
      TRUE ~ "Lainnya"
    )
  )

data$PULAU <- factor(data$PULAU, c("Sumatera", "Jawa", "Bali dan Nusa Tenggara", "Kalimantan",
                                   "Sulawesi", "Maluku dan Papua"))

# =========================================================
# CONTOH 1. BOXPLOT YANG LEBIH KOMUNIKATIF
# =========================================================

# Kenapa ini lebih baik?
# - kategori diurutkan, jadi perbandingan lebih mudah
# - warna dibuat netral, jadi fokus ke distribusi
# - judul menyampaikan pesan, bukan hanya "box plot"

p1 <- ggplot(data, aes(x = PULAU, y = POVERTY)) +
  geom_boxplot(fill = "grey85", colour = "grey30", outlier.alpha = 0.4) +
  # coord_flip() + # tambahkan ini jika ingin sumbunya dibalik
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Sebaran tingkat kemiskinan terlihat berbeda antar pulau",
    subtitle = "Pulau diurutkan berdasarkan kode wilayah agar perbandingan lebih mudah dibaca",
    x = NULL,
    y = "Tingkat kemiskinan"
  ) +
  theme_swd

print(p1)

# Coba bandingkan dengan Histogram berdasarkan kelompok berikut

p1_hist <- ggplot(data, aes(x = POVERTY, fill=PULAU)) +
  geom_histogram(colour='black') +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Sebaran tingkat kemiskinan terlihat berbeda antar pulau",
    subtitle = "Pulau diurutkan berdasarkan kode wilayah agar perbandingan lebih mudah dibaca",
    x = NULL,
    y = "Tingkat kemiskinan"
  ) +
  theme_swd

print(p1_hist)

# =========================================================
# CONTOH 2. SCATTER PLOT DENGAN FACET
# =========================================================

# Kenapa pakai facet?
# - kalau kelompok banyak, satu panel sering terlalu padat
# - facet membantu membandingkan pola tanpa gangguan warna berlebihan

p2 <- ggplot(data, aes(x = log(POPULATION), y = POVERTY)) +
  geom_point(alpha = 0.6, size = 1.8, colour = "grey40") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, colour = "black") +
  facet_wrap(~PULAU, ncol = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Sebaran log(Populasi) vs Tingkat Kemiskinan berdasarkan pulau",
    subtitle = "Facet dipilih agar setiap kelompok dapat dibandingkan tanpa tumpang tindih warna",
    x = "Log jumlah penduduk",
    y = "Tingkat kemiskinan"
  ) +
  theme_swd

print(p2)

# Coba bandingkan dengan Scatterplot dengan warna sebagai pengganti facet

p2 <- ggplot(data, aes(x = log(POPULATION), y = POVERTY, colour = PULAU)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Sebaran log(Populasi) vs Tingkat Kemiskinan",
    subtitle = "Sebaran dengan banyak kategori yang tercampur cenderung susah dibaca",
    x = "log(Populasi)",
    y = "Tingkat kemiskinan"
  ) +
  theme_swd

print(p2)

# =========================================================
# CONTOH 3. TIME SERIES DENGAN AIRPASSENGERS
# =========================================================

# AirPassengers adalah data bawaan R.
# Data ini bagus untuk demo karena tren naik dan pola musimannya jelas.
ap <- data.frame(
  tanggal = seq(as.Date("1949-01-01"), by = "month", length.out = length(AirPassengers)),
  penumpang = as.numeric(AirPassengers)
)

akhir <- ap[nrow(ap), ]

# Kenapa ini lebih baik?
# - hanya satu garis, jadi pesan utama jelas
# - anotasi langsung menunjukkan insight yang ingin dibaca audiens
# - judul menuliskan cerita, bukan hanya nama chart
p3 <- ggplot(ap, aes(x = tanggal, y = penumpang)) +
  geom_line(linewidth = 0.8, colour = "grey30") +
  geom_point(data = akhir, size = 2.2) +
  annotate(
    "text",
    x = akhir$tanggal,
    y = akhir$penumpang + 20,
    label = "Akhir seri jauh lebih tinggi\n daripada awal seri",
    hjust = 1,
    size = 4
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Jumlah penumpang pesawat meningkat tajam, dengan pola musiman yang berulang",
    subtitle = "Anotasi singkat membantu audiens langsung melihat pesan utama",
    x = "Tahun",
    y = "Jumlah penumpang"
  ) +
  theme_swd

print(p3)

# =========================================================
# BAGIAN B. LATIHAN
# =========================================================

# Catatan untuk peserta:
# 1. Isi semua bagian yang bertanda ____
# 2. Jangan berhenti di syntax. Pikirkan juga pesan yang mau disampaikan.
# 3. Coba ikuti kaidah yang sudah dicontohkan di atas.

# =========================================================
# LATIHAN 1. BOXPLOT TINGKAT KEMISKINAN ANTAR PULAU
# =========================================================

# Tugas:
# - isi judul dengan kalimat yang menyampaikan pesan
# - isi nama sumbu y
# - pertahankan kategori terurut dan tampilan bersih

ggplot(data, aes(x = ____, y = ____)) +
  geom_boxplot(fill = "grey85", colour = "grey30", outlier.alpha = 0.4) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "____",
    subtitle = "Pulau diurutkan berdasarkan kode wilayah agar perbandingan lebih mudah dibaca",
    x = NULL,
    y = "____"
  ) +
  theme_swd

# =========================================================
# LATIHAN 2. SCATTER PLOT DENGAN FACET
# =========================================================

# Tugas:
# - isi variabel sumbu x dan y
# - gunakan facet per pulau
# - isi judul yang menjelaskan mengapa facet membantu
# - isi nama sumbu x dan y

ggplot(data, aes(x = ____, y = ____)) +
  geom_point(alpha = 0.6, size = 1.8, colour = "grey40") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, colour = "black") +
  facet_wrap(~____, ncol = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "____",
    subtitle = "Pisahkan kelompok jika satu panel terasa terlalu padat",
    x = "____",
    y = "____"
  ) +
  theme_swd

# =========================================================
# LATIHAN 3. TIME SERIES DENGAN AIRPASSENGERS
# =========================================================

# Tugas:
# - isi nama variabel tanggal dan jumlah penumpang
# - isi judul yang menceritakan insight utama
# - isi label anotasi pada titik terakhir
# - isi nama sumbu x dan y

ggplot(ap, aes(x = ____, y = ____)) +
  geom_line(linewidth = 0.8, colour = "grey30") +
  geom_point(data = akhir, size = 2.2) +
  annotate(
    "text",
    x = akhir$tanggal,
    y = akhir$penumpang + 20,
    label = "____",
    hjust = 1,
    size = 4
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "____",
    subtitle = "Gunakan anotasi singkat untuk menekankan pesan utama",
    x = "____",
    y = "____"
  ) +
  theme_swd

# =========================================================
# SILAHKAN DICOBA DI RUMAH
# =========================================================

# Perbaiki visual time series European Stocks dengan cara berikut:
# 1. Buat judul.
# 2. Perjelas nama sumbu.
# 3. Tambahkan satu anotasi lagi pada bagian awal atau tengah seri.
# 4. Jelaskan dalam satu kalimat kenapa visual ini lebih baik dibanding line chart biasa.

eu <- data.frame(
  time_num = as.numeric(time(EuStockMarkets)),
  DAX = EuStockMarkets[,1],
  SMI = EuStockMarkets[,2],
  CAC = EuStockMarkets[,3],
  FTSE = EuStockMarkets[,4]
)

eu$date <- as.Date(paste0(floor(eu$time_num), "-01-01")) +
  round((eu$time_num - floor(eu$time_num)) * 365)

head(eu)

ggplot(eu, aes(x = date)) +
  geom_line(aes(y = ____, colour = "Jerman (DAX)"), linewidth = 1) +
  geom_line(aes(y = ____, colour = "Swiss (SMI)"), linewidth = 1) +
  geom_line(aes(y = ____, colour = "Prancis (CAC)"), linewidth = 1) +
  geom_line(aes(y = ____, colour = "Inggris (FTSE)"), linewidth = 1) +
  annotate(
    "text",
    x = ____,
    y = ____ + 20,
    label = "____",
    hjust = 1,
    size = 4
  ) +
  labs(
    title = "____",
    x = "____",
    y = "____",
    colour = "Negara"
  ) +
  scale_x_date(date_breaks = "____ year", date_labels = "%Y") +
  theme_swd


# =========================================================
# BAGIAN C. KUNCI JAWABAN
# =========================================================

# Jawaban latihan 1
ggplot(data, aes(x = PULAU, y = POVERTY)) +
  geom_boxplot(fill = "grey85", colour = "grey30", outlier.alpha = 0.4) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Sebaran tingkat kemiskinan terlihat berbeda antar pulau",
    subtitle = "Pulau diurutkan berdasarkan kode wilayah agar perbandingan lebih mudah dibaca",
    x = NULL,
    y = "Tingkat kemiskinan"
  ) +
  theme_swd

# Jawaban latihan 2
ggplot(data, aes(x = log(POPULATION), y = POVERTY)) +
  geom_point(alpha = 0.6, size = 1.8, colour = "grey40") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, colour = "black") +
  facet_wrap(~PULAU, ncol = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Memisahkan panel per pulau membuat pola hubungan lebih mudah dibaca",
    subtitle = "Pisahkan kelompok jika satu panel terasa terlalu padat",
    x = "Log jumlah penduduk",
    y = "Tingkat kemiskinan"
  ) +
  theme_swd

# Jawaban latihan 3
ggplot(ap, aes(x = tanggal, y = penumpang)) +
  geom_line(linewidth = 0.8, colour = "grey30") +
  geom_point(data = akhir, size = 2.2) +
  annotate(
    "text",
    x = akhir$tanggal,
    y = akhir$penumpang + 20,
    label = "Akhir seri jauh lebih tinggi\n daripada awal seri",
    hjust = 1,
    size = 4
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Jumlah penumpang pesawat meningkat tajam, dengan pola musiman yang berulang",
    subtitle = "Gunakan anotasi singkat untuk menekankan pesan utama",
    x = "Tahun",
    y = "Jumlah penumpang"
  ) +
  theme_swd


# Jawaban Latihan di Rumah

ggplot(eu, aes(x = date)) +
  geom_line(aes(y = DAX, colour = "Jerman (DAX)"), linewidth = 1) +
  geom_line(aes(y = SMI, colour = "Swiss (SMI)"), linewidth = 1) +
  geom_line(aes(y = CAC, colour = "Prancis (CAC)"), linewidth = 1) +
  geom_line(aes(y = FTSE, colour = "Inggris (FTSE)"), linewidth = 1) +
  geom_vline(xintercept = as.Date('1998-05-21'))+
  annotate(
    "text",
    x = as.Date('1998-05-10'),
    y = 1500,
    label = "Krisis Moneter\nGlobal",
    hjust = 1,
    size = 4
  ) +
  labs(
    title = "Pergerakan Indeks Saham Eropa",
    x = "Tanggal",
    y = "Nilai Indeks",
    colour = "Negara"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# =========================================================
# Selesai
# =========================================================

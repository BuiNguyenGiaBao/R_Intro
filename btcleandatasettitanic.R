# ================================================================
# Muc tieu:
#   1. Doc du lieu goc
#   2. Kiem tra cau truc du lieu
#   3. Lam sach ten cot va du lieu ky tu
#   4. Kiem tra duplicate
#   5. Phan tich missing values
#   6. Xu ly missing values theo logic hop ly
#   7. Kiem tra outlier co ban
#   8. Tao bien moi (feature engineering)
#   9. Chuan hoa kieu du lieu
#  10. Xuat file cleaned va file model-ready
# ================================================================

# ------------------------------
# 001. XOA BO DOI TUONG TRONG MOI TRUONG
# ------------------------------
# NOTE:
# - rm(list = ls()) giup moi truong sach hon.
rm(list = ls())

# ------------------------------
# 003. KHAI BAO FILE INPUT VA OUTPUT
# ------------------------------
input_file <- "train.csv"
output_clean_file <- "train_cleaned_300lines.csv"
output_model_file <- "train_model_ready_300lines.csv"
output_log_file   <- "train_cleaning_log.txt"

# ------------------------------
# 004. TAO FILE LOG DE GHI LAI CAC BUOC
# ------------------------------
# NOTE:
# - Ham cat(..., file = ..., append = TRUE) de ghi log.
# - Log giup minh chung minh quy trinh clean data ro rang.
if (file.exists(output_log_file)) {
  file.remove(output_log_file)
}

log_message <- function(...) {
  cat(..., "\n")
  cat(..., "\n", file = output_log_file, append = TRUE)
}

log_message("=== BAT DAU CLEAN DATA TITANIC ===")
log_message(paste("Input file:", input_file))
log_message(paste("Thoi diem chay:", Sys.time()))

# ------------------------------
# 005. KIEM TRA FILE CO TON TAI HAY KHONG
# ------------------------------
if (!file.exists(input_file)) {
  stop(paste("Khong tim thay file:", input_file))
}

log_message("Da xac nhan file input ton tai.")

# ------------------------------
# 006. DOC DU LIEU GOC
# ------------------------------
# NOTE:
# - stringsAsFactors = FALSE de tranh character bi doi thanh factor.
# - Day la lua chon an toan cho giai doan clean data.
df_raw <- read.csv(input_file, stringsAsFactors = FALSE)

# ------------------------------
# 007. TAO BAN SAO LAM VIEC
# ------------------------------
# NOTE:
# - Giup giu nguyen df_raw de doi chieu khi can.
df <- df_raw

# ------------------------------
# 008. IN KICH THUOC DU LIEU BAN DAU
# ------------------------------
log_message(paste("So dong ban dau:", nrow(df)))
log_message(paste("So cot ban dau:", ncol(df)))

# ------------------------------
# 009. XEM TEN CAC COT
# ------------------------------
log_message("Danh sach ten cot ban dau:")
log_message(paste(names(df), collapse = ", "))

# ------------------------------
# 010. CHUAN HOA TEN COT (NEU CAN)
# ------------------------------
# NOTE:
# - Titanic da co ten cot kha gon, nhung van nen trim khoang trang.
# - Neu du lieu khac co ten cot phuc tap, buoc nay rat huu ich.
names(df) <- trimws(names(df))

# ------------------------------
# 011. KIEM TRA KIEU DU LIEU TUNG COT
# ------------------------------
structure_info <- capture.output(str(df))
for (line in structure_info) {
  log_message(line)
}

# ------------------------------
# 012. THONG KE SO LUONG GIA TRI NA THEO COT
# ------------------------------
na_count_initial <- sapply(df, function(x) sum(is.na(x)))
log_message("So luong NA ban dau theo cot:")
for (nm in names(na_count_initial)) {
  log_message(paste(nm, ":", na_count_initial[[nm]]))
}

# ------------------------------
# 013. XAC DINH CAC COT CHARACTER
# ------------------------------
char_cols <- names(df)[sapply(df, is.character)]
log_message("Cac cot character:")
log_message(paste(char_cols, collapse = ", "))

# ------------------------------
# 014. TRIM KHOANG TRANG O DAU VA CUOI CHUOI
# ------------------------------
# NOTE:
# - Rat can thiet vi co nhung gia tri nhin giong nhau
#   nhung thuc ra khac nhau do thua khoang trang.
for (col in char_cols) {
  df[[col]] <- trimws(df[[col]])
}
log_message("Da trim khoang trang cho tat ca cot character.")

# ------------------------------
# 015. CHUYEN CHUOI RONG THANH NA
# ------------------------------
# NOTE:
# - "" va NA nen duoc thong nhat ve cung 1 dang thieu.
for (col in char_cols) {
  df[[col]][df[[col]] == ""] <- NA
}
log_message("Da doi chuoi rong thanh NA.")

# ------------------------------
# 016. KIEM TRA DUPLICATE TOAN BO DONG
# ------------------------------
dup_rows <- duplicated(df)
log_message(paste("So dong duplicate hoan toan:", sum(dup_rows)))

# ------------------------------
# 017. XOA DUPLICATE TOAN BO DONG
# ------------------------------
if (sum(dup_rows) > 0) {
  df <- df[!dup_rows, ]
}
log_message(paste("So dong sau khi xoa duplicate hoan toan:", nrow(df)))

# ------------------------------
# 018. KIEM TRA DUPLICATE THEO PassengerId
# ------------------------------
# NOTE:
# - PassengerId la khoa dinh danh tung hanh khach.
# - Neu trung lap, can giu 1 dong duy nhat.
dup_id <- duplicated(df$PassengerId)
log_message(paste("So PassengerId bi trung:", sum(dup_id, na.rm = TRUE)))

# ------------------------------
# 019. XOA DUPLICATE THEO PassengerId
# ------------------------------
if (sum(dup_id, na.rm = TRUE) > 0) {
  df <- df[!dup_id, ]
}
log_message(paste("So dong sau khi xoa trung PassengerId:", nrow(df)))

# ------------------------------
# 020. KIEM TRA CAC GIA TRI DANG SO NHUNG CO THE BAT THUONG
# ------------------------------
# NOTE:
# - O Titanic, cac cot so chinh la Survived, Pclass, Age, SibSp, Parch, Fare.
# - Buoc nay de soat logic co ban.
num_candidate <- c("Survived", "Pclass", "Age", "SibSp", "Parch", "Fare")
num_candidate <- num_candidate[num_candidate %in% names(df)]
log_message("Thong ke so hoc co ban:")
for (col in num_candidate) {
  sm <- summary(df[[col]])
  log_message(paste("---", col, "---"))
  for (i in seq_along(sm)) {
    log_message(paste(names(sm)[i], ":", sm[i]))
  }
}

# ------------------------------
# 021. KIEM TRA GIA TRI NGOAI PHAM VI CHO Survived
# ------------------------------
if ("Survived" %in% names(df)) {
  invalid_survived <- !(df$Survived %in% c(0, 1)) & !is.na(df$Survived)
  log_message(paste("So gia tri Survived khong hop le:", sum(invalid_survived)))
}

# ------------------------------
# 022. KIEM TRA GIA TRI NGOAI PHAM VI CHO Pclass
# ------------------------------
if ("Pclass" %in% names(df)) {
  invalid_pclass <- !(df$Pclass %in% c(1, 2, 3)) & !is.na(df$Pclass)
  log_message(paste("So gia tri Pclass khong hop le:", sum(invalid_pclass)))
}

# ------------------------------
# 023. KIEM TRA TUOI AM HOAC QUA BAT THUONG
# ------------------------------
if ("Age" %in% names(df)) {
  invalid_age <- (df$Age < 0 | df$Age > 100) & !is.na(df$Age)
  log_message(paste("So gia tri Age khong hop le (<0 hoac >100):", sum(invalid_age)))
}

# ------------------------------
# 024. KIEM TRA FARE AM
# ------------------------------
if ("Fare" %in% names(df)) {
  invalid_fare <- (df$Fare < 0) & !is.na(df$Fare)
  log_message(paste("So gia tri Fare am:", sum(invalid_fare)))
}

# ------------------------------
# 025. NEU CO Age BAT THUONG, DOI THANH NA
# ------------------------------
if ("Age" %in% names(df)) {
  bad_age_index <- which((df$Age < 0 | df$Age > 100) & !is.na(df$Age))
  if (length(bad_age_index) > 0) {
    df$Age[bad_age_index] <- NA
  }
  log_message("Da xu ly Age bat thuong thanh NA (neu co).")
}

# ------------------------------
# 026. NEU CO Fare AM, DOI THANH NA
# ------------------------------
if ("Fare" %in% names(df)) {
  bad_fare_index <- which((df$Fare < 0) & !is.na(df$Fare))
  if (length(bad_fare_index) > 0) {
    df$Fare[bad_fare_index] <- NA
  }
  log_message("Da xu ly Fare am thanh NA (neu co).")
}

# ------------------------------
# 027. TAO HAM LAY MODE CHO BIEN PHAN LOAI
# ------------------------------
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ------------------------------
# 028. TAO BIEN Title TU Name
# ------------------------------
# NOTE:
# - Day la buoc rat huu ich trong Titanic.
# - Vi danh xung (Mr, Mrs, Miss, Master...) lien quan den tuoi,
#   gioi tinh, tinh trang gia dinh va kha nang song sot.
df$Title <- NA
if ("Name" %in% names(df)) {
  df$Title <- sub(".*, ([A-Za-z]+)\\..*", "\\1", df$Name)
}
log_message("Da tao bien Title tu cot Name.")

# ------------------------------
# 029. XEM TAN SUAT CAC TITLE
# ------------------------------
if ("Title" %in% names(df)) {
  title_table <- sort(table(df$Title, useNA = "ifany"), decreasing = TRUE)
  log_message("Tan suat Title ban dau:")
  for (nm in names(title_table)) {
    log_message(paste(nm, ":", title_table[[nm]]))
  }
}

# ------------------------------
# 030. GOM CAC TITLE HIEM VE NHOM RARE
# ------------------------------
# NOTE:
# - Gop nhom de tranh qua nhieu muc phan loai.
# - Danh sach nay la cach gom pho bien trong Titanic.
rare_titles <- c("Lady", "Countess", "Capt", "Col", "Don", "Dr", "Major",
                 "Rev", "Sir", "Jonkheer", "Dona")
if ("Title" %in% names(df)) {
  df$Title[df$Title %in% rare_titles] <- "Rare"
  df$Title[df$Title %in% c("Mlle", "Ms")] <- "Miss"
  df$Title[df$Title %in% c("Mme")] <- "Mrs"
}
log_message("Da gom nhom title hiem ve Rare va chuan hoa mot so title.")

# ------------------------------
# 031. KIEM TRA LAI TAN SUAT TITLE SAU KHI GOM NHOM
# ------------------------------
if ("Title" %in% names(df)) {
  title_table2 <- sort(table(df$Title, useNA = "ifany"), decreasing = TRUE)
  log_message("Tan suat Title sau khi gom nhom:")
  for (nm in names(title_table2)) {
    log_message(paste(nm, ":", title_table2[[nm]]))
  }
}

# ------------------------------
# 032. TAO BIEN FamilySize
# ------------------------------
# NOTE:
# - Cong thuc pho bien: SibSp + Parch + 1
# - +1 vi tinh ca ban than hanh khach.
if (all(c("SibSp", "Parch") %in% names(df))) {
  df$FamilySize <- df$SibSp + df$Parch + 1
} else {
  df$FamilySize <- NA
}
log_message("Da tao bien FamilySize.")

# ------------------------------
# 033. TAO BIEN IsAlone
# ------------------------------
if ("FamilySize" %in% names(df)) {
  df$IsAlone <- ifelse(df$FamilySize == 1, 1, 0)
}
log_message("Da tao bien IsAlone.")

# ------------------------------
# 034. TAO BIEN Deck TU Cabin
# ------------------------------
# NOTE:
# - Cabin thieu rat nhieu, nen thuong khong imputing truc tiep.
# - Cach hay hon la tach ky tu dau lam Deck.
df$Deck <- NA
if ("Cabin" %in% names(df)) {
  df$Deck <- substr(df$Cabin, 1, 1)
  df$Deck[is.na(df$Cabin)] <- "Unknown"
}
log_message("Da tao bien Deck tu Cabin.")

# ------------------------------
# 035. CHUAN HOA Deck
# ------------------------------
if ("Deck" %in% names(df)) {
  df$Deck[df$Deck == ""] <- "Unknown"
}
log_message("Da chuan hoa Deck.")

# ------------------------------
# 036. TAO BIEN TicketPrefix
# ------------------------------
# NOTE:
# - Ticket co the chua thong tin prefix nhu PC, A/5, STON/O2...
# - Ta tach phan chu de co them thong tin phan loai.
df$TicketPrefix <- "NONE"
if ("Ticket" %in% names(df)) {
  temp_ticket <- gsub("[./]", "", df$Ticket)
  temp_ticket <- trimws(temp_ticket)
  temp_ticket <- strsplit(temp_ticket, " ")
  temp_prefix <- sapply(temp_ticket, function(x) {
    if (length(x) == 1) {
      return("NONE")
    } else {
      return(paste(x[-length(x)], collapse = ""))
    }
  })
  df$TicketPrefix <- temp_prefix
}
log_message("Da tao bien TicketPrefix.")

# ------------------------------
# 037. CHUAN HOA TicketPrefix VE CHU HOA
# ------------------------------
if ("TicketPrefix" %in% names(df)) {
  df$TicketPrefix <- toupper(df$TicketPrefix)
}
log_message("Da chuan hoa TicketPrefix thanh chu hoa.")

# ------------------------------
# 038. TAO BIEN TicketGroupSize
# ------------------------------
# NOTE:
# - Nhung nguoi co cung ticket co the di chung nhom.
# - Bien nay rat huu ich cho machine learning.
df$TicketGroupSize <- NA
if ("Ticket" %in% names(df)) {
  ticket_freq <- table(df$Ticket)
  df$TicketGroupSize <- as.numeric(ticket_freq[df$Ticket])
}
log_message("Da tao bien TicketGroupSize.")

# ------------------------------
# 039. KIEM TRA MISSING VALUES LAN 2
# ------------------------------
na_count_second <- sapply(df, function(x) sum(is.na(x)))
log_message("So luong NA truoc khi imputing:")
for (nm in names(na_count_second)) {
  log_message(paste(nm, ":", na_count_second[[nm]]))
}

# ------------------------------
# 040. XU LY MISSING CHO Embarked
# ------------------------------
# NOTE:
# - Embarked chi thieu rat it trong train.csv.
# - Cach thong dung: dien bang mode.
if ("Embarked" %in% names(df)) {
  embarked_mode <- get_mode(df$Embarked)
  df$Embarked[is.na(df$Embarked)] <- embarked_mode
  log_message(paste("Da dien Embarked bang mode:", embarked_mode))
}

# ------------------------------
# 041. XU LY MISSING CHO Fare
# ------------------------------
# NOTE:
# - Train.csv thuong khong thieu Fare, nhung script van xu ly de tong quat.
# - Dien bang median theo Pclass se hop ly hon median toan bo.
if ("Fare" %in% names(df) && "Pclass" %in% names(df)) {
  for (pc in sort(unique(df$Pclass))) {
    idx <- which(df$Pclass == pc & is.na(df$Fare))
    med_val <- median(df$Fare[df$Pclass == pc], na.rm = TRUE)
    if (length(idx) > 0 && is.finite(med_val)) {
      df$Fare[idx] <- med_val
    }
  }
  # Neu van con NA, dung median toan bo
  if (any(is.na(df$Fare))) {
    df$Fare[is.na(df$Fare)] <- median(df$Fare, na.rm = TRUE)
  }
  log_message("Da xu ly missing cho Fare.")
}

# ------------------------------
# 042. XU LY MISSING CHO Age THEO NHOM Title + Pclass
# ------------------------------
# NOTE:
# - Day la cach tot hon median tong the.
# - Vi tuoi lien quan manh toi danh xung va hang ve.
if (all(c("Age", "Title", "Pclass") %in% names(df))) {
  age_missing_before <- sum(is.na(df$Age))
  
  unique_title <- unique(df$Title)
  unique_pclass <- unique(df$Pclass)
  
  for (tt in unique_title) {
    for (pc in unique_pclass) {
      idx_group <- which(df$Title == tt & df$Pclass == pc)
      med_age <- median(df$Age[idx_group], na.rm = TRUE)
      idx_fill <- which(df$Title == tt & df$Pclass == pc & is.na(df$Age))
      if (length(idx_fill) > 0 && is.finite(med_age)) {
        df$Age[idx_fill] <- med_age
      }
    }
  }
  
  # Neu van con NA thi fill theo median cua Title
  if (any(is.na(df$Age))) {
    for (tt in unique_title) {
      idx_group <- which(df$Title == tt)
      med_age_title <- median(df$Age[idx_group], na.rm = TRUE)
      idx_fill <- which(df$Title == tt & is.na(df$Age))
      if (length(idx_fill) > 0 && is.finite(med_age_title)) {
        df$Age[idx_fill] <- med_age_title
      }
    }
  }
  
  # Neu van con thi fill median toan bo
  if (any(is.na(df$Age))) {
    df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)
  }
  
  age_missing_after <- sum(is.na(df$Age))
  log_message(paste("So Age thieu truoc imputing:", age_missing_before))
  log_message(paste("So Age thieu sau imputing:", age_missing_after))
}

# ------------------------------
# 043. XU LY MISSING CHO Deck / Cabin
# ------------------------------
# NOTE:
# - Khong nen dien Cabin cu the vi mat qua nhieu thong tin.
# - Giu Cabin nguyen ban va su dung Deck = Unknown cho phan thieu.
if ("Deck" %in% names(df)) {
  df$Deck[is.na(df$Deck)] <- "Unknown"
}
log_message("Da xu ly Deck missing ve Unknown.")

# ------------------------------
# 044. KIEM TRA MISSING SAU IMPUTING
# ------------------------------
na_count_after_impute <- sapply(df, function(x) sum(is.na(x)))
log_message("So luong NA sau imputing:")
for (nm in names(na_count_after_impute)) {
  log_message(paste(nm, ":", na_count_after_impute[[nm]]))
}

# ------------------------------
# 045. TAO BIEN AgeGroup
# ------------------------------
# NOTE:
# - Nhom tuoi giup phan tich de hon va co the huu ich cho model.
df$AgeGroup <- cut(
  df$Age,
  breaks = c(-Inf, 12, 18, 35, 60, Inf),
  labels = c("Child", "Teen", "YoungAdult", "Adult", "Senior"),
  right = TRUE
)
log_message("Da tao bien AgeGroup.")

# ------------------------------
# 046. TAO BIEN FareGroup THEO QUARTILE
# ------------------------------
# NOTE:
# - Dung quantile de chia muc gia ve.
fare_breaks <- quantile(df$Fare, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
fare_breaks <- unique(fare_breaks)
if (length(fare_breaks) >= 2) {
  df$FareGroup <- cut(df$Fare, breaks = fare_breaks, include.lowest = TRUE, dig.lab = 10)
} else {
  df$FareGroup <- NA
}
log_message("Da tao bien FareGroup.")

# ------------------------------
# 047. TAO BIEN FamilyCategory
# ------------------------------
# NOTE:
# - Chia theo quy mo gia dinh de quan sat xu huong.
df$FamilyCategory <- NA
if ("FamilySize" %in% names(df)) {
  df$FamilyCategory[df$FamilySize == 1] <- "Alone"
  df$FamilyCategory[df$FamilySize >= 2 & df$FamilySize <= 4] <- "Small"
  df$FamilyCategory[df$FamilySize >= 5] <- "Large"
}
log_message("Da tao bien FamilyCategory.")

# ------------------------------
# 048. KIEM TRA OUTLIER Age BANG IQR
# ------------------------------
# NOTE:
# - Outlier khong nhat thiet phai xoa.
# - O day minh chi danh dau de tham khao.
Q1_age <- quantile(df$Age, 0.25, na.rm = TRUE)
Q3_age <- quantile(df$Age, 0.75, na.rm = TRUE)
IQR_age <- Q3_age - Q1_age
lower_age <- Q1_age - 1.5 * IQR_age
upper_age <- Q3_age + 1.5 * IQR_age
age_outlier_flag <- ifelse(df$Age < lower_age | df$Age > upper_age, 1, 0)
df$AgeOutlierFlag <- age_outlier_flag
log_message(paste("So outlier Age theo IQR:", sum(df$AgeOutlierFlag, na.rm = TRUE)))

# ------------------------------
# 049. KIEM TRA OUTLIER Fare BANG IQR
# ------------------------------
Q1_fare <- quantile(df$Fare, 0.25, na.rm = TRUE)
Q3_fare <- quantile(df$Fare, 0.75, na.rm = TRUE)
IQR_fare <- Q3_fare - Q1_fare
lower_fare <- Q1_fare - 1.5 * IQR_fare
upper_fare <- Q3_fare + 1.5 * IQR_fare
fare_outlier_flag <- ifelse(df$Fare < lower_fare | df$Fare > upper_fare, 1, 0)
df$FareOutlierFlag <- fare_outlier_flag
log_message(paste("So outlier Fare theo IQR:", sum(df$FareOutlierFlag, na.rm = TRUE)))

# ------------------------------
# 050. LOG-TRANSFORM FARE DE GIAM LECH
# ------------------------------
# NOTE:
# - Gia ve thuong lech phai.
# - log1p(Fare) = log(1 + Fare), an toan khi Fare = 0.
df$FareLog <- log1p(df$Fare)
log_message("Da tao bien FareLog = log1p(Fare).")

# ------------------------------
# 051. TAO BIEN ChildFlag
# ------------------------------
df$ChildFlag <- ifelse(df$Age < 16, 1, 0)
log_message("Da tao bien ChildFlag.")

# ------------------------------
# 052. TAO BIEN MotherFlag
# ------------------------------
# NOTE:
# - Chi la bien heuristic (gan dung), khong phai xac dinh tuyet doi.
df$MotherFlag <- ifelse(
  df$Sex == "female" &
    df$Age > 18 &
    df$Parch > 0 &
    !(df$Title %in% c("Miss")),
  1,
  0
)
log_message("Da tao bien MotherFlag.")

# ------------------------------
# 053. TAO BIEN CabinKnown
# ------------------------------
df$CabinKnown <- ifelse(is.na(df$Cabin), 0, 1)
log_message("Da tao bien CabinKnown.")

# ------------------------------
# 054. TAO BIEN NameLength
# ------------------------------
df$NameLength <- ifelse(is.na(df$Name), NA, nchar(df$Name))
log_message("Da tao bien NameLength.")

# ------------------------------
# 055. TAO BIEN HasCabinLetterAtoG
# ------------------------------
# NOTE:
# - Giup kiem tra nhanh deck thuoc nhom nao.
df$HasCabinLetterAtoG <- ifelse(df$Deck %in% LETTERS[1:7], 1, 0)
log_message("Da tao bien HasCabinLetterAtoG.")

# ------------------------------
# 056. CHUAN HOA GIOI TINH VE CHU THUONG
# ------------------------------
if ("Sex" %in% names(df)) {
  df$Sex <- tolower(df$Sex)
}
log_message("Da chuan hoa cot Sex ve chu thuong.")

# ------------------------------
# 057. KIEM TRA GIA TRI DUY NHAT CUA Sex
# ------------------------------
if ("Sex" %in% names(df)) {
  log_message("Gia tri duy nhat cua Sex:")
  log_message(paste(sort(unique(df$Sex)), collapse = ", "))
}

# ------------------------------
# 058. CHUAN HOA Embarked VE CHU HOA
# ------------------------------
if ("Embarked" %in% names(df)) {
  df$Embarked <- toupper(df$Embarked)
}
log_message("Da chuan hoa Embarked ve chu hoa.")

# ------------------------------
# 059. KIEM TRA GIA TRI DUY NHAT CUA Embarked
# ------------------------------
if ("Embarked" %in% names(df)) {
  log_message("Gia tri duy nhat cua Embarked:")
  log_message(paste(sort(unique(df$Embarked)), collapse = ", "))
}

# ------------------------------
# 060. TAO BIEN PclassFactor
# ------------------------------
df$PclassFactor <- as.factor(df$Pclass)
log_message("Da tao bien PclassFactor.")

# ------------------------------
# 061. CHUYEN CAC BIEN NHOM THANH FACTOR CHO PHAN TICH
# ------------------------------
factor_cols <- c("Survived", "PclassFactor", "Sex", "Embarked", "Title",
                 "Deck", "AgeGroup", "FareGroup", "FamilyCategory")
for (col in factor_cols) {
  if (col %in% names(df)) {
    df[[col]] <- as.factor(df[[col]])
  }
}
log_message("Da chuyen mot so bien nhom thanh factor.")

# ------------------------------
# 062. KIEM TRA LAI CAU TRUC SAU KHI TAO BIEN
# ------------------------------
structure_info_2 <- capture.output(str(df))
for (line in structure_info_2) {
  log_message(line)
}

# ------------------------------
# 063. KIEM TRA SO LUONG GIA TRI DUY NHAT CHO TUNG COT
# ------------------------------
unique_count <- sapply(df, function(x) length(unique(x)))
log_message("So luong gia tri duy nhat moi cot:")
for (nm in names(unique_count)) {
  log_message(paste(nm, ":", unique_count[[nm]]))
}

# ------------------------------
# 064. THONG KE TAN SUAT CHO CAC BIEN PHAN LOAI CHINH
# ------------------------------
cat_vars_for_freq <- c("Sex", "Embarked", "Title", "Deck", "AgeGroup", "FamilyCategory")
for (col in cat_vars_for_freq) {
  if (col %in% names(df)) {
    tb <- table(df[[col]], useNA = "ifany")
    log_message(paste("Tan suat cua", col, ":"))
    for (nm in names(tb)) {
      log_message(paste(nm, ":", tb[[nm]]))
    }
  }
}

# ------------------------------
# 065. TAO BAO CAO TOM TAT SO HOC
# ------------------------------
num_cols_final <- names(df)[sapply(df, is.numeric)]
log_message("Danh sach cot numeric sau clean:")
log_message(paste(num_cols_final, collapse = ", "))

for (col in num_cols_final) {
  sm <- summary(df[[col]])
  log_message(paste("Thong ke summary cho cot:", col))
  for (i in seq_along(sm)) {
    log_message(paste(names(sm)[i], ":", sm[i]))
  }
}

# ------------------------------
# 066. KIEM TRA CAP NHAT MISSING VALUES CUOI CUNG
# ------------------------------
na_count_final <- sapply(df, function(x) sum(is.na(x)))
log_message("So luong NA cuoi cung theo cot:")
for (nm in names(na_count_final)) {
  log_message(paste(nm, ":", na_count_final[[nm]]))
}

# ------------------------------
# 067. TAO BIEN CHO MACHINE LEARNING: Sex_num
# ------------------------------
df$Sex_num <- ifelse(df$Sex == "male", 1,
                     ifelse(df$Sex == "female", 0, NA))
log_message("Da tao bien Sex_num.")

# ------------------------------
# 068. TAO BIEN Embarked_num
# ------------------------------
# NOTE:
# - Mapping thuong dung: C = 0, Q = 1, S = 2
# - Co the doi tuy thuoc mo hinh.
df$Embarked_num <- NA
if ("Embarked" %in% names(df)) {
  df$Embarked_num[df$Embarked == "C"] <- 0
  df$Embarked_num[df$Embarked == "Q"] <- 1
  df$Embarked_num[df$Embarked == "S"] <- 2
}
log_message("Da tao bien Embarked_num.")

# ------------------------------
# 069. TAO BIEN Title_num
# ------------------------------
# NOTE:
# - Mapping thu cong de de dua vao mo hinh don gian.
df$Title_num <- NA
if ("Title" %in% names(df)) {
  df$Title_num[df$Title == "Mr"] <- 1
  df$Title_num[df$Title == "Miss"] <- 2
  df$Title_num[df$Title == "Mrs"] <- 3
  df$Title_num[df$Title == "Master"] <- 4
  df$Title_num[df$Title == "Rare"] <- 5
}
log_message("Da tao bien Title_num.")

# ------------------------------
# 070. TAO BIEN Deck_num
# ------------------------------
df$Deck_num <- NA
if ("Deck" %in% names(df)) {
  deck_levels <- c("Unknown", "A", "B", "C", "D", "E", "F", "G", "T")
  for (i in seq_along(deck_levels)) {
    df$Deck_num[df$Deck == deck_levels[i]] <- i - 1
  }
}
log_message("Da tao bien Deck_num.")

# ------------------------------
# 071. TAO BIEN FamilySizeGroup_num
# ------------------------------
df$FamilySizeGroup_num <- NA
if ("FamilyCategory" %in% names(df)) {
  df$FamilySizeGroup_num[df$FamilyCategory == "Alone"] <- 0
  df$FamilySizeGroup_num[df$FamilyCategory == "Small"] <- 1
  df$FamilySizeGroup_num[df$FamilyCategory == "Large"] <- 2
}
log_message("Da tao bien FamilySizeGroup_num.")

# ------------------------------
# 072. SAP XEP DU LIEU THEO PassengerId CHO DE DOI CHIEU
# ------------------------------
if ("PassengerId" %in% names(df)) {
  df <- df[order(df$PassengerId), ]
  rownames(df) <- NULL
}
log_message("Da sap xep du lieu theo PassengerId.")

# ------------------------------
# 073. TAO BAN CLEANED DE LUU TOAN BO THONG TIN
# ------------------------------
df_cleaned <- df
log_message("Da tao df_cleaned.")


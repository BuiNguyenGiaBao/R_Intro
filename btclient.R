# Đọc dữ liệu
clients <- read.csv("clients.csv", stringsAsFactors = FALSE)

# Kiểm tra cấu trúc
str(clients)
sapply(clients, class)

# Xem NA ban đầu
colSums(is.na(clients))

# 3 tìm dòng bị lệch 
bad_rows <- clients$Marital_Status == "Cycle"
which(bad_rows)

clients[bad_rows, c("Education", "Marital_Status", "Income", "Kidhome", "Teenhome", "Dt_Customer")]

clients <- read.csv("clients.csv", stringsAsFactors = FALSE)

bad_rows <- clients$Marital_Status == "Cycle"

cols <- c(
  "Marital_Status", "Income", "Kidhome", "Teenhome", "Dt_Customer", "Recency",
  "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts",
  "MntSweetProducts", "MntGoldProds", "NumDealsPurchases", "NumWebPurchases",
  "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth",
  "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp1",
  "AcceptedCmp2", "Complain", "Z_CostContact", "Z_Revenue", "Response"
)

for (r in which(bad_rows)) {
  vals <- unlist(clients[r, cols], use.names = FALSE)
  
  clients$Education[r] <- "2n Cycle"
  clients[r, cols] <- c(vals[2:length(vals)], NA)
}

clients$Income <- as.numeric(clients$Income)
clients$Kidhome <- as.integer(clients$Kidhome)
clients$Teenhome[grepl("-", clients$Teenhome)] <- NA # nhằm tránh bị lỗi convert sang  na 
clients$Teenhome <- as.integer(clients$Teenhome)
clients$Recency <- as.integer(clients$Recency)
clients$Year_Birth <- as.numeric(clients$Year_Birth)
clients$MntWines <- as.numeric(clients$MntWines)
clients$Response <- as.integer(clients$Response)
clients$Dt_Customer <- as.Date(clients$Dt_Customer, format = "%d-%m-%Y")

#  3b điền giá trị bị thiếu 
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(
  median(clients$Year_Birth, na.rm = TRUE))

clients$Income[is.na(clients$Income)] <- median(clients$Income, na.rm = TRUE)

clients$MntWines[is.na(clients$MntWines)] <- median(clients$MntWines, na.rm = TRUE)

mode_response <- as.integer(names(sort(table(clients$Response), decreasing = TRUE)[1]))
clients$Response[is.na(clients$Response)] <- mode_response

# 3c. Đoạn mã để điền Year_Birth
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(
  median(clients$Year_Birth, na.rm = TRUE)
)

# 4. Kiểm tra lại tất cả giá trị thiếu đã được điền chưavà hiện các dòng có chứa na 
colSums(is.na(clients))
clients[!complete.cases(clients), ]

# 5a. các biến factor
factor_vars <- c("Marital_Status", "AcceptedCmp1", "AcceptedCmp2",
                 "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5",
                 "Complain", "Response")

# 5b. Đoạn mã ngắn nhất để chuyển Marital_Status thành factor
clients$Marital_Status <- factor(clients$Marital_Status)

# 6a: biến ordered factor
ordered_var <- "Education"
# 6b. Đoạn mã chuyển Education thành ordered factor
clients$Education <- factor(
  clients$Education,
  levels = c("Basic", "2n", "Graduation", "Master", "PhD"),
  ordered = TRUE
)

# 7. Chuyển các biến đã xác định thành lớp thích hợp
factor_vars <- c( 
  "Marital_Status", "AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3",
  "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")

clients[factor_vars] <- lapply(clients[factor_vars], factor)
# kiểm tra lại
str(clients)
sapply(clients, class)

# chuyen sang dinh dang 
save(clients, file = "clientsInR.RData")


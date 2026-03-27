library(tidyverse)
library(caret)
library(pROC)
install.packages(c("tidyverse", "caret", "pROC", "rsample", "yardstick"))

medical <- read.csv("medical_care.csv", stringsAsFactors = TRUE)
dim(medical)
head(medical)

# Kiểm tra phân phối biến mục tiêu
table(medical$UCURNINS)
prop.table(table(medical$UCURNINS))

set.seed(42)

# Tạo chỉ số cho tập training (70%)
train_idx <- createDataPartition(medical$UCURNINS, p = 0.7, list = FALSE)

train_data <- medical[train_idx, ]
test_data  <- medical[-train_idx, ]

cat("Kích thước tập train:", nrow(train_data), "\n")
cat("Kích thước tập test: ", nrow(test_data),  "\n")

# Kiểm tra tỷ lệ lớp trong từng tập
prop.table(table(train_data$UCURNINS))
prop.table(table(test_data$UCURNINS))

# tạo mô hình 
module_lr=<-glm(UCRINIS~ UMARSTAT+USATMED+ URELATED+REGION + FHOSP +
                  FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
                  U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
                data  = train_data,
                family = binomial(link = "logit"))
prob<-predict(module_lr,newdata=test_data)
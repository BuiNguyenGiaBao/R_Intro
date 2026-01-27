# 1. Bài tập về chuyển đổi kiểu dữ liệu số: 
# Tạo một số thập phân, sau đó chuyển đổi số đó thành số nguyên 
# rồi thành ký tự. Quan sát và giải thích sự thay đổi về giá trị 
# và cách hiển thị của số đó qua mỗi lần chuyển đổi.

# 2. Bài tập về ghép chuỗi: 
# Tạo hai biến chứa văn bản, tìm hiểu tài liệu về hàm paste() 
# và sử dụng nó để ghép các biến văn bản đã tạo. So sánh kết quả 
# của hàm paste() với hàm c() và giải thích sự khác biệt giữa chúng.

# 3. Bài tập về xử lý ngày tháng: 
# Cho vector vecDate <- c("09:12:12", "28:02:16", "31:05:22"). 
# Hãy: 
# a) Chuyển đổi vector này sang kiểu Date
# b) Tính số ngày giữa các ngày trong vector với ngày hiện tại.


# 4. Bài tập tạo vector số: 
# Tạo vector "vec1" chứa các số từ 2 đến 8 và từ 17 đến 30 
# bằng cách viết code ngắn gọn nhất có thể.


# 5. Bài tập sử dụng hàm seq(): 
# Tạo vector "vec2" có cấu trúc: (2, 8, 14, 20, 26, 32) 
# bằng cách sử dụng hàm seq().


# 6. Bài tập lặp chuỗi: 
# Tạo một vector có cấu trúc: "2", "7", "a", "2", "7", "a", 
# "2", "7", "a" bằng cách sử dụng hàm rep().

# 7. Bài tập về số chia hết: 
# Tạo một vector độ dài 100 chứa các số liên tiếp chia hết cho 3.


# 8. Bài tập tạo mẫu phức tạp: 
# Sử dụng một dòng code duy nhất để tạo vector "vec3" có cấu trúc: 
#   (1, 1, 3, 3, 5, 5, 7, 7, 9, 9) lặp lại 3 lần.


# 9. Bài tập về số ngẫu nhiên: 
# Tạo vector "vec4" gồm 50 số bằng hàm runif(). 
# Giải thích chức năng của hàm runif() và sử dụng các số đã tạo 
# để tạo vector mới chứa 50 số nguyên ngẫu nhiên trong khoảng 0-20.
# runif() tạo số ngẫu nhiên phân phối đều 


# 10. Bài tập truy xuất phần tử: 
# In ra các giá trị của phần tử thứ 5, 10 và 26 từ vector 
# vừa tạo ở câu 9


# 11. Bài tập về dãy có quy luật: 
# In ra giá trị của các phần tử cách đều nhau trong vector từ câu 9, 
# bắt đầu từ phần tử thứ 5 và lấy cứ mỗi phần tử thứ hai. 
# Gợi ý: Sử dụng hàm seq().

#######################################################################
#### SOLUTION ####
# 1. Bài tập về chuyển đổi kiểu dữ liệu số: 
# Tạo một số thập phân, sau đó chuyển đổi số đó thành số nguyên 
# rồi thành ký tự. Quan sát và giải thích sự thay đổi về giá trị 
# và cách hiển thị của số đó qua mỗi lần chuyển đổi.

# Tạo số thập phân
num <- 23.67
print(num)
class(num)

# Chuyển sang số nguyên
num_int <- as.integer(num)
class(num_int)


# 2. Bài tập về ghép chuỗi: 
# Tạo hai biến chứa văn bản, tìm hiểu tài liệu về hàm paste() 
# và sử dụng nó để ghép các biến văn bản đã tạo. So sánh kết quả 
# của hàm paste() với hàm c() và giải thích sự khác biệt giữa chúng.

text1 <- "Hello"
text2 <- "Le Nhat Tung"
help(paste)
help(paste0)

result1 <- paste(text1, text2)
result1

result2 <- paste(text1, text2, sep = "-")
result2

result3 <- paste("a", "b", "c", sep='->')
result3

result4 <- paste0(text1, text2)
result4

vectors_s <- c("a", "b", "c")
result5 <- paste(vectors_s)
result5

result5 <- paste(vectors_s, collapse = ", ")
result5

result6 <- paste(c("x", "y"), c("1", "2", "3", "4"), sep = "-", collapse = ";")
result6

result7 <- paste(c(), c("1", "2", "3", "4"), recycle0 = FALSE)
result7


# 3. Bài tập về xử lý ngày tháng: 
# Cho vector vecDate <- c("09:12:12", "28:02:16", "31:05:22"). 
# Hãy: 
# a) Chuyển đổi vector này sang kiểu Date
# b) Tính số ngày giữa các ngày trong vector với ngày hiện tại.
vecDate <- c("09:12:12", "28:02:16", "31:05:22")
class(vecDate)

dates <- as.Date(vecDate, format="%d:%m:%y");
print(dates)


# 4. Bài tập tạo vector số: 
# Tạo vector "vec1" chứa các số từ 2 đến 8 và từ 17 đến 30 
# bằng cách viết code ngắn gọn nhất có thể.

vec1 <- c(2:8, 17:30)
vec1

# 
# 5. Bài tập sử dụng hàm seq(): 
# Tạo vector "vec2" có cấu trúc: (2, 8, 14, 20, 26, 32) 
# bằng cách sử dụng hàm seq().

vec2 <- seq(from=2, to=32, by=6)
vec2


# 6. Bài tập lặp chuỗi: 
# Tạo một vector có cấu trúc: "2", "7", "a", "2", "7", "a", 
# "2", "7", "a" bằng cách sử dụng hàm rep().
result <- rep(c("2", "7", "a"), times=3)
result

# 7. Bài tập về số chia hết: 
# Tạo một vector độ dài 100 chứa các số liên tiếp chia hết cho 3.
vec3 <- seq(from=3, by=3, length.out=100)
vec3


# 8. Bài tập tạo mẫu phức tạp: 
# Sử dụng một dòng code duy nhất để tạo vector "vec3" có cấu trúc: 
#   (1, 1, 3, 3, 5, 5, 7, 7, 9, 9) lặp lại 3 lần.
rep(seq(from=1, to=9, by=2), each=2, times=3)


# 9. Bài tập về số ngẫu nhiên: 
# Tạo vector "vec4" gồm 50 số bằng hàm runif(). 
# Giải thích chức năng của hàm runif() và sử dụng các số đã tạo 
# để tạo vector mới chứa 50 số nguyên ngẫu nhiên trong khoảng 0-20.

# runif() tạo số ngẫu nhiên phân phối đều 
vec4 <- runif(50) # Mặc đinh trong khoảng [0,1]
vec4

vec4 <- (vec4*21)
vec4

vec4 <- floor(vec4)
vec4

# 10. Bài tập truy xuất phần tử: 
# In ra các giá trị của phần tử thứ 5, 10 và 26 từ vector 
# vừa tạo ở câu 9.
vec4[5]
vec4[c(5,10,26)]

# 11. Bài tập về dãy có quy luật: 
# In ra giá trị của các phần tử cách đều nhau trong vector từ câu 9, 
# bắt đầu từ phần tử thứ 5 và lấy cứ mỗi phần tử thứ hai. 
# Gợi ý: Sử dụng hàm seq().
indices <- seq(from=5, to=length(vec4), by=2)
result <- vec4[indices]
result

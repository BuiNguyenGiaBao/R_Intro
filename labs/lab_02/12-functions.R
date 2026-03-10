# I. Giới thiệu về Function
# Ví dụ Built-in function
numbers <- c(1, 2, 3, 4, 5)
mean(numbers)    # 3
sum(numbers)     # 15
length(numbers)  # 5


# Ví dụ User-defined function (sẽ học sau)
calculate_average <- function(x) {
  return(sum(x) / length(x))
}
calculate_average(numbers)  # 3

# 2 Cú pháp khai báo Function
# Cấu trúc cơ bản
# function_name <- function(parameter1, parameter2, ...) {
#   # Thân hàm (function body)
#   # Các câu lệnh xử lý
#   
#   return(result)  # Giá trị trả về (optional)
# }
# Các thành phần:
#   
# function_name: Tên của hàm (nên đặt tên có ý nghĩa)
# parameter1, parameter2: Các tham số đầu vào (có thể có hoặc không)
# function body: Khối lệnh thực thi
# return(): Trả về kết quả (không bắt buộc)


# II. Ví dụ
# Function không có tham số
say_hello <- function(){
  print("Hello")
}

say_hello()
say_hello()

# Function có tham số
say_hello_to <- function(name){
  message <- paste("Xin chào", name, "!");
  print(message)
}
say_hello_to("Tùng")
say_hello_to("A")

# Function có nhiều tham số
calculate_sum <- function(a, b) {
  result <- a + b
  return(result)
}

calculate_sum(5, 3)  # 8

# III. Tham số (Parameters)
# Tất cả tham số đều bắt buộc
divide <- function(a, b) {
  return(a / b)
}

divide(10, 2)   # 5
# divide(10)    # Lỗi! Thiếu tham số b

# 2 Tham số mặc định (Default Parameters)
# Tham số có giá trị mặc định
greet <- function(name, greeting = "Xin chào") {
  message <- paste(greeting, name, "!")
  return(message)
}

greet("Tùng")                    # "Xin chào Tùng !"
greet("Tùng", "Chào buổi sáng")  # "Chào buổi sáng Tùng !"

# Ví dụ thực tế: Tính lũy thừa
power <- function(base, exponent = 2) {
  return(base ^ exponent)
}

power(5)      # 25 (5^2)
power(5, 3)   # 125 (5^3)
power(2, 10)  # 1024 (2^10)

# 3 Truyền tham số theo tên
# Truyền tham số theo thứ tự
calculate_bmi <- function(weight, height) {
  bmi <- weight / (height ^ 2)
  return(bmi)
}

calculate_bmi(70, 1.75)  # 22.86

# Truyền tham số theo tên (không cần đúng thứ tự)
calculate_bmi(height = 1.75, weight = 70)  # 22.86

# Kết hợp
calculate_bmi(70, height = 1.75)  # 22.86

# 4 Tham số không giới hạn (...)
# Sử dụng ... để nhận số lượng tham số không xác định
calculate_average <- function(...) {
  values <- c(...)
  avg <- sum(values) / length(values)
  return(avg)
}

calculate_average(1, 2, 3)           # 2
calculate_average(10, 20, 30, 40)    # 25
calculate_average(5)                 # 5

# Ví dụ kết hợp với tham số cố định
print_info <- function(title, ...) {
  cat(title, ":\n")
  values <- c(...)
  for (val in values) {
    cat("-", val, "\n")
  }
}

print_info("Danh sách sinh viên", "Tùng", "Hùng", "Dũng")

# IV. Giá trị trả về (Return Value)
# 1 Sử dụng return()
# Trả về giá trị rõ ràng
square <- function(x) {
  result <- x * x
  return(result)
}

square(5)  # 25


# 2 Return ngầm định
# R tự động trả về biểu thức cuối cùng
square <- function(x) {
  x * x  # Không cần return
}

square(5)  # 25

# Ví dụ phức tạp hơn
get_grade <- function(score) {
  if (score >= 85) {
    "Xuất sắc"
  } else if (score >= 70) {
    "Giỏi"
  } else if (score >= 55) {
    "Khá"
  } else if (score >= 40) {
    "Trung bình"
  } else {
    "Yếu"
  }
}

get_grade(88)  # "Xuất sắc"
get_grade(65)  # "Khá"

# 3 Trả về nhiều giá trị
# Sử dụng list để trả về nhiều giá trị
calculate_stats <- function(numbers) {
  result <- list(
    mean = mean(numbers),
    median = median(numbers),
    sd = sd(numbers),
    min = min(numbers),
    max = max(numbers)
  )
  return(result)
}

scores <- c(75, 82, 68, 91, 77, 85, 73)
stats <- calculate_stats(scores)

print(stats$mean)    # 78.71
print(stats$median)  # 77
print(stats$sd)      # 7.89
print(stats$min)     # 68
print(stats$max)     # 91

# Hoặc sử dụng vector
get_min_max <- function(x) {
  return(c(min = min(x), max = max(x)))
}

result <- get_min_max(c(3, 7, 2, 9, 5))
print(result)
# min max 
#   2   9

# 4 Return sớm
# Sử dụng return() để thoát sớm
check_positive <- function(x) {
  if (x <= 0) {
    return("Số không dương")
  }
  
  # Code chỉ chạy khi x > 0
  result <- sqrt(x)
  return(paste("Căn bậc hai:", result))
}

check_positive(-5)  # "Số không dương"
check_positive(16)  # "Căn bậc hai: 4"

# V Phạm vi biến (Variable Scope)
# 1 Biến cục bộ (Local Variables)
# Biến trong function chỉ tồn tại trong function
test_function <- function() {
  local_var <- "Tôi là biến cục bộ"
  print(local_var)
}

test_function()  # "Tôi là biến cục bộ"

# print(local_var)  # Lỗi! local_var không tồn tại bên ngoài
# 2 Biến toàn cục (Global Variables)
# Biến toàn cục
global_var <- 100

use_global <- function() {
  print(global_var)  # Có thể đọc biến toàn cục
}

use_global()  # 100

# Sửa đổi biến toàn cục (không khuyến khích)
modify_global <- function() {
  global_var <<- 200  # Sử dụng <<- để gán cho biến toàn cục
}

print(global_var)    # 100
modify_global()
print(global_var)    # 200
3.5.3 Ví dụ về phạm vi biến
x <- 10  # Biến toàn cục

demo_scope <- function() {
  x <- 20  # Biến cục bộ (khác với biến toàn cục)
  print(paste("Trong function:", x))
}

demo_scope()         # "Trong function: 20"
print(paste("Ngoài function:", x))  # "Ngoài function: 10"

# x toàn cục không bị thay đổi

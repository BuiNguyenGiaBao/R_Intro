prime_numbers(30)
pascal_triangle(6)
student_classification(7.8)
x <- c(3, 5, 7, 9, NA)
summary_stats(x)
permutation(6, 2)
combination(6, 2)
scores <- c(8, 7.5, 9, NA, 6)
average_score(scores)
rectangle_area(10, 5)
circle_perimeter(3)
celsius_to_fahrenheit(30)

#bt1
# tinhs chu vi hinhf chuwx nhaatj 
rectangle_area <- function(length, width) {
  area <- length * width
  return(area)}
# chu vi hinh tron
circle_perimeter <- function(radius) {
  perimeter <- 2 * pi * radius
  return(perimeter)
}
# chuyen tu celsius thanh fahrenheit
celsius_to_fahrenheit <- function(celsius) {
  fahrenheit <- celsius * 9/5 + 32
  return(fahrenheit)}
#bt2
#kiemtra chang le 
check_even_odd <- function(x) {
  
  # kiểm tra có phải số không
  if (!is.numeric(x)) {
    stop("Input phải là số.")
  }
  
  # kiểm tra có phải số nguyên không
  if (x %% 1 != 0) {
    stop("Input phải là số nguyên.")
  }
  
  # kiểm tra chẵn lẻ
  if (x %% 2 == 0) {
    return("Chẵn")
  } else {
    return("Lẻ")
  }
}
#kiem tra trung binh 
average_score <- function(scores) {
  
  # kiểm tra input là numeric
  if (!is.numeric(scores)) {
    stop("Điểm phải là số.")
  }
  
  # loại bỏ NA
  scores <- scores[!is.na(scores)]
  
  # kiểm tra khoảng điểm
  if (any(scores < 0 | scores > 10)) {
    stop("Điểm phải nằm trong khoảng 0 đến 10.")
  }
  
  # tính trung bình
  mean_score <- mean(scores)
  
  return(mean_score)
}
#br3
#tinh toan thong ke tong quan
summary_stats <- function(x) {
  if (!is.numeric(x)) {
    stop("Input phải là vector số.")}
  # loại bỏ NA
  x <- x[!is.na(x)]
  
  result <- list(
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    min = min(x),
    max = max(x),
    range = max(x) - min(x))
  return(result)
}
#baitoan hoan vi 
permutation <- function(n, r) {
  if (n < r) {
    stop("n phải lớn hơn hoặc bằng r")
  }
  result <- factorial(n) / factorial(n - r)
  return(result)
}
# bai taon to hop c 
combination <- function(n, r) {
  if (n < r) {
    stop("n phải lớn hơn hoặc bằng r")
  }
  result <- factorial(n) / (factorial(r) * factorial(n - r))
  return(result)
}
#bt4
# function tu 1 den n
prime_numbers <- function(n) {
  
  primes <- c()
  
  for (num in 2:n) {
    is_prime <- TRUE
    
    for (i in 2:sqrt(num)) {
      if (num %% i == 0) {
        is_prime <- FALSE
        break
      }
    }
    
    if (is_prime) {
      primes <- c(primes, num)
    }
  }
  
  return(primes)
}
#function tinh tam giac
pascal_triangle <- function(n) {
  for (i in 0:(n-1)) {
    row <- c()
    
    for (j in 0:i) {
      value <- choose(i, j)
      row <- c(row, value)}
    print(row)
  }
}
#bt5
net_salary(10000000, 2000000, 24, 10)
net_salary <- function(base_salary, allowance, work_days, overtime_hours) {
  # lương theo ngày
  daily_salary <- base_salary / 26
  # lương theo giờ
  hourly_salary <- daily_salary / 8
  # lương làm việc
  work_salary <- daily_salary * work_days
  # lương tăng ca (150%)
  overtime_salary <- overtime_hours * hourly_salary * 1.5
  # tổng thu nhập
  gross_salary <- work_salary + allowance + overtime_salary
  # thuế 10%
  tax <- gross_salary * 0.10
  # lương ròng
  net_salary <- gross_salary - tax
  return(net_salary)
}
#chuan hoa diem thi
scores <- c(5, 6, 7, 8, 9)
normalize_scores(scores)
normalize_scores <- function(scores) {
  min_score <- min(scores, na.rm = TRUE)
  max_score <- max(scores, na.rm = TRUE)
  normalized <- (scores - min_score) / (max_score - min_score) * 100
  return(normalized)}
#phan tich du lieu snh vien
students <- data.frame(
  ten = c("An", "Binh", "Chi", "Dung"),
  tuoi = c(20, 21, 19, 22),
  diem = c(8.5, 7.0, 9.0, 6.5))

student_analysis(students)

student_analysis <- function(df) {
  result <- list(
    total_students = nrow(df),
    average_age = mean(df$tuoi, na.rm = TRUE),
    average_score = mean(df$diem, na.rm = TRUE),
    min_score = min(df$diem, na.rm = TRUE),
    max_score = max(df$diem, na.rm = TRUE),
    sd_score = sd(df$diem, na.rm = TRUE))
  return(result)
}

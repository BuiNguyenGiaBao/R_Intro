
#-------------- BÀI TẬP 1 ----------------------
subjects <- c("Toán", "Lý", "Hóa", "Văn", "Anh")
scores <- c(8, 7.5, 9, 8.5, 7)

bp <- barplot(scores,
              names.arg = subjects,
              horiz = TRUE,
              col = c("skyblue","black","green","purple","yellow"),
              main = "Điểm thi của bạn",
              xlab = "Điểm",
              las = 1)

text(scores, bp,
     labels = scores,
     pos = 4)
#------------- BÀI TẬP 2 ----------------------
set.seed(2024)
exam_scores <- rnorm(100, mean = 70, sd = 10)
par(mfrow = c(1,2))

hist(exam_scores,
     main='điểm kiểm tra',
     col ='blue',
     xlab= 'số người đạt',
     ylab='điểm kiểm tra'
     )

abline(v = mean(exam_scores),
       col = "red",
       lwd = 2)

hist(exam_scores,
     main='điểm kiểm tra',
     col ='blue',
     xlab= 'số người đạt',
     ylab='điểm kiểm tra',
     breaks=20
)
#------------ Bài Tập 3 ----------------------
data("iris")

boxplot(Petal.Length ~ Species,
        data = iris,
        col = c("green","pink","gray"),
        main = "Petal Length phần phân phối bởi Iris Species",
        xlab = "Species",
        ylab = "Petal Length",
        border = "black")

# loài có petal dài nhất là virginica
#loài có độ biến thiên lớn nhất là virginica
#loài có outliner là versicolor


#-------------- Bài tập 4 ----------------------

plot(mtcars$hp, mtcars$mpg,
     col = c("blue","red","brown")[as.factor(mtcars$cyl)],
     pch = 19,
     main = "Mối quan hệ giữa Horsepower và MPG",
     xlab = "Horsepower (hp)",
     ylab = "Miles per Gallon (mpg)")
# hồi quy 
abline(lm(mpg ~ hp, data = mtcars),
       col = "black",
       lwd = 2)
legend("topright",
       legend = c("4 cylinders", "6 cylinders", "8 cylinders"),
       col = c("blue", "green", "red"),
       pch = 19,
       title = "Số xy-lanh")

# theo tĩ lệ nghịch xe có nhiều horse power = lượng xăng tiêu tốn

#-------------- Bài Tập 5 -----------------------

par(mfrow = c(2, 2))

# 1. Histogram của hp
hist(mtcars$hp,
     main = "Histogram of hp",
     xlab = "Horsepower (hp)",
     col = "skyblue",
     border = "black")

# 2. Box plot của hp
boxplot(mtcars$hp,
        main = "Boxplot  hcảup",
        ylab = "Horsepower (hp)",
        col = "lightgreen")

# 3. Box plot so sánh hp theo cyl
boxplot(hp ~ cyl,
        data = mtcars,
        main = "số hp  của Cylinders",
        xlab = "Cylinders",
        ylab = "Horsepower (hp)",
        col = c("orange", "pink", "lightblue"))

# 4. Scatter plot hp vs mpg
plot(mtcars$hp, mtcars$mpg,
     main = "hp vs mpg",
     xlab = "Horsepower (hp)",
     ylab = "Miles per Gallon (mpg)",
     pch = 19,
     col = "red")

abline(lm(mpg ~ hp, data = mtcars),
       col = "black",
       lwd = 2)

#------------ Bài Tập 6 --------------
Q1 <- c(100, 120, 110, 130)
Q2 <- c(150, 140, 160, 155)
Q3 <- c(180, 170, 190, 185)
Q4 <- c(200, 210, 195, 220)

products <- c("Sản phẩm A", "Sản phẩm B", "Sản phẩm C", "Sản phẩm D")

# tạo data frame
sales <- rbind(Q1, Q2, Q3, Q4)

colnames(sales) <- products
rownames(sales) <- c("Q1","Q2","Q3","Q4")

# tính tổng
total_quarter <- rowSums(sales)
total_product <- colSums(sales)

# chia figure 2x2
par(mfrow = c(2,2))

# (1) Grouped bar chart
barplot(sales,
        beside = TRUE,
        col = c("skyblue","orange","lightgreen","pink"),
        legend.text = rownames(sales),
        args.legend = list(x="topleft"),
        main = "Grouped Bar Chart",
        xlab = "Sản phẩm",
        ylab = "Doanh thu")

# (2) Line plot
matplot(sales,
        type = "b",
        pch = 19,
        lty = 1,
        col = c("blue","red","green","purple"),
        xaxt = "n",
        xlab = "Quý",
        ylab = "Doanh thu",
        main = "Line Plot các sản phẩm")

axis(1, at = 1:4, labels = rownames(sales))

legend("topleft",
       legend = products,
       col = c("blue","red","green","purple"),
       pch = 19,
       lty = 1)

# (3) Pie chart tổng doanh thu mỗi quý
pie(total_quarter,
    labels = rownames(sales),
    col = c("skyblue","orange","lightgreen","pink"),
    main = "Tổng doanh thu mỗi quý")

# (4) Bar chart tổng doanh thu mỗi sản phẩm
barplot(total_product,
        names.arg = products,
        col = c("blue","red","green","purple"),
        main = "Tổng doanh thu mỗi sản phẩm",
        xlab = "Sản phẩm",
        ylab = "Doanh thu")

# trở về mặc định
par(mfrow = c(1,1))



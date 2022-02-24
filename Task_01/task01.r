#install.packages ("swirl")
library (swirl)
library(pkg)
swirl ()
mamatried89
5+7
x <- 5+7
x
y <- x-3
y
z <- c(1.1, 9, 3.14)
?c
c(z, 555, z)
z * 2 + 100
my_sqrt <- sqrt(z - 1)
print(my_sqrt)
my_sqrt
my_div <- z / my_sqrt
1
3
1
my_div
c(1, 2, 3) + c(0,10)
c(1, 2, 3, 4) + c(0,10)
c(1, 2, 3, 4) + c(0, 10, 100)
1000
library (swirl)
swirl ()
getwd ()
ls ()
x <- 9
ls ()
dir ()
?list.files
args ()
args (list.files)
old.dir <- dir()
old.dir <- getwd ()
dir.create("testdir")
dir.create (getwd)
dir.create()
setwd ("testdir")
file.create ("mytest.R")
dir ()
file.exists ("mytest.R")
file.info ("mytest.R")
file.rename ("mytest2.R")
file.rename ("mytest.R","mytest2.R")
file.copy ("mytest2.R","mytest3.R")
file.path ("mytest3.R")
file.path ("mytest3.R",'folder1','folder2')
file.path ("folder1","folder2")
?dir.create
dir.create(file.path("testdir2","testdir3"), recursive = TRUE)
setwd(old.dir)
swirl ()
1:20
pi:10
?`:`
seq (1, 20)
seq (0, 10, by=0.5)
seq (5, 10, length=30)
my_seq <- seq (5, 10, length=30)
length (my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0, times = 40)
rep(c(0,1,2), times = 10)
rep(c(0,1,2), each = 10)
x <- c(44, NA, 5, NA)
x*3
y <- rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y, z), 100)
my_na <- is.na(my_data)
my_na
my_data == NA
sum(my_na)
my_data
0/0
Inf - Inf
x
x[1:10]
x[is.na(x)]
y <- x[!is.na(x)]
y
y[y>0]
x[x>0]
x[!is.na(x) & x > 0]
x[c(3,5,7)]
x[0]
x[3000]
x[c(-2,-10)]
x[-c(2,10)]
vect <- c(foo = 11, bar = 2, norf= NA)
vect
names(vect)
vect2 <- c(11,2,NA)
names(vect2) <- c("foo", "bar", "norf")
identical(vect,vect2)
vect["bar"]
vect[c("foo", "bar")]
my_vector <- 1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector) <- c(4,5)
dim(my_vector)
attributes(my_vector)
my_vector
?matrix
my_matrix2 <- matrix((1:20), nrow = 4, ncol = 5)
identical(my_matrix, my_matrix2)
nxt()
patients <- c("Bill", "Gina", "Kelly", "Sean")
cbind(patients, my_matrix)
my_data <- data.frame(patients, my_matrix)
my_data
class(my_data)
cnames <- c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames
my_data
TRUE == TRUE
(FALSE == TRUE) == FALSE
6 == 7
6<7
10<=10
5!=7
!5=!7
!5==7
FALSE & FALSE
TRUE || c(TRUE, FALSE, FALSE)
5 > 8 || 6 != 8 && 4 >3.9
isTRUE(6>4)
identical('twins', 'twins')
xor(5 == 6, !FALSE)
ints <- sample(10)
ints
ints > 5
which(ints > 7)
any(ints < 0)
all(ints > 0)
Sys.Date()
data(cars)
?cars
head(cars)
plot(cars)
?plot
plot(x = cars$speed, y = cars$dist, ylab = "Stopping Distance")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance")
plot(cars, main = "My Plot")
plot(cars, sub = "My Plot Subtitle")
plot(cars, col = 2)
plot(cars, xlim = c(10,15))
plot(cars, pch = 2)
data(mtcars)
?boxplot
boxplot(formula = mpg ~ cyl, data = mtcars)
hist(mtcars$mpg)
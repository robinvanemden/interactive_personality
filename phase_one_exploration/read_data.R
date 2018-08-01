library(readr)
library(tidyr)
library(animation)
library(scatterplot3d)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

delete_na_rows <- function(DF, dart = c('NA')) {
  dirty_rows <- apply(DF, 1, function(r)
    !any(r %in% dart))
  DF <- DF[dirty_rows, ]
}

# ## In this R script, I analyze one of the experiments in the diadic social moment experiment

# ## First, lets read the csv of one experiment

# set working directory
setwd("C:/Users/robin/Desktop/Kinect/R_Analysis")
# Read CSV into R
# data <- read_csv(file = 'data/16091997a.csv')

#approx_entropy(rnorm(500), edim = 2)              # 1.351439  high, random
#approx_entropy(sin(seq(1,100,by=0.2)), edim = 2)  # 0.171806  low,  deterministic

data <- read_csv(file = 'data/2014b.csv')   #introv  0.5079269
#data <- read_csv(file = 'data/KOALA7b.csv') #introv 0.3064028
#data <- read_csv(file = 'data/IFRAb.csv')   #introv  0.1888351
#data <- read_csv(file = 'data/1808b.csv')   #introv  0.4638657



#data <- read_csv(file = 'data/Londonb.csv')   #extrav  0.2288509
#data <- read_csv(file = 'data/16091997b.csv')   #extrav  0.2288509
#data <- read_csv(file = 'data/Poarb.csv')   #extrav  0.1034042
data <- read_csv(file = 'data/MAZAb.csv')    #extrav 0.1764344

# ## Now lets take a look at the movements of one of the participant's Heads


data = separate(
  data,
  col = c("Body_1_XYZ_Head"),
  into = c(
    "Body_1_XYZ_Head_X",
    "Body_1_XYZ_Head_Y",
    "Body_1_XYZ_Head_Z",
    "Delete"
  ),
  sep = ";"
)

data = separate(
  data,
  col = c("Body_2_XYZ_Head"),
  into = c(
    "Body_2_XYZ_Head_X",
    "Body_2_XYZ_Head_Y",
    "Body_2_XYZ_Head_Z",
    "Delete"
  ),
  sep = ";"
)


data$Body_1_XYZ_Head_X = as.double(data$Body_1_XYZ_Head_X) * 1000
data$Body_1_XYZ_Head_Y = as.double(data$Body_1_XYZ_Head_Y) * 1000
data$Body_1_XYZ_Head_Z = as.double(data$Body_1_XYZ_Head_Z) * 1000
data$Body_2_XYZ_Head_X = as.double(data$Body_2_XYZ_Head_X) * 1000
data$Body_2_XYZ_Head_Y = as.double(data$Body_2_XYZ_Head_Y) * 1000
data$Body_2_XYZ_Head_Z = as.double(data$Body_2_XYZ_Head_Z) * 1000


# data$Body_1_XYZ_Head_X = as.double(data$Body_1_XYZ_Head_X) * 1000
# data$Body_1_XYZ_Head_Y = as.double(data$Body_1_XYZ_Head_Y) * 1000
# data$Body_1_XYZ_Head_Z = as.double(data$Body_1_XYZ_Head_Z) * 1000
# data = as.data.frame(data)
# data$Body_1_XYZ_Head_X = remove_outliers(data$Body_1_XYZ_Head_X)
# data$Body_1_XYZ_Head_Y = remove_outliers(data$Body_1_XYZ_Head_Y)
# data$Body_1_XYZ_Head_Z = remove_outliers(data$Body_1_XYZ_Head_Z)
# data = data[!(rowSums(is.na(data))), ]
# xrange1 = range(data$Body_1_XYZ_Head_X)
# yrange1 = range(data$Body_1_XYZ_Head_Y)
# zrange1 = range(data$Body_1_XYZ_Head_Z)
#
#
# data$Body_2_XYZ_Head_X = as.double(data$Body_2_XYZ_Head_X) * 1000
# data$Body_2_XYZ_Head_Y = as.double(data$Body_2_XYZ_Head_Y) * 1000
# data$Body_2_XYZ_Head_Z = as.double(data$Body_2_XYZ_Head_Z) * 1000
# data = as.data.frame(data)
# data$Body_2_XYZ_Head_X = remove_outliers(data$Body_2_XYZ_Head_X)
# data$Body_2_XYZ_Head_Y = remove_outliers(data$Body_2_XYZ_Head_Y)
# data$Body_2_XYZ_Head_Z = remove_outliers(data$Body_2_XYZ_Head_Z)
# data = data[!(rowSums(is.na(data))), ]
# xrange2 = range(data$Body_2_XYZ_Head_X)
# yrange2 = range(data$Body_2_XYZ_Head_Y)
# zrange2 = range(data$Body_2_XYZ_Head_Z)
#
#
# step_size = 30
# saveHTML({
#   for (i in seq(step_size + 1, nrow(data) - step_size, by = step_size)) {
#     layout(matrix(c(1, 2), 1))
#     scatterplot3d(
#       c(
#         data$Body_1_XYZ_Head_X[i - step_size],
#         data$Body_1_XYZ_Head_X[i],
#         data$Body_1_XYZ_Head_X[i + step_size]
#       ),
#       c(
#         data$Body_1_XYZ_Head_Y[i - step_size],
#         data$Body_1_XYZ_Head_Y[i],
#         data$Body_1_XYZ_Head_Y[i + step_size]
#       ),
#       c(
#         data$Body_1_XYZ_Head_Z[i - step_size],
#         data$Body_1_XYZ_Head_Z[i],
#         data$Body_1_XYZ_Head_Z[i + step_size]
#       ),
#       type = "l",
#       xlim = xrange1,
#       ylim = yrange1,
#       zlim = zrange1,
#       xlab = "",
#       ylab = "",
#       zlab = "",
#       sub = "",
#       lwd = 2
#     )
#     scatterplot3d(
#       c(
#         data$Body_2_XYZ_Head_X[i - step_size],
#         data$Body_2_XYZ_Head_X[i],
#         data$Body_2_XYZ_Head_X[i + step_size]
#       ),
#       c(
#         data$Body_2_XYZ_Head_Y[i - step_size],
#         data$Body_2_XYZ_Head_Y[i],
#         data$Body_2_XYZ_Head_Y[i + step_size]
#       ),
#       c(
#         data$Body_2_XYZ_Head_Z[i - step_size],
#         data$Body_2_XYZ_Head_Z[i],
#         data$Body_2_XYZ_Head_Z[i + step_size]
#       ),
#       type = "l",
#       xlim = xrange2,
#       ylim = yrange2,
#       zlim = zrange2,
#       xlab = "",
#       ylab = "",
#       zlab = "",
#       sub = "",
#       lwd = 2
#     )
#   }
# })

# ok, that doesn't tell me a little yet - though there does seem to be sync
# between subjects, in as far that both do "big movements that seem to
# be more or less at the same time, with the same direction

# things I can test: granger causality (per dimension? - distance to 0,0,0?
# choose a middle point in between, and see what happens to distances to that point?

# also, energy, in movements,

#devtools::install_github('areshenk/MSMVSampEn')
library(MSMVSampEn)

M = c(2)
tau = c(1)
r = 0.15
eps = 1

# person 1
#m = matrix(c(data$Body_1_XYZ_Head_X),nrow = 1)
#a = MSMVSampEn(mat = m, M, tau, r, eps, scaleMat = T)


# person 2
m2 = matrix(c(data$Body_2_XYZ_Head_X),nrow = 1)
b = MSMVSampEn(mat = m2, M, tau, r, eps, scaleMat = T)

#print(a)
print(b)

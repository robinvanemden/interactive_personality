library(data.table)
library(splitstackshape)
library(dyncomp)

person_vector <- c("Body_1", "Body_2")

part_vector   <- c("SpineBase","SpineMid","Neck","Head","ShoulderLeft",
                   "ElbowLeft","WristLeft","HandLeft","ShoulderRight",
                   "ElbowRight","WristRight","HandRight","HipLeft",
                   "KneeLeft","AnkleLeft","FootLeft","HipRight","KneeRight",
                   "AnkleRight","FootRight","SpineShoulder","HandTipLeft",
                   "ThumbLeft","HandTipRight","ThumbRight")

shannon_entropy <- function(p) {
  if (min(p) < 0 || sum(p) <= 0) return(NA)
  p.norm <- p[p>0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}

remove_outliers <- function(x, iqr_factor, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- iqr_factor * IQR(x, na.rm = na.rm)
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

threed_dist  <- function(data,person,part,person_length = TRUE) {
  plength = 1
  if(isTRUE(person_length)) {
    plength <- as.vector(data[,  paste0(person,"_Height_Estimate"), with=FALSE][[1]])
    plength <- mean(plength)
  }

  two_times_three <- data[, grep(paste0(person,"_XYZ_",part,"_*"), names(data)), with=FALSE]
  two_times_three <- cbind(tail(two_times_three, -1), head(two_times_three, -1))
  dx <- two_times_three[,1] - two_times_three[,4]
  dy <- two_times_three[,2] - two_times_three[,5]
  dz <- two_times_three[,3] - two_times_three[,6]
  dist <- as.vector(sqrt(dx*dx + dy*dy + dz*dz) * 100)[[1]] / plength
  dist <- remove_outliers(dist,iqr_factor = 2)
  dist[!is.na(dist)]
}


# ## Exploration of the diadic social moment experiment

# ## Read CSV

setwd("C:/Users/robin/Documents/GitHub/InteractivePersonality/phase_two_exploration/")

files <- list.files(path = "../phase_two_data/", pattern="*.csv")
for (i in 1:length(files)) {

  exp_name                   <- gsub("*.csv$", "", files[i])

  data                       <- fread(paste0("../phase_two_data/",files[i]), sep=",")
  data                       <- delete_na_rows(data)
  data                       <- cSplit(data, grep('XYZ', names(data), value=TRUE), ";")

  result                     <- list()
  result["exp"]              <- exp_name

  result["color_1"]          <- data[1,"Body_1_Color"]
  result["color_2"]          <- data[1,"Body_2_Color"]

  for(part in part_vector) {

    local_result                               <- threed_dist(data,"Body_1",part)
    result[paste0("Body_1","_",part,"_se")]    <- shannon_entropy(local_result)
    result[paste0("Body_1","_",part,"_sum")]   <- sum(local_result)
    result[paste0("Body_1","_",part,"_var")]   <- var(local_result)

    local_result                               <- threed_dist(data,"Body_2",part)
    result[paste0("Body_2","_",part,"_se")]    <- shannon_entropy(local_result)
    result[paste0("Body_2","_",part,"_sum")]   <- sum(local_result)
    result[paste0("Body_2","_",part,"_var")]   <- var(local_result)

  }

  if(i == 1) {
    all_experiments_results  <- as.data.table(result)
  } else {
    all_experiments_results  <- rbindlist(list(result, all_experiments_results), use.names = TRUE, fill = FALSE)
  }

}

all_experiments_results[,"dist_var_all_1"]  <- rowSums(all_experiments_results[, grep("Body_1.*var", names(all_experiments_results)), with=FALSE])
all_experiments_results[,"dist_sum_all_1"]  <- rowSums(all_experiments_results[, grep("Body_1.*sum", names(all_experiments_results)), with=FALSE])
all_experiments_results[,"dist_se_all_1"]   <- rowSums(all_experiments_results[, grep("Body_1.*se", names(all_experiments_results)), with=FALSE])

all_experiments_results[,"dist_var_all_2"]  <- rowSums(all_experiments_results[, grep("Body_2.*var", names(all_experiments_results)), with=FALSE])
all_experiments_results[,"dist_sum_all_2"]  <- rowSums(all_experiments_results[, grep("Body_2.*sum", names(all_experiments_results)), with=FALSE])
all_experiments_results[,"dist_se_all_2"]   <- rowSums(all_experiments_results[, grep("Body_2.*se", names(all_experiments_results)), with=FALSE])

k

last <- c("exp","color_1","dist_var_all_1","dist_sum_all_1","dist_se_all_1", "color_2","dist_var_all_2","dist_sum_all_2","dist_se_all_2")

all_experiments_overall                   <- all_experiments_results[ , ..last]

fwrite(all_experiments_overall, file="summary_stats_kinect_dyad.csv")

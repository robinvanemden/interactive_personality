library(data.table)
library(splitstackshape)
library(dyncomp)
library(foreign)
library(lubridate)
library(here)
library(zeallot)

# Set data analysis ------------------------------------------------------------------------------------------

name <- "spinebase_Sum"

# General setup ----------------------------------------------------------------------------------------------

setwd(here())

source("./R/kinect_meta_data.R")
source("./R/support_functions.R")

data_out<- "./data_out/"

data_dir_1  <- "./data_in/kinect_phase_1/"
data_dir_2  <- "./data_in/kinect_phase_2/"

data_dir_spss<- "./data_in/spss/"

# Import SPSS ------------------------------------------------------------------------------------------------

spss_data = read.spss(paste0(data_dir_spss,"kinect_2018_full.sav"), to.data.frame=TRUE)

rownames(spss_data) <- spss_data[,1]

# ## Read CSV  -----------------------------------------------------------------------------------------------

message("Start reading csv file from both kinect data directories.")

files<- list.files(path = data_dir_1, pattern="*.csv")

for (i in 1:length(files)) {

  # load a file and split data into three timebased parts ----------------------------------------------------

  exp_name  <- gsub("*_1.csv$", "", files[i])

  message(paste0("Analysing csv file ",files[i]))

  data_1 <- read_and_split_data(paste0(data_dir_1,exp_name,"_1.csv"))
  data_2 <- read_and_split_data(paste0(data_dir_2,exp_name,"_2.csv"))

  data_1 <- normalize_order_body_green_red(data_1, as.character(spss_data[exp_name,]$fake_part_color_part_1))
  data_2 <- normalize_order_body_green_red(data_2, as.character(spss_data[exp_name,]$fake_part_color_part_2))

  data_1$hours  <- hms::as.hms(as_datetime(as.integer(data_1$UNIX_timestamp/1000),origin="1970-01-01"))
  data_2$hours  <- hms::as.hms(as_datetime(as.integer(data_2$UNIX_timestamp/1000),origin="1970-01-01"))

  start_time_1 <- as.character(spss_data[exp_name,]$handshake_part_1)
  c(part_1_data, part_1a_data, part_1b_data)  %<-% split_data_by_time(data_1, start_time_1)

  start_time_2 <- as.character(spss_data[exp_name,]$conversation_part_2)
  c(part_2_data, part_2a_data, part_2b_data)  %<-% split_data_by_time(data_2, start_time_2)

  # build up results -----------------------------------------------------------------------------------------

  result<- list()
  result["exp"] <- exp_name
  result["color_1"] <- part_1_data[1,"Body_1_Color"]
  result["color_2"] <- part_1_data[1,"Body_2_Color"]
  result["length_color_1"]  <- as.numeric(colMeans(part_1_data[,"Body_1_Height_Estimate"]))
  result["length_color_2"]  <- as.numeric(colMeans(part_1_data[,"Body_2_Height_Estimate"]))

  # between subjects

  element                                     <- "Head"
  fname                                       <- "sum"

  result[paste0("part_1__",element,"_",fname,"_between")] <- sum(threed_between_persons(part_1_data, element))
  result[paste0("part_2__",element,"_",fname,"_between")] <- sum(threed_between_persons(part_2_data, element))
  result[paste0("part_1a_",element,"_",fname,"_between")] <- sum(threed_between_persons(part_1a_data,element))
  result[paste0("part_2a_",element,"_",fname,"_between")] <- sum(threed_between_persons(part_2a_data,element))
  result[paste0("part_1b_",element,"_",fname,"_between")] <- sum(threed_between_persons(part_1b_data,element))
  result[paste0("part_2b_",element,"_",fname,"_between")] <- sum(threed_between_persons(part_2b_data,element))

  if(i == 1) {
    all_experiments_results <- as.data.table(result)
  } else {
    all_experiments_results <- rbindlist(list(result, all_experiments_results), use.names = TRUE, fill = FALSE)
  }

}

# rename vars and write --------------------------------------------------------------------------------------

names(all_experiments_results) <- gsub(x = names(all_experiments_results), pattern = "color_1",
                                       replacement = "fake")

names(all_experiments_results) <- gsub(x = names(all_experiments_results), pattern = "color_2",
                                       replacement = "subject")

fwrite(all_experiments_results, file=paste0(data_out,paste0("kinect_dyad_",name,".csv")))



# ----------------------------------- TODO -------------------------------------------------------------------

#result["part_1__armddist_sum_color_1"]<- sum(threed_dist_parts(part_1_data, "Body_1","HandRight","HandLeft"))
#result["part_2__armddist_sum_color_1"]<- sum(threed_dist_parts(part_2_data, "Body_1","HandRight","HandLeft"))
#result["part_1__armddist_sum_color_2"]<- sum(threed_dist_parts(part_1_data, "Body_2","HandRight","HandLeft"))
#result["part_2__armddist_sum_color_2"]<- sum(threed_dist_parts(part_2_data, "Body_2","HandRight","HandLeft"))
#result["part_1a_armddist_sum_color_1"]<- sum(threed_dist_parts(part_1a_data,"Body_1","HandRight","HandLeft"))
#result["part_2a_armddist_sum_color_1"]<- sum(threed_dist_parts(part_2a_data,"Body_1","HandRight","HandLeft"))
#result["part_1a_armddist_sum_color_2"]<- sum(threed_dist_parts(part_1a_data,"Body_2","HandRight","HandLeft"))
#result["part_2a_armddist_sum_color_2"]<- sum(threed_dist_parts(part_2a_data,"Body_2","HandRight","HandLeft"))
#result["part_1b_armddist_sum_color_1"]<- sum(threed_dist_parts(part_1b_data,"Body_1","HandRight","HandLeft"))
#result["part_2b_armddist_sum_color_1"]<- sum(threed_dist_parts(part_2b_data,"Body_1","HandRight","HandLeft"))
#result["part_1b_armddist_sum_color_2"]<- sum(threed_dist_parts(part_1b_data,"Body_2","HandRight","HandLeft"))
#result["part_2b_armddist_sum_color_2"]<- sum(threed_dist_parts(part_2b_data,"Body_2","HandRight","HandLeft"))


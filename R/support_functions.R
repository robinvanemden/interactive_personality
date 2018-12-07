read_and_split_data <- function(file_name) {
  data <- fread(file_name, sep=",")
  data <- delete_na_rows(data)
  data <- cSplit(data, grep('XYZ', names(data), value=TRUE), ";")
  return(data)
}

normalize_order_body_green_red <- function(data, experimenter_color) {
  if (!is.na(experimenter_color) && experimenter_color == "green") {
    names(data)                                           <- gsub(x = names(data), pattern = "Body_1", replacement = "#")
    names(data)                                           <- gsub(x = names(data), pattern = "Body_2", replacement = "Body_1")
    names(data)                                           <- gsub(x = names(data), pattern = "#", replacement = "Body_2")
  }
  return(data)
}

split_data_by_time                                          <- function(data, start) {
  three_minutes                                             <- hms::as.hms("00:03:00")
  one_point_five_minutes                                    <- hms::as.hms("00:01:30")

  part_start                                                <- hms::as.hms(start)
  part_end                                                  <- hms::as.hms(part_start + three_minutes)
  part_data                                                 <- data[hours > part_start & hours < part_end]

  part_a_start                                              <- part_start
  part_a_end                                                <- hms::as.hms(part_a_start + one_point_five_minutes)
  part_a_data                                               <- data[hours > part_a_start & hours < part_a_end]

  part_b_start                                              <- part_a_end
  part_b_end                                                <- hms::as.hms(part_b_start + one_point_five_minutes)
  part_b_data                                               <- data[hours > part_b_start & hours < part_b_end]
  return(list(part_data,part_a_data,part_b_data))
}

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


threed_dist_step  <- function(data,person,part,person_length = TRUE) {
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

threed_dist_parts <- function(data,person,part1,part2,person_length = TRUE) {
  plength = 1
  if(isTRUE(person_length)) {
    plength <- as.vector(data[,  paste0(person,"_Height_Estimate"), with=FALSE][[1]])
    plength <- mean(plength)
  }

  two_times_three_1 <- data[, grep(paste0(person,"_XYZ_",part1,"_*"), names(data)), with=FALSE]
  two_times_three_2 <- data[, grep(paste0(person,"_XYZ_",part2,"_*"), names(data)), with=FALSE]

  two_times_three <- cbind(two_times_three_1, two_times_three_2)
  dx <- two_times_three[,1] - two_times_three[,4]
  dy <- two_times_three[,2] - two_times_three[,5]
  dz <- two_times_three[,3] - two_times_three[,6]
  dist <- as.vector(sqrt(dx*dx + dy*dy + dz*dz) * 100)[[1]] / plength
  dist <- remove_outliers(dist,iqr_factor = 2)
  dist[!is.na(dist)]
}

threed_between_persons <- function(data,part) {

  two_times_three_1 <- data[, grep(paste0("Body_1_XYZ_",part,"_*"), names(data)), with=FALSE]
  two_times_three_2 <- data[, grep(paste0("Body_2_XYZ_",part,"_*"), names(data)), with=FALSE]

  two_times_three <- cbind(two_times_three_1, two_times_three_2)
  dx <- two_times_three[,1] - two_times_three[,4]
  dy <- two_times_three[,2] - two_times_three[,5]
  dz <- two_times_three[,3] - two_times_three[,6]
  dist <- as.vector(sqrt(dx*dx + dy*dy + dz*dz) * 100)[[1]]
  dist <- remove_outliers(dist,iqr_factor = 2)
  dist[!is.na(dist)]
}

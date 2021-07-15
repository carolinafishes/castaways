#Thomas Howard
#Data manipulation for Cape Hatteras data
#Running the entire code may take a couple hours

#reset variables
rm(list = ls())

#set working directory to where you will read and write .csv the files
setwd("/Users/thowa/Desktop/Museum_work/Cape_Hatteras_3/")

#set input .csv file of interest
#format for castaway-classifications.csv files must be the same
input_csv_name <- "castaway-classifications.csv"

#read in .csv file, this may take a few minutes
input_csv_file <- read.csv(input_csv_name, header = T)

##################################################################################
#This is the start of the data organization

#columns of interest
col_of_interest <- match(c("workflow_name", "annotations","subject_data", "subject_ids"), colnames(input_csv_file))

#to_manipulate is just takes the data from the .csv file
to_manipulate <- input_csv_file[col_of_interest]

#throughout this entire .R file, whenever a .csv file is read in, the values need to be set to character each time
to_manipulate$annotations <- as.character(to_manipulate$annotations)
to_manipulate$subject_data <- as.character(to_manipulate$subject_data)

#the unique() command removes any duplicated rows from the specified data set
to_manipulate <- unique(to_manipulate)

#Only keep the data that includes Cape Hatteras data
to_keep <- c()
pb <- txtProgressBar(min = 0, max = nrow(to_manipulate), initial = 0, style = 3)
for(i in 1:nrow(to_manipulate)) {
  setTxtProgressBar(pb, i)
  if(length(grep("Cape Hatteras", to_manipulate$workflow_name[i])) > 0) {
    to_keep <- c(to_keep, i)
  }
}
to_manipulate <- to_manipulate[to_keep,]


#clean_table is a new data frame where all data will be organized
#.csv files will be saved periodically as "clean_table_#.csv" to serve as checkpoints if something goes wrong
clean_table <- data.frame(matrix(NA, ncol = 6, nrow = nrow(to_manipulate)))

colnames(clean_table) <- c("filename", "subject_ids", "id", "workflow_name", "task_label", "value")

#add subject_ids to clean_table
clean_table$subject_ids <- to_manipulate$subject_ids
clean_table$workflow_name <- to_manipulate$workflow_name

#loop to find filename from "annotations" and add to clean_table
pb <- txtProgressBar(min = 0, max = nrow(clean_table), initial = 0, style = 3)
for(i in 1:nrow(clean_table)) {
  setTxtProgressBar(pb, i)
  cur <- i
  current <- to_manipulate$subject_data[cur]
  loc1 <- gregexpr("Filename", current)[[1]][1]
  tmp <- substr(current, loc1, nchar(current))
  loc2 <- gregexpr("\"", tmp)[[1]][2]
  loc3 <- gregexpr(".png", tmp)[[1]][1]
  this_filename <- substr(tmp, loc2+1, loc3-1)
  
  #add id to clean_table
  clean_table$filename[i] <- this_filename
}

#loop to find id from "annotations" and add to clean_table
pb <- txtProgressBar(min = 0, max = nrow(clean_table), initial = 0, style = 3)
for(i in 1:nrow(clean_table)) {
  setTxtProgressBar(pb, i)
  cur <- i
  current <- to_manipulate$subject_data[cur]
  loc1 <- gregexpr("id", current)[[1]][1]
  tmp <- substr(current, loc1, nchar(current))
  loc2 <- gregexpr("\"", tmp)[[1]][1]
  loc3 <- gregexpr("\"", tmp)[[1]][2]
  tmp2 <- substr(tmp, loc2+2, loc3)
  #extract the id number value
  this_id <- gsub("([0-9]+).*$", "\\1", tmp2)
  
  #add id to clean_table
  clean_table$id[i] <- this_id
}


#the next few steps are very intensive, so I split the data sets into intervals of "interval" (ex. 500) for faster run time
#These steps will basically go through cell and extract the important parts of the data
interval <- 500
num_of_vars <- ceiling(nrow(clean_table) / interval)

prev_num_1 <- 1
prev_num_2 <- interval
for(i in 1:num_of_vars) {
  assign(paste0("ct", i), clean_table[(prev_num_1):prev_num_2,])
  assign(paste0("tm", i), to_manipulate[(prev_num_1):prev_num_2,])
  prev_num_1 <-prev_num_2 + 1
  if(i == num_of_vars) {
    prev_num_2 <- nrow(clean_table)
  } else {
    prev_num_2 <- prev_num_2 + interval
  }
}

#this block of code will take a few hours to run
new_table <- rbind()
for(a in 1:num_of_vars) {
  clean_table <- eval(parse(text = paste0("ct", a)))
  to_manipulate <- eval(parse(text = paste0("tm", a)))

    
  i <- 1
  while(i < (nrow(clean_table)+1)) {
    current <- to_manipulate$annotations[i]
    placement <- paste0(a, "/", num_of_vars, ", ", i, "/", nrow(clean_table))
    print(placement)
    find_brackets <- gregexpr("\\{", current)
    bracket_count <- length(find_brackets[[1]])
    
    if(bracket_count > 1) {
      for(j in 1:bracket_count) {
        if(j == 1) {
          
          open_bracket <- gregexpr("\\{", current)[[1]][j]
          closed_bracket <- gregexpr("\\}", current)[[1]][j]
          
          in_bracket <- substr(current, open_bracket, closed_bracket)
          
          find_value <- gregexpr("value", in_bracket)[[1]][1]
          value_to_end <- substr(in_bracket, find_value, nchar(in_bracket))
          
          first_slash <- gregexpr('\"', value_to_end)[[1]][2]
          next_slash <- gregexpr('\"', value_to_end)[[1]][3]
          this_value <- substr(value_to_end, first_slash+1, next_slash-1)
          
          clean_table$value[i] <- this_value
          
          
          find_label <- gregexpr("task_label", in_bracket)[[1]][1]
          label_to_end <- substr(in_bracket, find_label, nchar(in_bracket))
          
          first_slash <- gregexpr('\"', label_to_end)[[1]][2]
          next_slash <- gregexpr('\"', label_to_end)[[1]][3]
          this_task_label <- substr(label_to_end, first_slash+1, next_slash-1)
          clean_table$task_label[i] <- this_task_label
        } else {
          if(i == nrow(clean_table)) {
            clean_table <- rbind(clean_table[1:i,], clean_table[i,])
            to_manipulate <- rbind(to_manipulate[1:i,], to_manipulate[i,])
          } else {
            clean_table <- rbind(clean_table[1:i,], clean_table[i,], clean_table[(i+1):nrow(clean_table),])
            to_manipulate <- rbind(to_manipulate[1:i,], to_manipulate[i,], to_manipulate[(i+1):nrow(to_manipulate),])
          }
          
          
          rownames(clean_table) <- NULL
          i <- i+1
          
          open_bracket <- gregexpr("\\{", current)[[1]][j]
          closed_bracket <- gregexpr("\\}", current)[[1]][j]
          
          in_bracket <- substr(current, open_bracket, closed_bracket)
          
          find_value <- gregexpr("value", in_bracket)[[1]][1]
          value_to_end <- substr(in_bracket, find_value, nchar(in_bracket))
          
          first_slash <- gregexpr('\"', value_to_end)[[1]][2]
          next_slash <- gregexpr('\"', value_to_end)[[1]][3]
          this_value <- substr(value_to_end, first_slash+1, next_slash-1)
          
          clean_table$value[i] <- this_value
          
          
          find_label <- gregexpr("task_label", in_bracket)[[1]][1]
          label_to_end <- substr(in_bracket, find_label, nchar(in_bracket))
          
          first_slash <- gregexpr('\"', label_to_end)[[1]][2]
          next_slash <- gregexpr('\"', label_to_end)[[1]][3]
          this_task_label <- substr(label_to_end, first_slash+1, next_slash-1)
          clean_table$task_label[i] <- this_task_label
        }
      }
      
    } else {
      open_bracket <- gregexpr("\\{", current)[[1]][1]
      closed_bracket <- gregexpr("\\}", current)[[1]][1]
      
      in_bracket <- substr(current, open_bracket, closed_bracket)
      
      find_value <- gregexpr("value", in_bracket)[[1]][1]
      value_to_end <- substr(in_bracket, find_value, nchar(in_bracket))
      
      first_slash <- gregexpr('\"', value_to_end)[[1]][2]
      next_slash <- gregexpr('\"', value_to_end)[[1]][3]
      this_value <- substr(value_to_end, first_slash+1, next_slash-1)
      
      clean_table$value[i] <- this_value
      
      
      find_label <- gregexpr("task_label", in_bracket)[[1]][1]
      label_to_end <- substr(in_bracket, find_label, nchar(in_bracket))
      
      first_slash <- gregexpr('\"', label_to_end)[[1]][2]
      next_slash <- gregexpr('\"', label_to_end)[[1]][3]
      this_task_label <- substr(label_to_end, first_slash+1, next_slash-1)
      clean_table$task_label[i] <- this_task_label
    }
    
    i <- i+1
  }
  
  assign(paste0("ct", a), clean_table)
}

#combine everything together as new_table
new_table <- rbind()
ct_list <- "ct1"
for(i in 2:num_of_vars) {
  ct_list <- paste0(ct_list, ", ct", i)
}
new_table <- eval(parse(text = paste0("rbind(", ct_list, ")")))
new_table <- new_table[order(new_table$filename),]

#this is our first "checkpoint" saved as a .csv file in the working directory
write.csv(new_table, file = "clean_table.csv", row.names = F)

#At this point, the important parts of the data should have been extracted, and are ready for the next steps

##################################################################################

clean_table <- read.csv(file = "clean_table.csv")

#sort and remove duplicates from clean_table
clean_table <- clean_table[order(clean_table$filename, clean_table$id),]
clean_table <- unique(clean_table)

write.csv(clean_table, file = "clean_table_1.csv", row.names = F)

##################################################################################

clean_table_2 <- read.csv(file = "clean_table_1.csv")

#prepare to give filenames, station numbers, and the values their own columns in the data table
clean_table_2 <- cbind(clean_table_2[,1], NA, clean_table_2[,2:5], clean_table_2[,6])

colnames(clean_table_2)[1] <- "filename"
colnames(clean_table_2)[2] <- "Station_Number"
colnames(clean_table_2)[7] <- "value"

clean_table_2$task_label <- as.character(clean_table_2$task_label)
clean_table_2$workflow_name <- as.character(clean_table_2$workflow_name)
clean_table_2$value <- as.character(clean_table_2$value)

#list of all file names
file_list <- unique(clean_table_2$filename)

#this is just filtering out some unneeded data
to_delete <- c()
pb <- txtProgressBar(min = 0, max = nrow(clean_table_2), initial = 0, style = 3)
for(i in 1:nrow(clean_table_2)) {
  setTxtProgressBar(pb, i)
  if(length(grep("There is data", clean_table_2$value[i])) > 0) {
    to_delete <- c(to_delete, i)
  }
  if(length(grep("No data", clean_table_2$value[i])) > 0) {
    to_delete <- c(to_delete, i)
  }
  if(length(grep("###", clean_table_2$task_label[i])) > 0) {
    to_delete <- c(to_delete, i)
  }
  if(length(grep("Is there", clean_table_2$task_label[i])) > 0) {
    to_delete <- c(to_delete, i)
  }
  if(length(grep("Does Column", clean_table_2$task_label[i])) > 0) {
    to_delete <- c(to_delete, i)
  }
  if(length(grep("Nothing recorded", clean_table_2$task_label[i])) > 0) {
    to_delete <- c(to_delete, i)
  }
  if(is.na(clean_table_2$value[i])) {
    to_delete <- c(to_delete, i)
  }
  if(!is.na(clean_table_2$value[i])) {
    if(clean_table_2$value[i] == "Yes") {
      to_delete <- c(to_delete, i)
    }
    if(clean_table_2$value[i] == "") {
      to_delete <- c(to_delete, i)
    }
    if(clean_table_2$value[i] == " ") {
      to_delete <- c(to_delete, i)
    }
    if(clean_table_2$value[i] == "task") {
      to_delete <- c(to_delete, i)
    }
    if(clean_table_2$task_label[i] == "value") {
      to_delete <- c(to_delete, i)
    }
  }
}
clean_table_2 <- clean_table_2[-to_delete,]


#checkpoint_1 <- clean_table_2 #this is just another checkpoint for myself in case the next script fails

#this will make all of the task labels consistent
pb <- txtProgressBar(min = 0, max = nrow(clean_table_2), initial = 0, style = 3)
for(i in 1:nrow(clean_table_2)) {
  setTxtProgressBar(pb, i)
  if(length(grep("Station", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Station_Number"
  }
  if(length(grep("Cruise", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Cruise_Number"
  }
  if(length(grep("Sheet No", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Sheet_No"
  }
  if(length(grep("Master", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Master_and_Mates"
  }
  if(length(grep("Principal", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Principal_Investigator"
  }
  if(length(grep("Oceanographic", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Oceanographic_Party_Chief"
  }
  if(length(grep("Hydrographic", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Hydrographic_Gear"
  }
  if(length(grep("Plankton", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Plankton_Nets"
  }
  if(length(grep("Cameras", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Cameras"
  }
  if(length(grep("Cores", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Cores"
  }
  if(length(grep("Dredges", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Dredges"
  }
  if(length(grep("Trawls", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Trawls"
  }
  if(length(grep("Biological Gear", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Biological_Gear"
  }
  if(length(grep("Start Time", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Time_Start"
  }
  if(length(grep("Start Date", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Date_Start"
  }
  if(length(grep("Start Loran", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Loran_Start"
  }
  if(length(grep("Start Latitude/Longitude", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Latitude_Longitude_Start"
  }
  if(length(grep("Start Sonic", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Sonic_Depth_Start"
  }
  if(length(grep("Finish Time", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Time_Finish"
  }
  if(length(grep("Finish Date", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Date_Finish"
  }
  if(length(grep("Finish Loran", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Loran_Finish"
  }
  if(length(grep("Finish Latitude/Longitude", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Latitude_Longitude_Finish"
  }
  if(length(grep("Finish Sonic", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Sonic_Depth_Finish"
  }
  if(length(grep("Other", clean_table_2$task_label[i])) > 0) {
    clean_table_2$task_label[i] <- "Other"
  }
}

#remove any duplicates
clean_table_2 <- unique(clean_table_2)

#prepare to separate more data into their own columns
clean_table_2 <- cbind(clean_table_2[,1:2], NA, NA, NA, NA, NA, clean_table_2[,3:7])

colnames(clean_table_2)[3] <- "Cruise_Number"
colnames(clean_table_2)[4] <- "Sheet_No"
colnames(clean_table_2)[5] <- "Master_and_Mates"
colnames(clean_table_2)[6] <- "Principal_Investigator"
colnames(clean_table_2)[7] <- "Oceanographic_Party_Chief"

#save checkpoint as a .csv file
write.csv(clean_table_2, "clean_table_2a.csv", row.names = F)

##################################################################################

clean_table_2 <- read.csv(file = "clean_table_2a.csv")

clean_table_2$filename <- as.character(clean_table_2$filename)
clean_table_2$workflow_name <- as.character(clean_table_2$workflow_name)
clean_table_2$task_label <- as.character(clean_table_2$task_label)
clean_table_2$value <- as.character(clean_table_2$value)
file_list <- unique(clean_table_2$filename)

#resets the row numbers
rownames(clean_table_2) <- NULL

#the following variables, except the station numbers, are expected to be same for each filename and are listed as "Crew"
#fill in the variables with their correct values
for(i in 1:length(file_list)) {
  subset_1 <- clean_table_2[which(clean_table_2$filename == file_list[i]),]
  id_list <- unique(subset_1$id)
  print(paste0(i, "/", length(file_list)))
  
  #adjust crew data
  #delete crew *rows afterwards
  rows_of_this_filename <- which(grepl(file_list[i], clean_table_2$filename))
  
  cruise_rows <- c()
  cruise_rows <- grep("Cruise_Number", subset_1$task_label)
  if(length(cruise_rows) > 0) {
    num_of_cols <- length(grep("Cruise_Number", colnames(clean_table_2)))
    if(num_of_cols > length(cruise_rows)) {
      for(k in 1:length(cruise_rows)) {
        crew_col <- grep("Cruise_Number", colnames(clean_table_2))[k]
        clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[cruise_rows[k]]
      }
    } else {
      for(k in 1:num_of_cols) {
        crew_col <- grep("Cruise_Number", colnames(clean_table_2))[k]
        clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[cruise_rows[k]]
      }
    }
    
    while(length(cruise_rows) > num_of_cols) {
      clean_table_2 <- cbind(clean_table_2, NA)
      colnames(clean_table_2)[ncol(clean_table_2)] <- paste0("Cruise_Number", "_", num_of_cols + 1)
      num_of_cols <- length(grep("Cruise_Number", colnames(clean_table_2)))
      crew_col <- grep("Cruise_Number", colnames(clean_table_2))[num_of_cols]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[cruise_rows[num_of_cols]]
      
    }
  }
  
  Sheet_No_rows <- c()
  Sheet_No_rows <- grep("Sheet_No", subset_1$task_label)
  if(length(Sheet_No_rows) > 0) {
    num_of_cols <- length(grep("Sheet_No", colnames(clean_table_2)))
    for(k in 1:num_of_cols) {
      crew_col <- grep("Sheet_No", colnames(clean_table_2))[k]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[Sheet_No_rows[k]]
    }
    while(length(Sheet_No_rows) > num_of_cols) {
      clean_table_2 <- cbind(clean_table_2, NA)
      colnames(clean_table_2)[ncol(clean_table_2)] <- paste0("Sheet_No", "_", num_of_cols + 1)
      num_of_cols <- length(grep("Sheet_No", colnames(clean_table_2)))
      crew_col <- grep("Sheet_No", colnames(clean_table_2))[num_of_cols]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[Sheet_No_rows[num_of_cols]]
      
    }
  }
  
  Master_and_Mates_rows <- c()
  Master_and_Mates_rows <- grep("Master_and_Mates", subset_1$task_label)
  if(length(Master_and_Mates_rows) > 0) {
    num_of_cols <- length(grep("Master_and_Mates", colnames(clean_table_2)))
    for(k in 1:num_of_cols) {
      crew_col <- grep("Master_and_Mates", colnames(clean_table_2))[k]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[Master_and_Mates_rows[k]]
    }
    while(length(Master_and_Mates_rows) > num_of_cols) {
      clean_table_2 <- cbind(clean_table_2, NA)
      colnames(clean_table_2)[ncol(clean_table_2)] <- paste0("Master_and_Mates", "_", num_of_cols + 1)
      num_of_cols <- length(grep("Master_and_Mates", colnames(clean_table_2)))
      crew_col <- grep("Master_and_Mates", colnames(clean_table_2))[num_of_cols]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[Master_and_Mates_rows[num_of_cols]]
      
    }
  }
  
  Principal_Investigator_rows <- c()
  Principal_Investigator_rows <- grep("Principal_Investigator", subset_1$task_label)
  if(length(Principal_Investigator_rows) > 0) {
    num_of_cols <- length(grep("Principal_Investigator", colnames(clean_table_2)))
    for(k in 1:num_of_cols) {
      crew_col <- grep("Principal_Investigator", colnames(clean_table_2))[k]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[Principal_Investigator_rows[k]]
    }
    while(length(Principal_Investigator_rows) > num_of_cols) {
      clean_table_2 <- cbind(clean_table_2, NA)
      colnames(clean_table_2)[ncol(clean_table_2)] <- paste0("Principal_Investigator", "_", num_of_cols + 1)
      num_of_cols <- length(grep("Principal_Investigator", colnames(clean_table_2)))
      crew_col <- grep("Principal_Investigator", colnames(clean_table_2))[num_of_cols]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[Principal_Investigator_rows[num_of_cols]]
      
    }
  }
  
  Oceanographic_Party_Chief_rows <- c()
  Oceanographic_Party_Chief_rows <- grep("Oceanographic_Party_Chief", subset_1$task_label)
  if(length(Oceanographic_Party_Chief_rows) > 0) {
    num_of_cols <- length(grep("Oceanographic_Party_Chief", colnames(clean_table_2)))
    for(k in 1:num_of_cols) {
      crew_col <- grep("Oceanographic_Party_Chief", colnames(clean_table_2))[k]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[Oceanographic_Party_Chief_rows[k]]
    }
    while(length(Oceanographic_Party_Chief_rows) > num_of_cols) {
      clean_table_2 <- cbind(clean_table_2, NA)
      colnames(clean_table_2)[ncol(clean_table_2)] <- paste0("Oceanographic_Party_Chief", "_", num_of_cols + 1)
      num_of_cols <- length(grep("Oceanographic_Party_Chief", colnames(clean_table_2)))
      crew_col <- grep("Oceanographic_Party_Chief", colnames(clean_table_2))[num_of_cols]
      clean_table_2[rows_of_this_filename, crew_col] <- subset_1$value[Oceanographic_Party_Chief_rows[num_of_cols]]
      
    }
  }
  
  
  #cycle through each column for this file and manually transpose the station numbers
  for(m in 1:10){
    
    station_rows <- grep(paste0("#", m, " "), subset_1$workflow_name)
    find_station <- which(grepl("station", subset_1$task_label[station_rows], ignore.case = T))
    station_number <- c()
    station_number <- unique(subset_1$value[station_rows[find_station]])
    rows_to_edit <- as.integer(rownames(clean_table_2[which(clean_table_2$filename == file_list[i]),])[station_rows])
    if(length(station_number) > 0) {
      num_of_cols <- length(grep("Station_Number", colnames(clean_table_2)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Station_Number", colnames(clean_table_2))[k]
        clean_table_2[rows_to_edit, crew_col] <- station_number[k]
      }
      while(length(station_number) > num_of_cols) {
        clean_table_2 <- cbind(clean_table_2, NA)
        colnames(clean_table_2)[ncol(clean_table_2)] <- paste0("Station_Number", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Station_Number", colnames(clean_table_2)))
        crew_col <- grep("Station_Number", colnames(clean_table_2))[num_of_cols]
        clean_table_2[rows_to_edit, crew_col] <- station_number[num_of_cols]
        
      }
    }
  }
}

#fill the workflow names with the correct column values
for(m in 1:10) {
  station_rows <- grep(paste0("#", m, " "), clean_table_2$workflow_name)
  clean_table_2$workflow_name[station_rows] <- paste0("Column_", m)
}

#remove subject_ids and id columns
to_delete <- c()
to_delete <- c(to_delete, which(colnames(clean_table_2) == "subject_ids"))
to_delete <- c(to_delete, which(colnames(clean_table_2) == "id"))
clean_table_2 <- clean_table_2[,-to_delete]

#remove duplicates
clean_table_2 <- unique(clean_table_2)

#another checkpoint
write.csv(clean_table_2, file = "clean_table_2b.csv", row.names = F)

##################################################################################

clean_table_3 <- read.csv(file = "clean_table_2b.csv") 

#remove rows that include "Crew" in the workflow name, since these have been taken care of in the script above
to_delete <- grep("Crew", clean_table_3$workflow_name)
clean_table_3 <- clean_table_3[-to_delete,]
rownames(clean_table_3) <- NULL

#we have already added station numbers to our specified column as well, so we can remvoe these
to_delete <- grep("Station_Number", clean_table_3$task_label)
clean_table_3 <- clean_table_3[-to_delete,]

for(i in 1:ncol(clean_table_3)) {
  clean_table_3[,i] <- as.character(clean_table_3[,i])
}

#remove excess spaces (" ") at the beginning and end of values to keep things cleaner
for(i in 1:ncol(clean_table_3)) {
  clean_table_3[,i] <- trimws(clean_table_3[,i], which = "both")
}
clean_table_3 <- unique(clean_table_3)
rownames(clean_table_3) <- NULL


#now we will transpose the bulk of the data from the "task_label" and "value" that have not been done yet
#other transpose functions didn't work quite right, so I had to do this manually
#transpose start, this may take an hour or 2
filename_list <- unique(clean_table_3$filename)
pb <- txtProgressBar(min = 0, max = length(filename_list), initial = 0, style = 3)
for(i in 1:length(filename_list)) {
  setTxtProgressBar(pb, i)
  rows_of_this_filename <- which(clean_table_3$filename == filename_list[i])
  station_number_list <- unique(clean_table_3$workflow_name[rows_of_this_filename])
  for(j in 1:length(station_number_list)) {
    rows_of_this_station <- which(clean_table_3$workflow_name[rows_of_this_filename] == station_number_list[j])
    
    rows_of_interest <- rows_of_this_filename[rows_of_this_station]
    
    Date_Start_rows <- c()
    Date_Start_rows <- grep("Date_Start", clean_table_3$task_label[rows_of_interest])
    if(length(Date_Start_rows) > 0) {
      num_of_cols <- length(grep("Date_Start", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Date_Start"
      }
      num_of_cols <- length(grep("Date_Start", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Date_Start", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Date_Start_rows[k]]]
      }
      while(length(Date_Start_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Date_Start", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Date_Start", colnames(clean_table_3)))
        crew_col <- grep("Date_Start", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Date_Start_rows[num_of_cols]]]
      }
    }
    
    Time_Start_rows <- c()
    Time_Start_rows <- grep("Time_Start", clean_table_3$task_label[rows_of_interest])
    if(length(Time_Start_rows) > 0) {
      num_of_cols <- length(grep("Time_Start", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Time_Start"
      }
      num_of_cols <- length(grep("Time_Start", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Time_Start", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Time_Start_rows[k]]]
      }
      while(length(Time_Start_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Time_Start", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Time_Start", colnames(clean_table_3)))
        crew_col <- grep("Time_Start", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Time_Start_rows[num_of_cols]]]
      }
    }
    
    Latitude_Longitude_Start_rows <- c()
    Latitude_Longitude_Start_rows <- grep("Latitude_Longitude_Start", clean_table_3$task_label[rows_of_interest])
    if(length(Latitude_Longitude_Start_rows) > 0) {
      num_of_cols <- length(grep("Latitude_Longitude_Start", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Latitude_Longitude_Start"
      }
      num_of_cols <- length(grep("Latitude_Longitude_Start", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Latitude_Longitude_Start", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Latitude_Longitude_Start_rows[k]]]
      }
      while(length(Latitude_Longitude_Start_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Latitude_Longitude_Start", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Latitude_Longitude_Start", colnames(clean_table_3)))
        crew_col <- grep("Latitude_Longitude_Start", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Latitude_Longitude_Start_rows[num_of_cols]]]
      }
    }
    
    Sonic_Depth_Start_rows <- c()
    Sonic_Depth_Start_rows <- grep("Sonic_Depth_Start", clean_table_3$task_label[rows_of_interest])
    if(length(Sonic_Depth_Start_rows) > 0) {
      num_of_cols <- length(grep("Sonic_Depth_Start", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Sonic_Depth_Start"
      }
      num_of_cols <- length(grep("Sonic_Depth_Start", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Sonic_Depth_Start", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Sonic_Depth_Start_rows[k]]]
      }
      while(length(Sonic_Depth_Start_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Sonic_Depth_Start", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Sonic_Depth_Start", colnames(clean_table_3)))
        crew_col <- grep("Sonic_Depth_Start", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Sonic_Depth_Start_rows[num_of_cols]]]
      }
    }
    
    Hydrographic_Gear_rows <- c()
    Hydrographic_Gear_rows <- grep("Hydrographic_Gear", clean_table_3$task_label[rows_of_interest])
    if(length(Hydrographic_Gear_rows) > 0) {
      num_of_cols <- length(grep("Hydrographic_Gear", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Hydrographic_Gear"
      }
      num_of_cols <- length(grep("Hydrographic_Gear", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Hydrographic_Gear", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Hydrographic_Gear_rows[k]]]
      }
      while(length(Hydrographic_Gear_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Hydrographic_Gear", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Hydrographic_Gear", colnames(clean_table_3)))
        crew_col <- grep("Hydrographic_Gear", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Hydrographic_Gear_rows[num_of_cols]]]
      }
    }
    
    Sonic_Depth_Finish_rows <- c()
    Sonic_Depth_Finish_rows <- grep("Sonic_Depth_Finish", clean_table_3$task_label[rows_of_interest])
    if(length(Sonic_Depth_Finish_rows) > 0) {
      num_of_cols <- length(grep("Sonic_Depth_Finish", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Sonic_Depth_Finish"
      }
      num_of_cols <- length(grep("Sonic_Depth_Finish", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Sonic_Depth_Finish", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Sonic_Depth_Finish_rows[k]]]
      }
      while(length(Sonic_Depth_Finish_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Sonic_Depth_Finish", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Sonic_Depth_Finish", colnames(clean_table_3)))
        crew_col <- grep("Sonic_Depth_Finish", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Sonic_Depth_Finish_rows[num_of_cols]]]
      }
    }
    
    Loran_Start_rows <- c()
    Loran_Start_rows <- grep("Loran_Start", clean_table_3$task_label[rows_of_interest])
    if(length(Loran_Start_rows) > 0) {
      num_of_cols <- length(grep("Loran_Start", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Loran_Start"
      }
      num_of_cols <- length(grep("Loran_Start", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Loran_Start", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Loran_Start_rows[k]]]
      }
      while(length(Loran_Start_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Loran_Start", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Loran_Start", colnames(clean_table_3)))
        crew_col <- grep("Loran_Start", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Loran_Start_rows[num_of_cols]]]
      }
    }
    
    Time_Finish_rows <- c()
    Time_Finish_rows <- grep("Time_Finish", clean_table_3$task_label[rows_of_interest])
    if(length(Time_Finish_rows) > 0) {
      num_of_cols <- length(grep("Time_Finish", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Time_Finish"
      }
      num_of_cols <- length(grep("Time_Finish", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Time_Finish", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Time_Finish_rows[k]]]
      }
      while(length(Time_Finish_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Time_Finish", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Time_Finish", colnames(clean_table_3)))
        crew_col <- grep("Time_Finish", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Time_Finish_rows[num_of_cols]]]
      }
    }
    
    Loran_Finish_rows <- c()
    Loran_Finish_rows <- grep("Loran_Finish", clean_table_3$task_label[rows_of_interest])
    if(length(Loran_Finish_rows) > 0) {
      num_of_cols <- length(grep("Loran_Finish", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Loran_Finish"
      }
      num_of_cols <- length(grep("Loran_Finish", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Loran_Finish", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Loran_Finish_rows[k]]]
      }
      while(length(Loran_Finish_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Loran_Finish", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Loran_Finish", colnames(clean_table_3)))
        crew_col <- grep("Loran_Finish", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Loran_Finish_rows[num_of_cols]]]
      }
    }
    
    Latitude_Longitude_Finish_rows <- c()
    Latitude_Longitude_Finish_rows <- grep("Latitude_Longitude_Finish", clean_table_3$task_label[rows_of_interest])
    if(length(Latitude_Longitude_Finish_rows) > 0) {
      num_of_cols <- length(grep("Latitude_Longitude_Finish", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Latitude_Longitude_Finish"
      }
      num_of_cols <- length(grep("Latitude_Longitude_Finish", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Latitude_Longitude_Finish", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Latitude_Longitude_Finish_rows[k]]]
      }
      while(length(Latitude_Longitude_Finish_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Latitude_Longitude_Finish", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Latitude_Longitude_Finish", colnames(clean_table_3)))
        crew_col <- grep("Latitude_Longitude_Finish", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Latitude_Longitude_Finish_rows[num_of_cols]]]
      }
    }
    
    Plankton_Nets_rows <- c()
    Plankton_Nets_rows <- grep("Plankton_Nets", clean_table_3$task_label[rows_of_interest])
    if(length(Plankton_Nets_rows) > 0) {
      num_of_cols <- length(grep("Plankton_Nets", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Plankton_Nets"
      }
      num_of_cols <- length(grep("Plankton_Nets", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Plankton_Nets", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Plankton_Nets_rows[k]]]
      }
      while(length(Plankton_Nets_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Plankton_Nets", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Plankton_Nets", colnames(clean_table_3)))
        crew_col <- grep("Plankton_Nets", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Plankton_Nets_rows[num_of_cols]]]
      }
    }
    
    Other_rows <- c()
    Other_rows <- grep("Other", clean_table_3$task_label[rows_of_interest])
    if(length(Other_rows) > 0) {
      num_of_cols <- length(grep("Other", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Other"
      }
      num_of_cols <- length(grep("Other", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Other", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Other_rows[k]]]
      }
      while(length(Other_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Other", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Other", colnames(clean_table_3)))
        crew_col <- grep("Other", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Other_rows[num_of_cols]]]
      }
    }
    
    Dredges_rows <- c()
    Dredges_rows <- grep("Dredges", clean_table_3$task_label[rows_of_interest])
    if(length(Dredges_rows) > 0) {
      num_of_cols <- length(grep("Dredges", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Dredges"
      }
      num_of_cols <- length(grep("Dredges", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Dredges", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Dredges_rows[k]]]
      }
      while(length(Dredges_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Dredges", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Dredges", colnames(clean_table_3)))
        crew_col <- grep("Dredges", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Dredges_rows[num_of_cols]]]
      }
    }
    
    Cameras_rows <- c()
    Cameras_rows <- grep("Cameras", clean_table_3$task_label[rows_of_interest])
    if(length(Cameras_rows) > 0) {
      num_of_cols <- length(grep("Cameras", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Cameras"
      }
      num_of_cols <- length(grep("Cameras", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Cameras", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Cameras_rows[k]]]
      }
      while(length(Cameras_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Cameras", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Cameras", colnames(clean_table_3)))
        crew_col <- grep("Cameras", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Cameras_rows[num_of_cols]]]
      }
    }
    
    Cores_rows <- c()
    Cores_rows <- grep("Cores", clean_table_3$task_label[rows_of_interest])
    if(length(Cores_rows) > 0) {
      num_of_cols <- length(grep("Cores", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Cores"
      }
      num_of_cols <- length(grep("Cores", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Cores", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Cores_rows[k]]]
      }
      while(length(Cores_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Cores", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Cores", colnames(clean_table_3)))
        crew_col <- grep("Cores", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Cores_rows[num_of_cols]]]
      }
    }
    
    Trawls_rows <- c()
    Trawls_rows <- grep("Trawls", clean_table_3$task_label[rows_of_interest])
    if(length(Trawls_rows) > 0) {
      num_of_cols <- length(grep("Trawls", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Trawls"
      }
      num_of_cols <- length(grep("Trawls", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Trawls", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Trawls_rows[k]]]
      }
      while(length(Trawls_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Trawls", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Trawls", colnames(clean_table_3)))
        crew_col <- grep("Trawls", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Trawls_rows[num_of_cols]]]
      }
    }
    
    Biological_Gear_rows <- c()
    Biological_Gear_rows <- grep("Biological_Gear", clean_table_3$task_label[rows_of_interest])
    if(length(Biological_Gear_rows) > 0) {
      num_of_cols <- length(grep("Biological_Gear", colnames(clean_table_3)))
      if(num_of_cols == 0) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- "Biological_Gear"
      }
      num_of_cols <- length(grep("Biological_Gear", colnames(clean_table_3)))
      for(k in 1:num_of_cols) {
        crew_col <- grep("Biological_Gear", colnames(clean_table_3))[k]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Biological_Gear_rows[k]]]
      }
      while(length(Biological_Gear_rows) > num_of_cols) {
        clean_table_3 <- cbind(clean_table_3, NA)
        colnames(clean_table_3)[ncol(clean_table_3)] <- paste0("Biological_Gear", "_", num_of_cols + 1)
        num_of_cols <- length(grep("Biological_Gear", colnames(clean_table_3)))
        crew_col <- grep("Biological_Gear", colnames(clean_table_3))[num_of_cols]
        clean_table_3[rows_of_interest, crew_col] <- clean_table_3$value[rows_of_interest[Biological_Gear_rows[num_of_cols]]]
      }
    }
  }
}

#now we can remove the "task_label" and "value" columns
to_delete <- which(colnames(clean_table_3) == "task_label")
clean_table_3 <- clean_table_3[,-to_delete]
to_delete <- which(colnames(clean_table_3) == "value")
clean_table_3 <- clean_table_3[,-to_delete]


write.csv(clean_table_3, "Cape_Hatteras_table_3.csv", row.names = F)

##################################################################################

clean_table_4 <- read.csv("Cape_Hatteras_table_3.csv")

tmp <- unique(clean_table_4)

#sort the columns of our data to keep things cleaner
tmp <- tmp[order(tmp$filename, tmp$workflow_name),]
to_order <- c(colnames(tmp)[grep("filename", colnames(tmp))], 
              colnames(tmp)[grep("workflow_name", colnames(tmp))], 
              colnames(tmp)[grep("Sheet_No", colnames(tmp))], 
              colnames(tmp)[grep("Master_and_Mates", colnames(tmp))], 
              colnames(tmp)[grep("Oceanographic_Party_Chief", colnames(tmp))], 
              colnames(tmp)[grep("Principal_Investigator", colnames(tmp))], 
              colnames(tmp)[grep("Cruise_Number", colnames(tmp))], 
              colnames(tmp)[grep("Station_Number", colnames(tmp))], 
              colnames(tmp)[grep("Date_Start", colnames(tmp))], 
              colnames(tmp)[grep("Time_Start", colnames(tmp))], 
              colnames(tmp)[grep("Time_Finish", colnames(tmp))], 
              colnames(tmp)[grep("Loran_Start", colnames(tmp))], 
              colnames(tmp)[grep("Loran_Finish", colnames(tmp))], 
              colnames(tmp)[grep("Latitude_Longitude_Start", colnames(tmp))], 
              colnames(tmp)[grep("Latitude_Longitude_Finish", colnames(tmp))], 
              colnames(tmp)[grep("Sonic_Depth_Start", colnames(tmp))], 
              colnames(tmp)[grep("Sonic_Depth_Finish", colnames(tmp))], 
              colnames(tmp)[grep("Cores", colnames(tmp))], 
              colnames(tmp)[grep("Trawls", colnames(tmp))], 
              colnames(tmp)[grep("Plankton_Nets", colnames(tmp))], 
              colnames(tmp)[grep("Biological_Gear", colnames(tmp))], 
              colnames(tmp)[grep("Cameras", colnames(tmp))], 
              colnames(tmp)[grep("Hydrographic_Gear", colnames(tmp))], 
              colnames(tmp)[grep("Dredges", colnames(tmp))], 
              colnames(tmp)[grep("Other", colnames(tmp))])
tmp <- tmp[,to_order]

#set checkpoint
write.csv(tmp, "Cape_Hatteras_table_4.csv", row.names = F)

##################################################################################

#In these last blocks of code, I tried to find any patterns within the sets of columns 
#to try to narrow down the amount of data as best I could
clean_table_5 <- read.csv(file = "Cape_Hatteras_table_4.csv")

#if "unclear" is written anywhere, just label that value as "unclear"
#also, capitalize all letters to help reduce variations between inputted words
for(i in 1:ncol(clean_table_5)) {
  clean_table_5[,i] <- as.character(clean_table_5[,i])
  clean_table_5[,i] <- gsub("\\\\n", " ", clean_table_5[,i], ignore.case = F)
  clean_table_5[,i] <- gsub("\\?", "unclear", clean_table_5[,i], ignore.case = F)
  clean_table_5[,i] <- toupper(clean_table_5[,i])
}

#ckeck again for extra white space
for(i in 1:ncol(clean_table_5)) {
  clean_table_5[,i] <- trimws(clean_table_5[,i], which = "both")
}

#substitute multiple white spacing to 1 space
pb <- txtProgressBar(min = 0, max = nrow(clean_table_5), initial = 0, style = 3)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  
  unclear_cols <- grep("unclear", clean_table_5[i,], ignore.case = T)
  clean_table_5[i,unclear_cols] <- "[unclear]"
  for(j in 1:ncol(clean_table_5)) {
    if(!is.na(clean_table_5[i,j])) {
      string <- clean_table_5[i,j]
      string <- trimws(gsub("\\s+", " ", string))
      clean_table_5[i,j] <- string
    }
  }
}


#The following blocks of code will try to filter for different types of data

#filter Sheet_No columns
coi <- grep("Sheet_No", colnames(clean_table_5))

setTxtProgressBar(pb, i)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  
  to_edit <- which((clean_table_5[i,] == "\\") == T)
  clean_table_5[i,to_edit] <- NA
  for(j in coi) {
    clean_table_5[i,j] <- gsub("0f", "OF", clean_table_5[i,j], ignore.case = T)
  }
}


#filter Cruise_Number
coi <- grep("Cruise_Number", colnames(clean_table_5))

setTxtProgressBar(pb, i)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  for(j in coi) {
    clean_table_5[i,j] <- gsub(" ", "", clean_table_5[i,j], ignore.case = T)
  }
}


#filter Station_Number
coi <- grep("Station_Number", colnames(clean_table_5))

setTxtProgressBar(pb, i)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  for(j in coi) {
    clean_table_5[i,j] <- gsub("\\s*\\([^\\)]+\\)", " ", clean_table_5[i,j])
    clean_table_5[i,j] <- gsub(" ;", ", ", clean_table_5[i,j])
    clean_table_5[i,j] <- gsub(" :", ", ", clean_table_5[i,j])
    clean_table_5[i,j] <- gsub(":", ", ", clean_table_5[i,j])
    clean_table_5[i,j] <- gsub(";", ", ", clean_table_5[i,j])
    clean_table_5[i,j] <- gsub("/", ", ", clean_table_5[i,j])
    clean_table_5[i,j] <- trimws(gsub("\\s+", " ", clean_table_5[i,j]))
    clean_table_5[i,j] <- gsub(" ", ", ", clean_table_5[i,j])
    clean_table_5[i,j] <- gsub(",,", ",", clean_table_5[i,j])
  }
}

for(i in 1:ncol(clean_table_5)) {
  clean_table_5[,i] <- trimws(clean_table_5[,i], which = "both")
}


#unable to filter Dates, and Latitudes

#filter Sonic_Depth
coi <- grep("Sonic_Depth", colnames(clean_table_5))

setTxtProgressBar(pb, i)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  for(j in coi) {
    clean_table_5[i,j] <- gsub("\\D+", " ", clean_table_5[i,j], ignore.case = T)
    clean_table_5[i,j] <- trimws(gsub("\\s+", " ", clean_table_5[i,j]))
  }
}


#filter Plankton_Nets
coi <- grep("Plankton_Nets", colnames(clean_table_5))

setTxtProgressBar(pb, i)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  for(j in coi) {
    clean_table_5[i,j] <- gsub("MID-WATER", "MIDWATER", clean_table_5[i,j], ignore.case = T)
    clean_table_5[i,j] <- gsub("MID WATER", "MIDWATER", clean_table_5[i,j], ignore.case = T)
    clean_table_5[i,j] <- gsub("MID=WATER", "MIDWATER", clean_table_5[i,j], ignore.case = T)
    clean_table_5[i,j] <- gsub(paste0("[[:digit:]]", " "), "M", clean_table_5[i,j], ignore.case = T)
  }
}


#filter Hydrographic_Gear
coi <- grep("Hydrographic_Gear", colnames(clean_table_5))

setTxtProgressBar(pb, i)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  for(j in coi) {
    clean_table_5[i,j] <- gsub("/ ", "/", clean_table_5[i,j], ignore.case = T)
    clean_table_5[i,j] <- gsub("/", " ", clean_table_5[i,j], ignore.case = T)
    clean_table_5[i,j] <- gsub("-", " ", clean_table_5[i,j], ignore.case = T)
    clean_table_5[i,j] <- gsub(".", "", clean_table_5[i,j], ignore.case = T)
    clean_table_5[i,j] <- gsub("C T D", "CTD", clean_table_5[i,j], ignore.case = T)
  }
}

#filter Other
coi <- grep("Other", colnames(clean_table_5))

setTxtProgressBar(pb, i)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  for(j in coi) {
    clean_table_5[i,j] <- gsub(". ", ".", clean_table_5[i,j], ignore.case = T)
  }
}


#remove duplicates
get_colnames <- unique(gsub(paste0("_", "[[:digit:]]"), "", colnames(clean_table_5)))
pb <- txtProgressBar(min = 0, max = nrow(clean_table_5), initial = 0, style = 3)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  for(j in 1:length(get_colnames)) {
    coi <- grep(get_colnames[j], colnames(clean_table_5))
    new_vector <- c()
    if(!is.na(clean_table_5[i,coi])[[1]]) {
      if(length(clean_table_5[i,coi]) > 1) {
        for(k in 1:length(coi)) {
          new_vector <- c(new_vector, clean_table_5[i,coi][1,k])
        }
        new_vector[duplicated(new_vector)] <- NA
        new_vector <- c(new_vector[!is.na(new_vector)], new_vector[is.na(new_vector)])
        clean_table_5[i,coi] <- new_vector
      }
    }
  }
}
row_with_dups <- c()
for(i in 1:nrow(clean_table_5)) {
  no_dups_this_row <- all(is.na(clean_table_5[i,grep("[[:digit:]]", colnames(clean_table_5))]))
  if(no_dups_this_row == FALSE) {
    row_with_dups <- c(row_with_dups, i)
  }
}
length(row_with_dups)
#~31,000
#30,994
#30,993
#28,919
#28,914
#28,737


#check for, and remove, completely NA columns
col_to_remove <- c()
for(i in 1:ncol((clean_table_5))) {
  empty_col <- all(is.na(clean_table_5[,i]))
  print(empty_col)
  if(empty_col == T) {
    col_to_remove <- c(col_to_remove, i)
  }
}
clean_table_5 <- clean_table_5[,-col_to_remove]

#shorten workflow_names of the column numbers to just a number
pb <- txtProgressBar(min = 0, max = nrow(clean_table_5), initial = 0, style = 3)
for(i in 1:nrow(clean_table_5)) {
  setTxtProgressBar(pb, i)
  clean_table_5$workflow_name[i] <- gsub("COLUMN_", "", clean_table_5$workflow_name[i])
}

clean_table_5 <- clean_table_5[order(clean_table_5$filename, clean_table_5$workflow_name),]
clean_table_5$workflow_name <- as.numeric(clean_table_5$workflow_name)
colnames(clean_table_5)[2] <- "Column_Number"
rownames(clean_table_5) <- NULL

#final output of the Cape Hatteras data
write.csv(clean_table_5, file = "Cape_Hatteras_Final.csv", row.names = F)

















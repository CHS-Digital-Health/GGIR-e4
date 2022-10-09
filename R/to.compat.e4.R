to.compat.e4 <- function(zipLocation, csvlocation.GGIRout, tz) {
  zipDIR = zipLocation
  zipFiles <- list.files(zipLocation, pattern = "*.zip", full.names = FALSE)
  
  if (!dir.exists(csvlocation.GGIRout) == TRUE) {
    dir.create(csvlocation.GGIRout, recursive = TRUE)
  }
  
  for (fileName in zipFiles) {
    ACC_TEMP <- NULL
    currZip <- paste(zipLocation, "/", fileName, sep = "")
    ACC_single <- read.csv(unzip(currZip,
            unzip = "internal", exdir = tempdir(), files = "ACC.csv"), 
            sep = ",", header = FALSE)
    StartTime <- ACC_single[1, 1]
    SamplingRate <- ACC_single[2, 1]
    ACC_single <- ACC_single[-c(1:2), ]
    ACC_single <- as.data.frame(ACC_single)
    
    E4_serial <- substring(fileName, regexpr("_", fileName) + 1)
    E4_serial <- substr(E4_serial, 1, 6)
    
    EndTime <- (StartTime + round((nrow(ACC_single)/SamplingRate), 0))
    ACC_single$ts <- seq(from = StartTime * 1000, 
                         to = EndTime * 1000, length.out = nrow(ACC_single))
    
    
    TEMP_single <- read.csv(unzip(currZip, 
              unzip = "internal", exdir = tempdir(), files = "TEMP.csv"), 
              sep = ",", header = FALSE)
    StartTime_TEMP <- TEMP_single[1, 1]
    SamplingRate_TEMP <- TEMP_single[2, 1]
    TEMP_single <- TEMP_single[-c(1:2), ]
    TEMP_single <- as.data.frame(TEMP_single)
    EndTime_TEMP <- (StartTime_TEMP + round((nrow(TEMP_single)/SamplingRate_TEMP), 0))
    TEMP_single$ts <- seq(from = StartTime_TEMP * 1000, 
                          to = EndTime_TEMP * 1000, length.out = nrow(TEMP_single))
    
    ACC_single_table <- data.table::as.data.table(ACC_single)
    TEMP_single_table <- data.table::as.data.table(TEMP_single)
    data.table::setkey(ACC_single_table, ts)
    data.table::setkey(TEMP_single_table, ts)
    ACC_TEMP_SINGLE <- TEMP_single_table[ACC_single_table, roll = "nearest"]
    ACC_TEMP_SINGLE$serial <- E4_serial
    ACC_TEMP <- rbind(ACC_TEMP, ACC_TEMP_SINGLE)

    
    ACC_TEMP$ts <- round(ACC_TEMP$ts/1000, 0)
    names(ACC_TEMP) <- c("temp", "ts", "acc_x", "acc_y", "acc_z", "E4serial")
    data.table::setcolorder(ACC_TEMP, c("ts", "E4serial", "acc_x", "acc_y", "acc_z", "temp"))
    overall_start <- ((anytime::anytime(min(as.numeric((ACC_TEMP$ts))), tz = tz)))
    ACC_TEMP <- ACC_TEMP[, `:=`(E4serial, NULL)]
    
    names(ACC_TEMP) <- c("timestamp", "acc_x_bits", "acc_y_bits", "acc_z_bits", "temp")
    data.table::setcolorder(ACC_TEMP, c("timestamp", "acc_x_bits", "acc_y_bits", "acc_z_bits", "temp"))
  
  
    filename <- paste(csvlocation.GGIRout, "GGIR_out_", strsplit(fileName, "_")[[1]][1], ".csv", sep = "")
    print(head(ACC_TEMP))
    write.table(ACC_TEMP, file = filename, quote = FALSE, row.names = FALSE, 
                     col.names = TRUE, sep = ",", na = "")
  }

  
}

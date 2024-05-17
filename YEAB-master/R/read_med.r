#' @title Process MED to csv based on standard data structure event.time
#'
#' @param fname chr, the name of a MED file to read; can include the directory
#' @param save_file logical, save csv on the disk? TRUE or FALSE (default)
#' @param path_save chr, directory to save csv files if save_file is TRUE;
#' @param col_r chr, MED array to read (may be an event.time variable; see Details)
#' @param col_names chr, a vector of column names
#' @param out logical, if true returns the data.frame of n x 2
#' @param time_dot_event logical, if true, assumes that array to process has a time.event format
#' @param num_col int, corresponds to DISKCOLUMNS of MED
#' @param ... other arguments passed to \code{\link{read.table}}
#'
#' @return if out is true, returns a data.frame; if save_file is TRUE, writes the data.frame in csv format at path_save
#' @export
#'
#' @details The default behavior of this function has time_dot_event = TRUE, 
#' which means that the raw MED can be should be in time.event convention.
#'    For example, if a response is coded as 23, the time is in 1/100 seconds and a response
#'    occurred at 2 minutes, the event is saved in, say, column C as 6000.23. This will be
#'    processed as
#'    time event
#'    6000  23
#'
#' However, if time_dot_event = FALSE, the output will be a data.frame with one column
#'   values. For example
#'  values
#'   6000.23
#' 
#' @examples
#' # read raw data from MED
#' data("fi60_raw_from_med")
#' # see first 10 lines
#' head(fi60_raw_from_med, 10)
#' # now write the data as txt or any extension
#' writeLines(fi60_raw_from_med, "fi60_raw.txt")
#' file_name <- "fi60_raw.txt"
#' path_to_save <- getwd() # change to something like "data/processed/"
#' fi60_processed <- read_med(fname = file_name, save_file = TRUE,
#'   path_save = path_to_save, col_r = "C:", out = TRUE,
#'   col_names = c("time", "event"), num_col = 6, time_dot_event = TRUE)
#' head(fi60_processed)
#' # __________________________________________________________________________
#' ## To use in bulk
#' # 1) Generate a list of filenames of raw MED data
#' # 2) Loop over the list with the function, using each element
#' #    of the list as the fname argument.
#' # __________________________________________________________________________
#' # Suppose all raw MED files start with 2020, and you are in the working directory
#' # If all the raw MED files are in the wd, we can directly get the filenames
#' # with unspecified path
#' # filenames <- list.files(pattern = "^2020")
#' # The above line will look in the wd for all the files starting with "2020"
#' # and it will save it as a vector of strings in "filenames".
#' # With that vector, make a for loop like the following:
#' # __________________________________________________________________________
#' # If you want to work immediately with the processed data, first create an empty
#' # dataframe to store the data file per file
#' # df_working = data.frame()
#' # __________________________________________________________________________
#' # for (f in filenames) {
#' #   df_tmp <- read_med(fname = f,
#' #                     path_save = "data/processed/", # put here your path to save the csv
#' #                     col_r = 'C:', # if the time.event vector is saved in variable C
#' #                     out = TRUE ) # If you want to store processed data in df_tmp,
#' # otherwise write out = FALSE
#' # now append at rows the new data.frame
#' #   df_working = rbind(df_working, df_tmp)
#' # }
#' # Thats all.
#'
read_med <- function(fname, # Name of the MED file to read;
                     # can include the path directory
                     save_file = FALSE, # Save csv? TRUE or FALSE (default)
                     path_save = NULL, # Path to folder to save csv files;
                     # e.g. /Dropbox/exp1/Phase_1/
                     col_r = "C:", # Variable of MED-raw to read
                     col_names = c("time", "event"),
                     out = TRUE,
                     num_col = 6, # corresponds to DISKCOLUMNS of MED
                     time_dot_event = TRUE,
                     ...) { # store the output file in memory?
  # this will return the data.frame in RAM
  # available to work on it immediatly.
  options(stringsAsFactors = FALSE)
  dfx <- read.table(fname,
    skip = 3, na.strings = "NA", fill = TRUE,
    col.names = paste0("V", seq_len(num_col)),
    ...
  )

  # Create a numeric vector of the positions where dfx have "0:",
  # wich in MED is the start of an array. This will tell us where
  # the variables are, including our col_r variable ('C:' by default)
  a <- which((dfx$V1 == "0:"))

  # Look where is col_r (where is "C:" by default), and add 1 to that
  # position, because it is where MED start to count the array ("0:")
  col_pos <- which(dfx$V1 == col_r) + 1

  # Of the positions stored in "a", which of them is col_pos?
  idx1 <- col_pos # a[a == col_pos]
  # And which of them is the NEXT position? That is, where col_r ends

  # If col_r ("C:" by default) is the LAST variable, idx2 will be empty,
  # if that's the case, we'll take the last row of dfx
  idx2 <- min(c(a[which(a == idx1) + 1] - 2, nrow(dfx)))

  # if (length(idx2) == 0) {
  #   idx2 <- nrow(dfx)
  # }
  # Handle case when col_r is not found
  if (length(col_pos) == 0 || length(idx2) == 0) {
    stop("Specified column not found in the data")
  }

  # Now, we slice dfx in positions idx1 to idx2
  varY <- dfx[idx1:idx2, ]

  varY[1] <- NULL # Because 1st column are just row names

  # Stack matrix in two vectors
  varY <- na.omit(stack(varY))
  # Erase second vector
  varY[2] <- NULL

  # Drop all 0s (there's nothing interesting there)
  varY <- varY$values
  varY = as.numeric(varY)
  if (time_dot_event) {
    # This splits the time.event vector in two
    var_tmp <- do.call(rbind, strsplit(as.character(varY), "\\."))
    # And this creates a dataframe with two columns (time and event)
    var_tmp <- as.data.frame(var_tmp)
    # This assigns names to the columns
    if (ncol(var_tmp) > 1) {
      colnames(var_tmp) <- col_names
    }
    # Removes 0s from the first variable
    var_tmp <- var_tmp[var_tmp[, 1] > 0, ]
    # This converts variables to numeric class
    var_tmp[] <- lapply(var_tmp, as.numeric)
    # sort by time
    var_tmp <- var_tmp[order(var_tmp[, 1]), ]
    row.names(var_tmp) <- NULL
  } else {
    var_tmp <- data.frame(values = as.numeric(varY))
    # colnames(var_tmp) <- col_r
  }

  # Write to CSV if required
  if (save_file) {
    filesave <- sprintf("%s/%s.csv", path_save, sub("\\..*$", "", basename(fname)))
    write.csv(var_tmp, file = filesave, row.names = FALSE)
  }

  # if our is true (T), this returns (and save in memory) var_temp
  if (out) {
    var_tmp
  }
}

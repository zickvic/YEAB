# event_extractor.R
# Description:
#   A function to slice MED data based on start and stop events. This function
#   should be used after read_med.r, which outputs a csv of 2 columns: time and
#   events (in that order). Its use is exemplified at the end of the function.

# Creation: April 2020, during a pandemic
#   Author: Emmanuel Alcala

# Inputs:
#   dframe: a dataframe of m rows x 2 columns, where columns corresponds
#           to time and events IDs, in that order.
#   ev0: event ID start (where the event we want to extract begins)
#   ev1: event ID stop. This event won't be returned, so keep in mind that
#   evname: a string for the event name, for identification purposes. For example
#           if the event we want to extract is component 1 in a multiple-2 sche-
#           dule, this can be eventname = "c1", so when we extract the second
#           component we can row-combine both in a unique dataframe.
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Outputs:
#   dftmp: data frame with j x 4 columns of time, events, cum_id and evname

#' @title Event extractor
#' 
#' @description  A function to slice data based on start and stop events. This function
#'   should be used after read_med.r, which outputs a csv of 2 columns: time and
#'   events (in that order). Its use is exemplified at the end of the function.
#'
#' @param dframe data frame with events ev0 and ev1 (e.g., start of trial and reinforcement delivery)
#' @param ev0 event ID start (where the event we want to extract begins)
#' @param ev1 event ID stop. This event won't be returned, so keep in mind that
#' @param evname a string for the event name, for identification purposes. For example
#'    if the event we want to extract is component 1 in a multiple-2 schedule,
#'    this can be eventname = "c1", so when we extract the second
#'    component we can row-combine both in a unique dataframe.
#' @return data frame with nrows x 4 columns of time, events, cum_id and evname
#' @export
#' @details Works by trials
#' @examples
#' If we have a component starting with 5 and ending with 2 and a dataframe "df"
#' # we can extract the data of component "comp52" following the next steps:
#' # 0 - From the output of read_med.R function, load the csv file and assign to df
#' # 1 - source the event_extractor.R function
#' # 2 - use it with the appropiate arguments as follows
#' component52df <- event_extractor(
#'   dframe = df, # enter the data as the 1st arg
#'   ev0 = 5, ev1 = 2, # enter start and stop
#'   evname = "comp52"
#' ) # enter the event's name
#'
event_extractor <- function(dframe, ev0, ev1, evname) {
  evs <- c(ev0, ev1)

  # Boolean variable where there is either an ev0 or ev1
  mark.v <- ifelse(dframe[, 2] %in% evs, 1, 0)
  # Make a cumulative sum of events
  dframe$cum_id <- cumsum(mark.v)
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ## Slice the event based on cumsum of start and end. For an alternative, see
  ## 'alternative slicing' at the end of the script.
  # The above lines make a vector (cum_id) of integers with odd numbers corresponding
  # to the event we want to extract. It starts to count 1 where there is ev0,
  # and 2 when there is ev1, 3 when ev0 again. So, even numbers corresponds to
  # the end of the events of interest. We will use this information next, using
  # the %% (module) operator, which returns the remainder of a division (not the
  # result of the division).  4 %% 2 equals 0, while 4 / 2 = 2

  # The operation x %% 2 == 1 evaluates if x/2 has a remainder of 1, or if is
  # an exact multiple of 2 (remainder of 0). This will make a boolean variable
  # that we'll use to slice data in the form dframe[TRUE, ]

  # 1
  event_remover <- dframe$cum_id %% 2 == 1
  # 2
  dftmp <- dframe[event_remover, ]

  #dftmp[, 4] <- evname
  dftmp$evname <- evname
  # return dftmp
  dftmp
}

# How to use ----
# If we have a component starting with 5 and ending with 2 and a dataframe "df"
# we can extract the data of component "comp52" following the next steps:

# 0 - From the output of read_med.R function, load the csv file and assign to df
# 1 - source the event_extractor.R function
# 2 - use it with the appropiate arguments as follows
# component52df <- event_extractor(dframe = df, # enter the data as the 1st arg
#                                  ev0 = 5, ev1 = 2, # enter start and stop
#                                  evname = "comp52") # enter the event's name
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# alternative slicing ----
# The code below can replace # 1 and # 2 above, but I don't see any reason beyond
# making the code more readable.

# slicingvec <- which(dframe$evento %in% evs)
# slicingvec <- matrix(slicingvec, ncol = 2, byrow = T)
#
# dftmp <- apply(
#   slicingvec, 1, function(x){
#   dframe[x[1]:(x[2] - 1), ]
# })
#
# dftmp <- do.call(rbind, dftmp)

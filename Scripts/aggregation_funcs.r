f_mean_na <- function(x, percentage_na = 0.2, consec = F, consec_values = 5) {
  # set consec to true if consecutive missing values are needed to be taken into 
  # account
  # If consec is true, set the number to how many values should be taken into 
  # account
  if(consec == F) {
    if(sum(is.na(x))/length(x) > percentage_na) {NA} else {mean(x, na.rm = T)}
  } else if (consec == T) {
    i <- x
    i[is.na(i)] <- -9999
    i <- rle(i)
    i <- any(i$lengths[which(i$values==-9999)] >= consec_values)
    if ((sum(is.na(x))/length(x) > percentage_na) | i == TRUE) {
      NA
    } else {
      mean(x, na.rm = T)
    }
  }
} 

f_sum_na <- function(x, percentage_na = 0, consec = F) {
  # set consec to true if consecutive missing days are neede to be taken into 
  # account
  if(consec == F) {
    if(sum(is.na(x))/length(x) > percentage_na) {NA} else {sum(x, na.rm = T)}
  } else if (consec == T) {
    i <- x
    i[is.na(i)] <- -9999
    i <- rle(i)
    i <- any(i$lengths[which(i$values==-9999)] >= 5)
    if ((sum(is.na(x))/length(x) > percentage_na) | i == TRUE) {
      NA
    } else {
      sum(x, na.rm = T)
    }
  }
} 

f_min_na <- function(x, percentage_na = 0) {
  if(sum(is.na(x))/length(x) > percentage_na) {NA} else {min(x, na.rm = T)}
} 

f_max_na <- function(x, percentage_na = 0) {
  if(sum(is.na(x))/length(x) > percentage_na) {NA} else {max(x, na.rm = T)}
}
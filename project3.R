### LIBRARIES

### PROJECT 3

## TASK 1

data <- read.csv("cloud.csv", stringsAsFactors = TRUE)

## TASK 2

# price and cloud_storage ordinal factors
data$price <- factor(data$price, order = TRUE, 
                     levels = c("p6", "p12", "p18"))

data$cloud_storage <- factor(data$cloud_storage, order = TRUE,
                             levels = c("30gb", "2000gb", "5000gb"))

## TASK 3

data_p3t3 <- data

# adding price_n
for(i in 1:nrow(data_p3t3)){
  if(data_p3t3[i, 8] == "p6"){
    data_p3t3[i, 10] = 6
  } else if(data_p3t3[i, 8] == "p12"){
    data_p3t3[i, 10] = 12
  } else {
    data_p3t3[i, 10] = 18
  }
}

names(data_p3t3)[names(data_p3t3) == 'V10'] <- 'price_n'

# price_n mean (12.01533)
mean(data_p3t3$price_n)

## TASK 4

# filtering dataset for choice == 1
data_p3t4 <- data_p3t3

data_p3t4 <- filter(data_p3t4, choice == 1)

# times 30gb was chosen (830)
table(data_p3t4$cloud_storage)

# percentage that chose email only (20.8%)
prop.table(table(data_p3t4$cloud_services)) * 100

## TASK 5

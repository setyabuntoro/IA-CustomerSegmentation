# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# © Copyright IBM Corp. 2019. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

# Load Data from CSV files

library(scales)

readDataset <- function(fileName) { readr::read_csv(file.path("..", "..", "datasets", fileName)) }

customer <- readDataset("customer.csv")
customer_summary <- readDataset("customer_summary.csv")
account <- readDataset("account.csv")
# account_summary <- readDataset("account_summary.csv")

clients <- list(
  list(name="Leo Rakes", image="6M.jpg"),
  list(name="Catalina Santos", image="9F.jpg"),
  list(name="Thomas Owens", image="23M.jpg"),
  list(name="Liliana Hunnisett", image="10F.jpg"),
  list(name="Jeffery Smith", image="5M.jpg"),
  list(name="Jesica Abrams", image="22F.jpg"),
  list(name="Carla Warnes", image="25F.jpg"),
  list(name="Paige Carson", image="1F.jpg"),
  list(name="Alex Anderson", image="2M.jpg"),
  list(name="Ian Gray", image="3M.jpg"),
  list(name="Jane Wilson", image="8F.jpg"),
  list(name="Robert Taylor", image="4M.jpg")
)
clientIds <- c(1000:1010)
names(clients) <- clientIds

for(id in clientIds) {
  clients[[toString(id)]]$income <- dollar(customer[customer$CUSTOMER_ID == id,][[1,'ANNUAL_INCOME']])
}

coldToDrop <- c(
  "CUSTOMER_SUMMARY_FUNDS_UNDER_MANAGEMENT_mean",
  "CUSTOMER_SUMMARY_FUNDS_UNDER_MANAGEMENT_min",
  "CUSTOMER_SUMMARY_FUNDS_UNDER_MANAGEMENT_max",
  "CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_min",
  "CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_max",
  "CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_sum",
  "CUSTOMER_ANNUAL_INCOME",
  "CUSTOMER_NUMBER_OF_DEPENDENT_CHILDREN",
  "CUSTOMER_TENURE",
  "NUM_ACCOUNTS_WITH_RISK_TOLERANCE_MODERATE",
  "NUM_ACCOUNTS_WITH_RISK_TOLERANCE_HIGH",
  "NUM_ACCOUNTS_WITH_RISK_TOLERANCE_VERY_LOW",
  "NUM_ACCOUNTS_WITH_RISK_TOLERANCE_LOW",
  "CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_current_vs_6_months_ago",
  "CUSTOMER_SUMMARY_TOTAL_AMOUNT_OF_DEPOSITS_max_min_ratio"
)
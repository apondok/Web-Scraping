cat("\014")
rm(list=ls())

library(robotstxt)
library(rvest)
library(dplyr)
library(xml2)
library(jsonlite)
library(tidyverse)
library(scales)
library(ggplot2)
library(base)
library(h2o)
library(pdftools)
library(tabulizer)
library(httr)
library(stringr)
library(xts)
library(readxl)

# readinputs <- function()
# {

# 2 Things to edit manually for now

month.year <- "0719"
m <- 7
ytd <- "YTD 2019"

start_1 <- paste(month.abb[m],"2018",sep = " ")

site <- "http://www.freddiemac.com/investors/financials/monthly-volume-summaries.html"
save.pdf <- "C:/Users/Alex Pondok/Desktop/CMD Research and Developemt/CMD_PDF/freddie.pdf"
save.csv <- "C:/Users/Alex Pondok/Desktop/CMD Research and Developemt/CMD_CSV/freddie.csv"
# START HERE
s <- html_session(site)
part <- parse_url(site)
#get the link in the mainFrame iframe holding the pdf
mmyy <- month.year
pdf_link <- s %>%
  read_html() %>%
  html_nodes(xpath=paste("//a[@href='/investors/financials/pdf/",mmyy,"mvs.pdf']",sep= "")) %>%
  html_attr("href")
#get the pdf link, download it, and save it
grab_pdf <- paste0(part$scheme,"://",part$hostname,"/",pdf_link)
download.file(grab_pdf, save.pdf, mode="wb")
# PDF to text file and data.frame Table 1
freddie_mac <- pdf_text(save.pdf)
length(freddie_mac)
# Page 1
portfolio_profile <- freddie_mac[1]
# Page 2
portfolio_profile_2 <- freddie_mac[2]
# Page 1
portfolio_profile <- trimws(portfolio_profile)
# Page 2
portfolio_profile_2 <- trimws(portfolio_profile_2)
# Page 1
portfolio_profile <- strsplit(portfolio_profile, "\r\n")
# Page 2
portfolio_profile_2 <- strsplit(portfolio_profile_2, "\r\n")
# Page 1
portfolio_profile <- portfolio_profile[[1]]
# Page 2
portfolio_profile_2 <- portfolio_profile_2[[1]]
# Page 1
portfolio_profile_1_1 <- portfolio_profile[as.numeric(gsub("L","",grep(start_1, portfolio_profile)[1])):as.numeric(gsub("L","",grep(ytd, portfolio_profile)[1]))]
portfolio_profile_1_2 <- portfolio_profile[as.numeric(gsub("L","",grep(start_1, portfolio_profile)[2])):as.numeric(gsub("L","",grep(ytd, portfolio_profile)[2]))]
# Page 2
portfolio_profile_2_1 <- portfolio_profile_2[as.numeric(gsub("L","",grep(start_1, portfolio_profile_2)[1])):as.numeric(gsub("L","",grep(ytd, portfolio_profile_2)[1]))]
portfolio_profile_2_2 <- portfolio_profile_2[as.numeric(gsub("L","",grep(start_1, portfolio_profile_2)[2])):as.numeric(gsub("L","",grep(ytd, portfolio_profile_2)[2]))]
# Page 1
portfolio_profile_1_1 <- str_split_fixed(portfolio_profile_1_1," {2,}", 8)
portfolio_profile_1_s <- str_split_fixed(portfolio_profile_1_1[,8]," ", 3)
portfolio_profile_1_s <- portfolio_profile_1_s[,-2]
portfolio_profile_1_s <- portfolio_profile_1_s[,-2]
portfolio_profile_1_1 <- portfolio_profile_1_1[,-8]
portfolio_profile_1_1 <- data.frame(portfolio_profile_1_1, stringsAsFactors = FALSE)
portfolio_profile_1_s <- data.frame(portfolio_profile_1_s, stringsAsFactors = FALSE)
portfolio_profile_1_1 <- cbind(portfolio_profile_1_1,portfolio_profile_1_s)
portfolio_profile_1_2 <- str_split_fixed(portfolio_profile_1_2," {2,}", 13)
# Page 2
portfolio_profile_2_1 <- str_split_fixed(portfolio_profile_2_1," {2,}", 15)
portfolio_profile_2_2 <- str_split_fixed(portfolio_profile_2_2," {2,}", 16)
# Page 1
portfolio_profile_1_1 <- data.frame(portfolio_profile_1_1, stringsAsFactors = FALSE)
portfolio_profile_1_1[, 1] <- ifelse(portfolio_profile_1_1[, 1] == "", portfolio_profile_1_1[, -1],)
names(portfolio_profile_1_1) <- c("Month - Table 1","Purchases or Issuances ","Sales","Liquidations","Net Increase/(Decrease)",
                                  "Ending Balance","Annualized Growth Rate","Annualized LiquidationRate")
portfolio_profile_1_1 <- portfolio_profile_1_1[portfolio_profile_1_1$Sales != "", ]
portfolio_profile_1_2 <- data.frame(portfolio_profile_1_2, stringsAsFactors = FALSE)
names(portfolio_profile_1_2) <- c("Month - Table 2","Purchases","Sales","Liquidations","Ending Balance",
                                  "Annualized Growth Rate","Annualized Liquidation Rate",
                                  "Month - Table 3","Freddie Mac Mortgage-Related Securities","Agency",
                                  "Non-Agency","Mortgage Loans","Ending Balance")
# Page 2
portfolio_profile_2_1 <- data.frame(portfolio_profile_2_1, stringsAsFactors = FALSE)
names(portfolio_profile_2_1) <- c("","Month - Table 4","Issuances","Liquidations","Net Increase/ (Decrease)",
                                  "Ending Balance","Annualized Growth Rate","Annualized Liquidation Rate",
                                  "Month - Table 5","Ending Balance","Issuances","Maturities and Redemptions",
                                  "Repurchases","Ending Balance","Total Debt Outstanding")
portfolio_profile_2_2 <- data.frame(portfolio_profile_2_2, stringsAsFactors = FALSE)
names(portfolio_profile_2_2) <- c("","Month - Table 6","Non-Credit Enhanced","Primary Mortgage Insurance",
                                  "Other","Total-Single-Family","Total-Multifamily",
                                  "Month - Table 7","Ending Balance",
                                  "Month - Table 8","Monthly Average: PVS-L (50 bp)","Quarterly Average: PVS-L (50 bp)",
                                  "Monthly Average: PVS-YC (25 bp)","Quarterly Average: PVS-YC (25 bp)",
                                  "Monthly Average: Duration Gap","Quarterly Average: Duration Gap")
# Page 1 and Page 2 combined
portfolio_profile <- cbind(portfolio_profile_1_1,portfolio_profile_1_2,portfolio_profile_2_1,portfolio_profile_2_2)
# ^^^https://crimebythenumbers.com/scrape-table.html#making-a-function^^^

# ^^^User input and function: http://www.rexamples.com/4/Reading%20user%20input^^^

portfolio_profile <- data.frame(lapply(portfolio_profile, function(x) {
  gsub("[$%,)]", "", as.character(x),)
}))

portfolio_profile <- data.frame(lapply(portfolio_profile, function(x) {
  gsub("[(]", "-", as.character(x),)
}))

l <- 12 - m + 2

portfolio_profile <- portfolio_profile[-c(1:l),]
portfolio_profile <- head(portfolio_profile,m)
link <- save.csv
write.csv(portfolio_profile, link, row.names = F)
# }
# print(readinputs())

freddie <- read.csv("C:/Users/Alex Pondok/Desktop/CMD Research and Developemt/CMD_CSV/freddie.csv")
FHLMCrecent <- read_excel("C:/Users/Alex Pondok/Desktop/CMD Research and Developemt/CMD_Excel/SCBsupplement.xlsx",
                          sheet = "FHLMC monthly summary", skip = 2)

freddie1 <- FHLMCrecent[,c(2:18,20:26)]
freddie$freddie18 <- freddie[,19] + freddie[,18]
freddie2 <- tail(freddie[,c(2:8,10:15,17,53,18:19,21,24:29)],1)
colnames(freddie1) <- colnames(freddie2) <- c("fhlmc_port_pur","fhlmc_port_sales","fhlmc_port_liq","fhlmc_port_netch","fhlmc_port_endbal",
                                              "fhlmc_port_ann_gr","fhlmc_port_ann_liq","fhlmc_mrip_pur","fhlmc_mrip_sales","fhlmc_mrip_liq",
                                              "fhlmc_mrip_endbal","fhlmc_mrip_ann_gr","fhlmc_mrip_ann_liq","fhlmc_mripc_fhlmc","fhlmc_mripc_xfhlmc","fhlmc_mripc_agency","fhlmc_mripc_xagency","fhlmc_mripc_mort_loans",
                                              "fhlmc_mrs_iss","fhlmc_mrs_liq","fhlmc_mrs_netch","fhlmc_mrs_endbal","fhlmc_mrs_ann_gr","fhlmc_mrs_ann_liq")

FHLMCrecent <- as.xts(ts(rbind(freddie1,freddie2),start = c(1971,1),frequency = 12))

FHLMCrecent_dat <- data.frame(Date=index(FHLMCrecent),FHLMCrecent)

write.csv("C:/Users/Alex Pondok/Desktop/CMD Research and Developemt/CMD_CSV/freddie2.csv",x = FHLMCrecent_dat)
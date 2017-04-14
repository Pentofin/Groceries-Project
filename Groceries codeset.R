library(formattable)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

## Read in csv dataset
Groc <- read.csv("/Users/pbibra/Data Science/Groceries Project/Groceries.csv")
colnames(Groc) <- c("Date", "Frozen Food - Name", "Frozen Food - Price", "Produce - Name", "Produce - Type",
                    "Produce - Price", "Deli & Meat - Name", "Deli & Meat - Type", 
                    "Deli & Meat - Price", "Shelf Product - Name","Shelf Product - Type", 
                    "Shelf Product - Price", "Seafood - Name", "Seafood - Type",
                    "Seafood - Price", "Dairy - Name", "Dairy - Type", "Dairy - Price")

## Create pie chart displaying proportion of total money spent on groceries, grouped by product category.
TotFrozPrice <- summarise(Groc, s = sum(Groc$`Frozen Food - Price`, na.rm = TRUE))
TotProdPrice <- summarise(Groc, s = sum(Groc$`Produce - Price`, na.rm = TRUE))
TotDeliMPrice <- summarise(Groc, s = sum(Groc$`Deli & Meat - Price`, na.rm = TRUE))
TotShelfPrice <- summarise(Groc, s = sum(Groc$`Shelf Product - Price`, na.rm = TRUE))
TotSeaPrice <- summarise(Groc, s = sum(Groc$`Seafood - Price`, na.rm = TRUE))
TotDairyPrice <- summarise(Groc, s = sum(Groc$`Dairy - Price`, na.rm = TRUE))
Product_Category <- t(data.frame(a = "Frozen", b = "Produce", c = "Deli & Meat", 
                                 d = "Shelf", e = "Seafood", f = "Dairy"))
Total_Cost <- t(data.frame(a = TotFrozPrice, b = TotProdPrice, c = TotDeliMPrice, 
                           d = TotShelfPrice, e = TotSeaPrice, f = TotDairyPrice))
Ndf <- cbind(Product_Category, Total_Cost)
colnames(Ndf) <- c("Product_Category", "Total_Price")
rownames(Ndf) <- NULL
slices <- as.numeric(Ndf[,2])
lbls <- Ndf[,1]
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(as.numeric(Ndf[,2]), labels = lbls, 
    col=c("darkslategray1", "springgreen3", "tomato1", "darkkhaki", "aquamarine", "lightyellow"), 
    main="Proportion of Product Type to Total Amount Spent")

## Actual amounts spent on each product category.
Sums <- c()
Names <- c()
Prices <- select(Groc, contains("Price"))
for(i in 1:length(Prices))
{
  Sums <- c(Sums, sum(Prices[,i], na.rm = TRUE))
  Names <- c(Names,colnames(Prices)[i])
}
Sums <- data.frame(Sums)
Names <- data.frame(Names)
ProdCatSums <- cbind(Names, Sums)
knitr::kable(ProdCatSums)

## Total Price Changes Over Time
GrocRowSums <- rowSums(Prices, na.rm = TRUE)
# Calculating total amount spent over unique dates in dataset
Dateold <- Groc["Date"]
Dates <- c()
for(i in 1:length(Groc[,1])) 
{
  Dates <- c(Dates, toString(Dateold[i,1]))
}
DateRowSums <- cbind(Dates, GrocRowSums)
DateSums <- aggregate(as.numeric(as.character(DateRowSums[,2])), 
                      by = list(DateRowSums[,1]), FUN = sum)
colnames(DateSums) <- c("Date", "Total Spent ($)")
# Line graph of total prices over time
ggplot(data=DateSums, aes(x=Date, y=`Total Spent ($)`, group=1)) +
  geom_line() + theme(plot.title = element_text (face = "bold")) +
  geom_point(col = "blue") + ggtitle("Cost of Groceries by Date") + scale_x_discrete("Date", labels = c("Jan 10","Jan 12","Jan 15","Jan 18","Jan 23","Jan 30","Jan 31","Feb 5", "Feb 13","Feb 21","Feb 26")) + geom_label(label = DateSums$`Total Spent`, col = "Blue")

## Average Grocery Bill
options(digits = 2)
DateSumsN <- DateSums[which(DateSums$`Total Spent` > 4.69 ),]
Mean <- mean(DateSumsN$`Total Spent`)

## Time Period Between Grocery Bills
DATA <- distinct(data.frame(Groc$Date))
#Process of converting dataset date values into recognizable format by R
date <- as.Date(DATA$Groc.Date, format="%m/%d/%Y")
ndate <- date[c(-2,-7)]
diffs <- c()
for(i in 1:length(ndate))
{
  diffs <- c(diffs, ndate[i+1] - ndate[i])
}
m <- mean(diffs, na.rm = TRUE)

## Break down all categories even further through bar plots

## Produce bar plot
Pr <- data.frame(table(Groc$`Produce - Name`))
Pr <- Pr[-1,] 
nPr1 <- cbind(data.frame(c(1:24)), Pr$Var1)
nPr2 <- transform(nPr1, newcol=paste(Pr$Var1,c.1.24., sep="-"))
## colourCount and getPalette are useful user defined fns to extend palette across all bars in plot
colourCount <- length(unique(Pr[,1]))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
p <-ggplot(Pr,aes(Pr[,1], Pr[,2], fill = factor(nPr2[,3])))
p +geom_bar(stat = "identity", width = 0.7) + geom_text(aes(label = c(1:24)), position=position_dodge(width=0.7), vjust=-0.27) + theme_dark() + theme(plot.title = element_text(face = "bold"),axis.title.x=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                      axis.ticks.x=element_blank()) + ylab("Number of purchases") + 
  labs(title = "Count of Produce Product Purchases", fill = "Produce Products") + scale_y_continuous(breaks=seq(0, 10, 1)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Greens"))(colourCount))

## Wide variety of Produce products
## Easier to derive results through breakdown of Produce Product Type
ProdPT <- table(Groc[,5])
ProdPT_counts <- ProdPT[-1]
lbls <- c("Bulb", "Flower", "Fruit", "Fungi", "Leaves", "Root", "Stem")
pct <- round(ProdPT_counts/sum(ProdPT_counts)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(ProdPT_counts,labels = lbls, col=c("darkorchid", "lightpink1", "magenta", "wheat", "springgreen4", "tan3", "green3"), main="Proportion of Produce Product Type", radius = 1, cex = 0.9)

## Shelf bar plot
Shelfo <- data.frame(table(Groc$`Shelf Product - Name`))[-1,]
## Many shelf products were only purchased once
## Visually more appealing to represent these purchases with one bar with a purchase count of 1
Shelf <- rbind(head(Shelfo[Shelfo$Freq == 1,], n = 1), Shelfo[Shelfo$Freq != 1,])
Shelfnames <- c("SEE LIST BELOW",as.vector(Shelfo[Shelfo$Freq != 1,]$Var1))
Shelfothernames <- Shelfnames[Shelfnames != ""]
colourCount <- length(unique(Shelf[,1]))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
s <-ggplot(Shelf,aes(Shelf[,1], Shelf[,2], fill = factor(Shelf[,1])))
s +geom_bar(stat = "identity", width = 0.7) + theme_dark() + theme(plot.title = element_text(face = "bold"), axis.title.x=element_blank(),axis.text.x=element_blank(),
                                                                   axis.ticks.x=element_blank()) + ylab("Number of purchases") + 
  labs(title = "Count of Shelf Product Purchases", fill = "Shelf Products") +  scale_y_continuous(breaks=seq(0, 10, 1)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(4, "YlOrBr"))(colourCount), labels = paste(Shelfothernames))
# Shelf products purchased once
SP1o <- Shelfo[Shelfo$Freq == 1,]
SP1 <- as.data.frame(SP1o[,1])
colnames(SP1) <- "Shelf Products purchased once"

## Similar to Produce, Shelf products broken down by Product Type and reprsented as pie chart
ShelfPT <- table(Groc$`Shelf Product - Type`)
ShelfPT_counts <- ShelfPT[-1]
lbls <- c("Bread", "Condiments", "Juice", "Marinade", "Oil", "Pasta Sauce", "Salad Dressing",
          "Seasoning", "Snacks", "Soup", "Spreads")
pct <- round(ShelfPT_counts/sum(ShelfPT_counts)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(ShelfPT_counts,labels =lbls, col=c("peru", "orange", "palevioletred", "khaki2", "wheat", "orangered", "yellow2", "yellowgreen", "lightyellow", "lightsalmon", "lightpink"), main="Proportion of Shelf Product Type", radius = 0.9999, cex = 0.77)

## Deli & Meat bar plot
DeliM <- data.frame(table(Groc$`Deli & Meat - Name`))
DeliM <- DeliM[-1,] 
colourCount <- length(unique(DeliM[,1]))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
d <-ggplot(DeliM,aes(DeliM[,1], DeliM[,2], fill = factor(DeliM[,1])))
d +geom_bar(stat = "identity", width = 0.7) + theme_dark() + theme(plot.title = element_text(face = "bold"),axis.title.x=element_blank(),axis.text.x=element_blank(),
                                                                   axis.ticks.x=element_blank()) + ylab("Number of purchases") + 
  labs(title = "Count of Deli & Meat Product Purchases", fill = "Deli & Meat Products") + scale_y_continuous(breaks=seq(0, 10, 1)) + scale_fill_brewer(palette = "Reds")

## Seafood bar plot
Seaf <- data.frame(table(Groc$`Seafood - Name`))
Seaf <- Seaf[-1,] 
sf <-ggplot(Seaf,aes(Seaf[,1], Seaf[,2], fill = factor(Seaf[,1])))
sf +geom_bar(stat = "identity", width = 0.3) + theme_dark() + theme(plot.title = element_text(face = "bold"),axis.title.x=element_blank(),axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + ylab("Number of purchases") + 
  labs(title = "Count of Seafood Product Purchases", fill = "Seafood Products") +  scale_y_continuous(breaks=seq(0, 10, 1)) + scale_fill_brewer(palette = "Blues")

## Dairy bar plot
Dry <- data.frame(table(Groc$`Dairy - Name`))
Dry <- Dry[-1,] 
da <-ggplot(Dry,aes(Dry[,1], Dry[,2], fill = factor(Dry[,1])))
da +geom_bar(stat = "identity", width = 0.3) + theme_dark() +  theme(plot.title = element_text(face = "bold"),axis.title.x=element_blank(),axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank()) + ylab("Number of purchases") + 
  labs(title = "Count of Dairy Product Purchases", fill = "Dairy Products") +  scale_y_continuous(breaks=seq(0, 10, 1)) + scale_fill_brewer(palette = "Oranges")

## Break down chicken breast (only) by price and corresponding dates when purchased. 
DeliMP <- data.frame(table(Groc$`Deli & Meat - Name`, Groc$`Deli & Meat - Price`, Dates))
CB <- filter(DeliMP, DeliMP$Var1 == "Chicken Breast", Freq != 0)
colnames(CB) <- c("Product", "Price", "Date", "Freq")
arrange(CB, Product, Date)

## Need a new variable quantity to match chicken package counts
## Was recorded separately from csv file
Quantity <- t(data.frame(a = 2, b = 5, c = 4, d = 2, e = 2, f = 4, g = 3, h = 3, i = 4))
CBn <- cbind(CB, Quantity)
knitr::kable(CBn[,1:4])

## Average price of chicken breast in comparison to other poultry products 
DeliMN <- filter(DeliMP, Freq != 0)
colnames(DeliMN) <- c("Product", "Price", "Date", "Freq")
arrange(DeliMN, Product, Date)
options (digits = 2)
DeliMMeans <- aggregate(as.numeric(as.character(DeliMN$Price)), list(DeliMN$Product), mean)
colnames(DeliMMeans) <- c("Deli & Meat Product", "Average Price")
ChickenMeansdraft <- filter(DeliMMeans, DeliMMeans$`Deli & Meat Product`%in% c("BBQ Chicken", "Chicken Thighs", "Ground Chicken","Sliced chicken breast"))
ChickenMeansdraft$`Deli & Meat Product` <- sub("Chicken Thighs", "Chicken Thighs (5)", ChickenMeansdraft$`Deli & Meat Product`)
colnames(ChickenMeansdraft) <- c("Deli & Meat Product", "Average Price")

## Find chicken breast averages seperately due to varying quantities
CBMeans <- aggregate(as.numeric(as.character(CBn[,2])), list(CBn$Quantity), mean)
CBMeansn <- cbind(t(data.frame(a = "Chicken Breast (2)", b = "Chicken Breast (3)", c = "Chicken Breast (4)", d = "Chicken Breast (5)")),as.data.frame(sprintf("%.2f", CBMeans[,2])))
colnames(CBMeansn) <- c("Deli & Meat Product", "Average Price")
ChickenMeans <- rbind(ChickenMeansdraft,CBMeansn)
colnames(ChickenMeans) <- c("Deli & Meat Product", "Average Price")
rownames(ChickenMeans) <- NULL
knitr::kable(ChickenMeans)

## Track chicken breast prices over time to determine specials
## Create "yes/no" barplots
# Day of the Week and Special column added to chicken breast table
DOW <- t(data.frame(a = "Sun", b = "Wed", c = "Mon", d = "Mon", e = "Mon", f = "Sun", g = "Mon", h = "Mon", i = "Sun"))
Special <- t(data.frame(a = "Yes", b = "Yes", c = "No", d = "No", e = "No", f = "Yes", g = "No", h = "No", i = "Yes"))
CBndowSpec <- cbind(CBn, DOW, Special)
CBndowS <- CBndowSpec[CBndowSpec$DOW == "Sun",]
CBndowM <- CBndowSpec[CBndowSpec$DOW == "Mon",]
CBndowW <- CBndowSpec[CBndowSpec$DOW == "Wed",]
# par and mfrow useful for plotting multiple graphs in a single sapce (e.g. side-by-side)
par(mfrow = c(2,2))
barplot(table(CBndowS$Special), main="Occurences of Specials on Sundays", col = c("red", "dark green"), axes = FALSE)
axis(side = 2,at = c(0:5))
barplot(table(CBndowM$Special), main="Occurences of Specials on Mondays", col = c("red", "dark green"), axes = FALSE)
axis(side = 2,at = c(0:5))
barplot(table(CBndowW$Special), main="Occurences of Specials on Wednesdays", col = c("red", "dark green"), axes = FALSE)
axis(side = 2,at = c(0:5))

## Similar analysis with chicken breast also done with bell peppers
# Break down bell peppers by price
PrP <- data.frame(table(Groc$`Produce - Name`, Groc$`Produce - Price`, Dates))
colnames(PrP) <- c("Product", "Price", "Date", "Freq")
BP <- filter(PrP, PrP$Product %in% c("Bell Peppers", "Bell Peppers (Green)"), Freq != 0)
arrange(BP, Product, Date)
# Day of the Week and Special column added to bell peppers table
Quantity <- t(data.frame(a = 3, b = 1, c = 1, d = 2, e = 3, f = 2, g = 2, h = 3))
DOW <- t(data.frame(a = "Thurs", b = "Sun", c = "Mon", d = "Mon", e = "Sun", f = "Mon", g = "Tues", h = "Sun"))
Special <- t(data.frame(a = "No", b = "No", c = "No", d = "No", e = "Yes", f = "No", g = "No", h = "No"))
BPdowSpec <- cbind(BP,Quantity, DOW, Special)
rownames(BPdowSpec) <- NULL
knitr::kable(BPdowSpec)
## Track days with specials for bell peppers
BPdowS <- BPdowSpec[BPdowSpec$DOW == "Sun",]
BPdowM <- BPdowSpec[BPdowSpec$DOW == "Mon",]
BPdowT <- BPdowSpec[BPdowSpec$DOW == "Tues",]
BPdowTh <- BPdowSpec[BPdowSpec$DOW == "Thurs",]
# Four graphs instead of three (as done with chicken breast analysis) since
# bell peppers purchased on four different days of the week
par(mfrow = c(2,2))
barplot(table(BPdowS$Special), main="Occurences of Specials on Sundays", col = c("red", "dark green"), axes = FALSE)
axis(side = 2,at = c(0:5))
barplot(table(BPdowM$Special), main="Occurences of Specials on Mondays", col = c("red", "dark green"), axes = FALSE)
axis(side = 2,at = c(0:5))
barplot(table(BPdowT$Special), main="Occurences of Specials on Tuesdays", col = c("red", "dark green"), axes = FALSE)
axis(side = 2,at = c(0:5))
barplot(table(BPdowTh$Special), main="Occurences of Specials on Thursdays", col = c("red", "dark green"), axes = FALSE)
axis(side = 2,at = c(0:5))
## Average price of a Bell Pepper (Coloured vs Green)
BPMeansold <- aggregate(as.numeric(as.character(BP$Price)), list(BP$Product), mean)
BPMeans <- cbind(BPMeansold$Group.1, as.data.frame((sprintf("%.2f",BPMeansold[,2]))))
colnames(BPMeans) <- c("Product", "Average Price")
knitr::kable(BPMeans, align = 'c')

## General data on Dairy products and total amount spent
DairyP <- data.frame(table(Groc$`Dairy - Name`, Groc$`Dairy - Price`, Dates))
DairyN <- filter(DairyP, Freq != 0)
colnames(DairyN) <- c("Product", "Price", "Date", "Quantity")
arrange(DairyN, Product, Date)
DairyMeans <- aggregate(as.numeric(as.character(DairyN$Price)), list(DairyN$Product), mean)
colnames(DairyMeans) <- c("Dairy Product", "Average Price")
options(digits = 2)
DairySums <- aggregate(as.numeric(as.character(DairyN$Price)), by = list(DairyN$Product), FUN = sum)
colnames(DairySums) <- c("Dairy Product", "Total Amount Spent on Product")

## Comparison of Lactantia vs Sealtest prices (when on sale vs not on sale)
Milk <- filter(DairyN, DairyN$Product == "2% Milk")
Brand <- t(data.frame(a = "Lactantia", b = "Other", c = "Lactantia", d = "Other", e = "Lactantia"))
DOW <- t(data.frame(a = "Sun", b = "Mon", c = "Tues", d = "Mon", e = "Sun"))
Special <- t(data.frame(a = "Yes", b = "No", c = "Yes", d = "Yes", e = "No"))
Milk2 <- cbind(Milk, Brand, DOW, Special)

## General data on Shelf products and total amount spent
ShelfP <- data.frame(table(Groc$`Shelf Product - Name`, Groc$`Shelf Product - Price`, Dates))
ShelfN <- filter(ShelfP, Freq != 0)
colnames(ShelfN) <- c("Product", "Price", "Date", "Quantity")
arrange(ShelfN, Product, Date)
ShelfMeans <- aggregate(as.numeric(as.character(ShelfN$Price)), list(ShelfN$Product), mean)
colnames(ShelfMeans) <- c("Shelf Product", "Average Price")
options(digits = 2)
ShelfSums <- aggregate(as.numeric(as.character(ShelfN$Price)), by = list(ShelfN$Product), FUN = sum)
colnames(ShelfSums) <- c("Shelf Product", "Total Amount Spent on Product")
knitr::kable(ShelfSums, align = 'c')

## General data on Seafood products and total amount spent
SeaFP <- data.frame(table(Groc$`Seafood - Name`, Groc$`Seafood - Price`, Dates))
SeaFN <- filter(SeaFP, Freq != 0)
colnames(SeaFN) <- c("Product", "Price", "Date", "Quantity")
arrange(SeaFN, Product, Date)
SeaFNMeans <- aggregate(as.numeric(as.character(SeaFN$Price)), list(SeaFN$Product), mean)
colnames(SeaFNMeans) <- c("Seafood Product", "Average Price")
SeaFNSumsold <- aggregate(as.numeric(as.character(SeaFN$Price)), by = list(SeaFN$Product), FUN = sum)
SeaFNSums <- cbind(SeaFNSumsold[,1], as.data.frame(sprintf("%.2f", SeaFNSumsold[,2])))
colnames(SeaFNSums) <- c("Seafood Product", "Total Amount Spent on Product")
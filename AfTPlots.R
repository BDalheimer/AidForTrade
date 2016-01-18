setwd("/home/bernhard/Dropbox/University of Goettingen/Semester III/Seminar Latin America/Plots")

library(ggplot2)
library(reshape2)
library(treemap)
library(grid)
library(data.table)
# Net Disbursments stacked bar plot

ODA = read.csv("ODA_totalNetDisbursments.csv", stringsAsFactors = F)

ODA$Donor = c("All Donors, Total", "DAC Countries, Total",  "Non-DAC Countries, Total", "Multilateral Agencies, Total")
rownames(ODA) = 1:4

ODA_long = melt(ODA, id.var = "Donor", value.name = "Aid", variable.name = "Year")
ODA_long$Year = gsub("X", "", ODA_long$Year)
ODA_long = ODA_long[!ODA_long$Donor == "All Donors, Total",]
ggplot(data = ODA_long, aes(y = Aid, x = Year, fill=Donor)) + 
  geom_bar(stat = "identity") +
  ylab("Aid in current million USD") + 
  theme(legend.position = "bottom", legend.direction = "horizontal")
  

## treemap plane 

tileData = read.csv("Sector_data.csv")

tileData$Sub.CategoryShare = paste(tileData$Sub.Category,
                                   paste("(", round(tileData$amounts / sum(tileData$amounts), digits = 4) * 100, "%", ")",  sep = ""), 
                                  sep = " ")

CategoryShare = ddply(tileData, .(Category), function(x) {sum(x$amounts)})
CategoryShare$CategoryShare = round(CategoryShares$V1 / sum(CategoryShares$V1), digits = 4) * 100
tileData = merge(tileData, CategoryShare, by="Category")
tileData$CategoryShare = paste(tileData$Category, 
                                paste("(", tileData$CategoryShare, ")", "%", sep = ""), sep= " ")

treemap(tileData,
        index=c("CategoryShare", "Sub.CategoryShare"),
        vSize="amounts",
        vColor="Sub.CategoryShare",
        #palette.HCL.options=palette.HCL.options,
        palette = "Set1",
        type="index",
        sortID = "Category",
        title = "",
        lowerbound.cex.labels=1,
        #fontfamily.labels = "Times",
        fontsize.labels = c(14, 11),
        aspRatio = 16/9,
        border.lwds=0,
        align.labels=list(c("center", "center"), c("center", "top")))


# region and income group bar charts

regionalData = read.csv("Region_data.csv")
incomeData = read.csv("Income_group_data.csv")

regionalPlot = ggplot(data=regionalData, aes(x=Continent, y=amounts, fill=Continent)) +
                 geom_bar(stat="identity") +
                 guides(fill=FALSE) +
                 ylab("Mio USD") + xlab("") +
                 ggtitle("Regional Distribution of AfT") + 
                 theme(plot.title = element_text(lineheight=4, face="bold"))

incomePlot = ggplot(data=incomeData, aes(x=Income.Group, y=amounts, fill=Income.Group.labels)) +
                 geom_bar(stat="identity") +
                 guides(fill = F)+
                 ylab("Mio USD") + xlab("") +
                 ggtitle("AfT Distribution by Income Group") + 
                 theme(plot.title = element_text(lineheight=4, face="bold")) +
                 theme(legend.position = "bottom", legend.direction = "horizontal") 
                 #theme(legend.text=element_text(size=1))

multiplot(regionalPlot, incomePlot, cols=2)

grid.arrange(regionalPlot, incomePlot, ncol = 2, main = "Main title")
plot_grid(regionalPlot, incomePlot, labels=c("A", "B"), ncol = 2, nrow = 1)


# combined regional and income distribution plots


fullData = as.data.table(read.csv("OECD_full_data.csv"))
fullDataAggregate = as.data.table(aggregate(data = fullData, 
                                                 amounts ~ Continent + Income.Group, 
                                                 sum))
fullDataAggregate[, test := 1:length(fullDataAggregate[,Continent])]
ggplot(fullDataAggregate, aes(Continent, Income.Group)) + 
geom_point(aes(size = amounts, colour = Continent)) + scale_size_area(max_size = 18) +
  guides(colour=FALSE)+
  ylab("Income Group") +
  xlab("Continent")+
  labs(size="Mio USD")+
  ggtitle("AfT Distribution by Income Group and Continent") + 
  theme(plot.title = element_text(lineheight=4, face="bold")) 


# Stacked barplot Region - Income AfT
ggplot(fullDataAggregate, aes(x = Continent, y = amounts, fill=Income.Group.)) + 
  geom_bar(stat = "identity") +
  #theme_minimal() + 
  ylab("Mio USD") + xlab("") +
  ggtitle("AfT by Region and Income Group") + 
  theme(plot.title = element_text(lineheight=4, face="bold")) +
  geom_text(aes(label=len), vjust=1.6, color="white", size=3.5) +
  labs(fill = "Income Group")

# Top Donor and Recipient countries

donors = as.data.table(read.csv("Top_10_donor_data.csv"))
recipients = as.data.table(read.csv("Top_10_recipient_data.csv"))

setnames(donors, c("amounts..copy.", "Donor.Name"), c("type", "country"))
donors[, type := rep("donor", length(donors[, type]))]

setnames(recipients, c("amounts..copy.", "Recipient"),  c("type", "country"))
recipients[, type := rep("recipient", length(recipients[, type]))]

donorsRecipients = rbind(donors, recipients)

ggplot(donorsRecipients, aes(x=country, y = amounts)) + 
  geom_bar(stat = "identity") +  
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8))) +
  facet_wrap(~type)

donorPlot = ggplot(donors, aes(x=amounts, y=reorder(factor(country),amounts)))+
        geom_segment(aes(yend=country,xend=0), size=6, color = 'dodgerblue4') +
        labs(x="Mio USD", y="",title="Top 10 Donors")  +
        scale_x_continuous(expand = c(0, 0)) +
        theme(plot.title = element_text(lineheight=4, face="bold"))

recipientPlot = ggplot(recipients, aes(x=amounts, y=reorder(factor(country),amounts)))+
  geom_segment(aes(yend=country,xend=0), size=6, color = 'red4') +
  labs(x="Mio USD", y="",title="Top 10 Recipients")  +
  scale_x_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(lineheight=4, face="bold"))  


grid.arrange(donorPlot, recipientPlot, ncol=2,widths=c(1,1))

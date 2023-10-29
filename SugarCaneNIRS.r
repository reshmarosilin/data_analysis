Data = read.table("crx.data",na.strings = "?", sep=”,”, header = FALSE)
names(Data) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", "Occupation", "BankingInstitution", "YearsEmployed", "NoPriorDefault", "Employed", "CreditScore", "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved")
Data$Gender <- as.factor(Data$Gender) Data$Age <- as.numeric(Data$Age)
Data$MonthlyExpenses <- as.integer(Data$MonthlyExpenses) Data$MaritalStatus <- as.factor(Data$MaritalStatus) Data$HomeStatus <- as.factor(Data$HomeStatus) Data$Occupation <- as.factor(Data$Occupation) Data$BankingInstitution <- as.factor(Data$BankingInstitution) Data$YearsEmployed <- as.numeric(Data$YearsEmployed) Data$NoPriorDefault <- as.factor(Data$NoPriorDefault) Data$Employed <- as.factor(Data$Employed) Data$CreditScore <- as.numeric(Data$CreditScore) Data$DriversLicense <- as.factor(Data$DriversLicense)
Data$AccountType <- as.factor(Data$AccountType) Data$MonthlyIncome <- as.integer(Data$MonthlyIncome) Data$AccountBalance <- as.numeric(Data$AccountBalance) Data$Approved <- as.factor(Data$Approved)
Data <- na.omit(Data)
library(cluster)
dist = daisy(Data, metric = "gower")
Dist <- as.matrix(Dist)
dim <- ncol(Dist) # used to define axis in image
image(1:dim, 1:dim, Dist, axes = FALSE, xlab="", ylab="", col = rainbow(100))
heatmap(Dist, Rowv=TRUE, Colv="Rowv", symm = TRUE)
num<-Data [,c(2,3,8,11,14,15)]
str(v)
P <-cor( num ,method = "Pearson")
S <-cor( num ,method = "Spearman")

plot_1 <- ggplot(data = Data,mapping = aes(x = Approved, y = AccountBalance))+geom_boxplot()
plot_1
plot_2 <- ggplot(data = Data,mapping = aes(x = Approved, y = MonthlyExpenses))+geom_boxplot()
plot_2
plot_3 <- ggplot(data = Data,mapping = aes(x = Approved, y = CreditScore))+geom_boxplot()
plot_3
plot_4 <- ggplot(data = Data,mapping = aes(x = Approved, y = Age))+geom_boxplot()
plot_4

plott_1<-ggplot(data=Data)+geom_bar(mapping = aes(x=Employed,fill=Approved))
plott_1
plott_2 <- ggplot(data=Data)+geom_bar(mapping = aes(x=MaritalStatus,fill=Approved))
plott_2
plott_3 <- ggplot(data=Data)+geom_bar(mapping = aes(x=BankingInstitution,fill=Approved))
plott_3
plott_4 <- ggplot(data=Data)+geom_bar(mapping = aes(x=NoPriorDefault,fill=Approved))
plott_4

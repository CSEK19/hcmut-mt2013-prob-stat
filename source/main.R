library(stats)
library(dplyr)
library(magrittr)
library(ggplot2)

raw_data = read.csv('data.csv')
View(raw_data)

data = raw_data %>% select('Manufacturer','Name','Architecture','Boost_Clock','Core_Speed','Max_Power','Memory','Memory_Bus', 'Memory_Speed', 'Release_Year', 'Release_Price','Shader','TMUs');
View(data)

df = data %>% filter(!( is.na(Manufacturer) | is.na(Name) | is.na(Architecture) | is.na(Boost_Clock) | is.na(Core_Speed) | is.na(Max_Power) |
                         is.na(Memory) | is.na(Memory_Bus) | is.na(Memory_Speed) | is.na(Release_Year) | is.na(Release_Price) | is.na(Shader) | is.na(TMUs)))
View(df)

df['log.Boost_Clock'] <- log(df['Boost_Clock'])
df['log.Core_Speed'] <- log(df['Core_Speed'])
df['log.Max_Power'] <- log(df['Max_Power'])
df['log.Memory'] <- log(df['Memory'])
df['log.Memory_Bus'] <- log(df['Memory_Bus'])
df['log.Memory_Speed'] <- log(df['Memory_Speed'])
df['log.Release_Year'] <- log(df['Release_Year'])
df['log.Release_Price'] <- log(df['Release_Price'])
df['log.Shader'] <- log(df['Shader'])
df['log.TMUs'] <- log(df['TMUs'])

table = data.frame(
  Name = c('mean', 'median', 'sd', 'min', 'max'),
  log.Boost_Clock = c(mean(df[, 'log.Boost_Clock']), median(df[, 'log.Boost_Clock']), sd(df[, 'log.Boost_Clock']), min(df[, 'log.Boost_Clock']), max(df[, 'log.Boost_Clock'])),
  log.Core_Speed = c(mean(df[, 'log.Core_Speed']), median(df[, 'log.Core_Speed']), sd(df[, 'log.Core_Speed']), min(df[, 'log.Core_Speed']), max(df[, 'log.Core_Speed'])),
  log.Max_Power = c(mean(df[, 'log.Max_Power']), median(df[, 'log.Max_Power']), sd(df[, 'log.Max_Power']), min(df[, 'log.Max_Power']), max(df[, 'log.Max_Power'])),
  log.Memory = c(mean(df[, 'log.Memory']), median(df[, 'log.Memory']), sd(df[, 'log.Memory']), min(df[, 'log.Memory']), max(df[, 'log.Memory'])),
  log.Memory_Bus = c(mean(df[, 'log.Memory_Bus']), median(df[, 'log.Memory_Bus']), sd(df[, 'log.Memory_Bus']), min(df[, 'log.Memory_Bus']), max(df[, 'log.Memory_Bus'])),
  log.Memory_Speed = c(mean(df[, 'log.Memory_Speed']), median(df[, 'log.Memory_Speed']), sd(df[, 'log.Memory_Speed']), min(df[, 'log.Memory_Speed']), max(df[, 'log.Memory_Speed'])),
  log.Release_Year = c(mean(df[, 'log.Release_Year']), median(df[, 'log.Release_Year']), sd(df[, 'log.Release_Year']), min(df[, 'log.Release_Year']), max(df[, 'log.Release_Year'])),
  log.Release_Price = c(mean(df[, 'log.Release_Price']), median(df[, 'log.Release_Price']), sd(df[, 'log.Release_Price']), min(df[, 'log.Release_Price']), max(df[, 'log.Release_Price'])),
  log.Shader = c(mean(df[, 'log.Shader']), median(df[, 'log.Shader']), sd(df[, 'log.Shader']), min(df[, 'log.Shader']), max(df[, 'log.Shader'])),
  log.TMUs = c(mean(df[, 'log.TMUs']), median(df[, 'log.TMUs']), sd(df[, 'log.TMUs']), min(df[, 'log.TMUs']), max(df[, 'log.TMUs']))
)


hist(df$log.Release_Price, main="Distribution of Release_Price", xlab = "US Dollar", ylab = "Number of GPUs", col = rainbow(9))
hist(df$Release_Year, main="Distribution of Release_Year", xlab = "Year", ylab = "Number of GPUs", col = rainbow(9))



lmPrice = lm(log.Release_Price ~ log.Boost_Clock + log.Core_Speed + log.Max_Power + log.Memory + log.Memory_Bus + log.Memory_Speed + log.Shader + log.TMUs, df)
summary(lmPrice)

lmPriceNoMem = lm(log.Release_Price ~ log.Boost_Clock + log.Core_Speed + log.Max_Power + log.Memory_Bus + log.Memory_Speed + log.Shader + log.TMUs, df)
summary(lmPriceNoMem)

lmPrice_Mem = lm(log.Release_Price ~ log.Memory, df[,c('log.Release_Price','log.Memory')])
anova(lmPrice_Mem)

plot(lmPrice$fitted.values, lmPrice$residuals, col = "blue", xlab = "fitted.values", ylab = "residuals")


Price.graph<-ggplot(lmPrice, aes(x=Release_Price, y=Max_Power))+geom_point()
Price.graph

Price.graph <- Price.graph + geom_smooth(method="lm", col="black")
Price.graph

ggplot(df) + geom_bar(aes(x = Release_Year), stat = "count", fill = rainbow(6)) + scale_x_continuous(breaks = pretty(df$Release_Year, n = 10)) + xlab("Year") + ylab("Number of GPUs")

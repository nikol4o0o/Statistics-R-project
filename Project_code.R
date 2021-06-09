library(readxl)
anketa_statistika_en <- read_excel("~/Downloads/anketa_statistika_en.xlsx")
colnames(anketa_statistika_en) <- paste("Q", 1:ncol(anketa_statistika_en), sep = "_")
str(anketa_statistika_en)
View(anketa_statistika_en) 


#1
table_age <- table(anketa_statistika_en$Q_2)
prop_table_age <- prop.table(table_age)
barplot(height = (prop_table_age)*100, col = "green",xlab = "Age", ylab = "Per cent", ylim = c(0,100), main = "What is your age?")

#2
table_gender <- table(anketa_statistika_en$Q_1)

prop_table_gender <- prop.table(table_gender)
barplot(height = prop_table_gender, col = rainbow(3), main = "What is your gender")

piepercent_gender <- round(100*table_gender/sum(table_gender), 1)
pie(table_gender, labels = piepercent_gender, main = " What is your gender", col =
      rainbow(n = length(table_gender)*2))
legend(x = "bottomright", legend = c("Men", "Women"),
       cex = 1.5, fill = rainbow(length(table_gender) *2))

#3 
 table_smartphone <- table(anketa_statistika_en$Q_3)
 factor(table_smartphone)

#4 
table_brands <- table(anketa_statistika_en$Q_4)
prop_table_brands <- prop.table(table_brands)

barplot(height =prop_table_brands*100, col = "darkgreen", main = "What is the brand of your phone?")

piepercent_brands <- round(100*table_brands/sum(table_brands), 1)
pie(table_brands, labels = piepercent_brands, main = " What is the brand of your phone?", col =
      rainbow(n = length(table_brands)))
legend(x = "bottomleft", legend = c("Alcatel","Apple", "Google Pixel", "HTC", "Huawei", "Nokia","OnePlus", "Samsung", "Xiaomi"),
       cex = 0.8, fill = rainbow(length(table_brands)))

#5
smartphone_size <- anketa_statistika_en$Q_8
modeFunction <- function(x) 
{
  res_table <- table(x)
  return(names(res_table)[res_table == max(res_table)])
}
modeFunction(smartphone_size)

summary(smartphone_size)

var(smartphone_size)

sd(smartphone_size)


hist(smartphone_size, main = "What is the size of your smartphone?", xlab = "Size inches", ylab = "Frequency", col = "cornsilk2", prob = T)
abline(v = mean(smartphone_size), lwd = 2, lty = 4, col = "red")
abline(v = median(smartphone_size), lwd = 2, lty = 3, col = "blue")

boxplot(smartphone_size, col = "bisque4", main = "What is the size of your smartphone?",
        xlab = "Size inches")

#6
answer_1 = "Battery"
answer_2 = "Camera"
answer_3 = "Speed"
answer_4 = "Brand"
answer_5 = "Memory"
answer_6 = "Durability"
answer_7 = "Price/quality"
answer_8 = "Operating system"
answer_9 = "Price"
answer_10 = "App strores"
answer_11 = "Problems"

feature_var = c(rep(answer_1, 75), rep(answer_2, 62), rep(answer_3, 68), rep(answer_4, 27), rep(answer_5, 54), rep(answer_6, 2),
                 rep(answer_7, 1), rep(answer_8, 1), rep(answer_9, 5), rep(answer_10, 1), rep(answer_11, 1))

feature_table = table(feature_var)


feature_table_prop <- prop.table(feature_table)

# Графично представяне
barplot(height = feature_table, col = rainbow(10), main = "Which is the main feature when you are purchasing a smartphone?")


piepercent_features <- round(100*feature_table/sum(feature_table), 1)
pie(feature_table, labels = piepercent_features, main = "Which is the main feature when you are purchasing a smartphone?", 
    col = rainbow(n = length(feature_table)))

legend(x = "bottomleft", legend = c("App stores","Battery","Brand","Camera","Durability",
                                    "Memory","Operating system","Price","Price/Quality","Problems", "Speed"), cex = 0.8,
       fill = rainbow(length(feature_table)))

#7
new_phone_interval <- anketa_statistika_en$Q_6
modeFunction <- function(x) {
  res_table <- table(x)
  return(names(res_table)[res_table == max(res_table)])
}
 modeFunction(new_phone_interval)

summary(new_phone_interval)

var(new_phone_interval)

sd(new_phone_interval)

hist(new_phone_interval, main = " The interval in which you are buying a new smartphone?",
     xlab = "Years", ylab = "Frequency", col = "burlywood4", prob = T)
abline(v = mean(new_phone_interval), lwd = 2, lty = 4, col = "red")
abline(v = median(new_phone_interval), lwd = 2, lty = 3, col = "blue")


boxplot(new_phone_interval, col = "burlywood4", main = "The interval in which you are buying a new smartphone?",
        xlab = "Years")

#8

customer_satisfication <- anketa_statistika_en$Q_7

modeFunction <- function(x) {
  res_table <- table(x)
  return(names(res_table)[res_table == max(res_table)])
}
modeFunction(customer_satisfication)

summary(customer_satisfication)

var(customer_satisfication)

sd(customer_satisfication)

hist(customer_satisfication, main = " From 0- 10 how much you are satisfied of your phone?",
     xlab = "Rate", ylab = "Frequency", col = "burlywood4", prob = T)
abline(v = mean(customer_satisfication), lwd = 2, lty = 4, col = "red")
abline(v = median(customer_satisfication), lwd = 2, lty = 3, col = "blue")


boxplot(customer_satisfication, col = "burlywood4", main = "From 0- 10 how much you are satisfied of your phone?",
        xlab = "")



# Числова VS числова
#Корелационен
rho <- round(cor(smartphone_size, customer_satisfication), 3) #коефициент на корелация
par(mfrow = c(1, 1))
plot(smartphone_size, customer_satisfication, main = "Correlation analysis")
cor(smartphone_size, customer_satisfication)
#Линейна регресия
DF <- data.frame(new_phone_interval, customer_satisfication)

model1 <- lm(customer_satisfication ~ new_phone_interval)
summary(model1)

resid(lm(new_phone_interval ~ customer_satisfication))
par(mfrow = c(1, 1))
plot(lm(new_phone_interval ~ customer_satisfication))
#Числова vs категорийна
brands <-anketa_statistika_en$Q_4
new_phone_interval_vs_table_brands <-boxplot(new_phone_interval~brands)

#Числова vs категорийна
ages <- anketa_statistika_en$Q_2
DF1 <-data.frame(new_phone_interval, ages)

new_phone_interval_vs_ages <-boxplot(new_phone_interval~ages)

aggregate(new_phone_interval ~ ages, data = DF1, FUN = function(x) {shapiro.test(x)$p.value})
bartlett.test(new_phone_interval ~ ages, data = DF1)

kruskal.test(new_phone_interval ~ ages, data = DF1)

#Категорийна vs категорийна
ages <- anketa_statistika_en$Q_2
brands <-anketa_statistika_en$Q_4
table(ages, brands)
prop.table(x = table(ages, brands), margin = 1)
# Графично представяне
barplot(prop.table(x = table(ages, brands), margin = 1),col = rainbow(length(table(ages))) ,beside = TRUE)
legend(x = "top", legend = c("16-25","26-35", "35-50", "51+"),
       cex = 0.8, fill = rainbow(length(table(ages))))

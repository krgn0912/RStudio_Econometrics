library(readxl)

#загрузили бд
baza<-read_excel('C:/Users/Home/Desktop/БД Кургина С.М., Дегтярёв А.С..xlsx')
baza

#описательная статистика
summary(baza)

#корреляция
library(corrplot) #библиотека для матрицы
corrplot(cor(baza),
         method = "color",
         type = "upper", #тепловое отображение
         addCoef.col = "black", #добавила значения в цветные ячейки
         number.cex = 0.7, #шрифт для значений
         tl.cex = 0.8, #шрифт для переменных
         tl.col = "black",#заливка переменных
         title = "Матрица корреляций", #вывела название матрицы
         mar = c(0, 0, 2, 0)) #отступ для названия

#бокс плот
par(mfrow = c(3, 4)) #расположение графиков 3 на 4
par(mar = c(2, 2, 2, 1))
for(i in 1:ncol(baza)) {
  if(names(baza)[i] != "x8med") { #если из базы убрать x8med
    boxplot(baza[[i]], main = names(baza)[i], col = "green") #цвет зеленый
  }
}
mtext("Боксплоты", side = 3, outer = TRUE, line = -1) #общее название графика

#выбросы
outliers <- sapply(baza, function(x) {
  if(!is.numeric(x)) return(0)
  qvant <- quantile(x, c(0.25, 0.75), na.rm = TRUE) #25 и 75 квантили
  graniza <- qvant + c(-1.5, 1.5) * (qvant[2] - qvant[1]) #границы выбросов
  sum(x < graniza[1] | x > graniza[2], na.rm = TRUE) #кол-во чисел вне границ
})
vibros <- data.frame(Переменная = names(baza), Выбросы = outliers) #сделала таблицу с название перменных и числом выбросов 
print(vibros) #вывела результат

#плотность распределения y
plot(density(baza$y), main = "Плотность y", col = "blue", lwd = 2, 
     xlab = "", ylab = "") #график плотности для y
polygon(density(baza$y), col = "lightblue") #заливка синим
abline(v = c(mean(baza$y), median(baza$y)), col = c("red", "green"), lwd = 2) #внесла на график ср и мед
s <- shapiro.test(baza$y) #тест шапиро-уилка
legend("topright", 
       legend = paste(c("Среднее", "Медиана", "W", "p"), 
                      c(round(mean(baza$y), 2), round(median(baza$y), 2),
                        round(s$statistic, 3), round(s$p.value, 4))),
       col = c("red", "green", "black", "black"), bty = "n") #легенда что есть что

#проверка на линейность
library(patchwork)
library(ggplot2)
variables <- c("x1div","x2mar","x3sal","x4die","x5unempl","x6gini","x7dec","x8med","x9hous","x10ir","x11we","x12gdp")
plot_list <- list()
for (var in variables) {
  p <- ggplot(baza, aes(x = .data[[var]], y = .data[["y"]])) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    geom_smooth(method = "loess", color = "blue", se = FALSE, linetype = "dashed") +
      labs(title = paste("y vs", var),
           x = var, y = "y") +
      theme_minimal()
    plot_list[[var]] <- p
}
final_plot <- wrap_plots(plot_list, ncol = 4, nrow = 3)
print(final_plot)

#рассеивание 
par(mfrow = c(3, 4)) #графики будут 3 на 4
par(mar = c(3, 3, 2, 1)) #отступы
for (i in 1:ncol(baza)) {
  if (names(baza)[i] != "x8med") {
    plot(baza[[i]], 
         pch = 19,
         col = "black", #черный цвет выбрала
         cex = 0.7,
         xlab = "Наблюдения",#ось x
         ylab = names(baza)[i], #ось y
         main = names(baza)[i]) #заголовок
  }
}

#линейная регрессия
linmod<-lm(y~x3sal+x11we+x9hous+x5unempl+x6gini+x7dec+x10ir+x12gdp+x1div,data=baza)
summary(linmod)

#дамми переменная 
x8med<-ifelse(baza$x8med == "1",1,0) #создала дамми перменную для med

#создала новый дф
df_reg<-data.frame(y=baza$y,
x3sal=baza$x3sal,
x11we=baza$x11we,
x9hous=baza$x9hous,
x5unempl=baza$x5unempl,
x6gini=baza$x6gini,
x7dec=baza$x7dec,
x10ir=baza$x10ir,
x12gdp=baza$x12gdp,
x1div=baza$x1div,
x8med=x8med)

#разделила на две подвыборки на основе дамми
med_yes<-subset(df_reg,x8med==1)
med_no<-subset(df_reg,x8med !=1)

#построим 3 модели
model_yes<-lm(y~x3sal+x11we+x9hous+x5unempl+x6gini+x7dec+x10ir+x12gdp+x1div,data=med_yes)
model_no<-lm(y~x3sal+x11we+x9hous+x5unempl+x6gini+x7dec+x10ir+x12gdp+x1div,data=med_no)
model_full<-lm(y~x3sal+x11we+x9hous+x5unempl+x6gini+x7dec+x10ir+x12gdp+x1div,data=df_reg)

#посчитала рсс
rss_yes<-sum(resid(model_yes)^2)
rss_no<-sum(resid(model_no)^2)
rss_full<-sum(resid(model_full)^2)

#статистика для теста чоу
chow_stat<-((rss_full-(rss_no+rss_yes))/10)/((rss_no+rss_yes)/(nrow(baza)-20))
p_value<-1-pf(chow_stat,df1=10, df2=nrow(baza)-20)
cat("Chow test stat:",chow_stat,"\n")
cat("p-value:",p_value,"\n")

#тест рамсея
library(lmtest)
reset_ramsey<-resettest(model_full)
reset_ramsey

#логарифмируем
ln_y<-log(baza$y)
ln_x3sal<-log(baza$x3sal)
ln_x11we<-log(baza$x11we)
ln_x9hous<-log(baza$x9hous)
ln_x5unempl<-log(baza$x5unempl)
ln_x6gini<-log(baza$x6gini)
ln_x7dec<-log(baza$x7dec)
ln_x10ir<-log(baza$x10ir)
ln_x12gdp<-log(baza$x12gdp)
ln_x1div<-log(baza$x1div)

#модель с результатами оценки регрессии
log_lin<-lm(ln_y~ln_x3sal+ln_x11we+ln_x9hous+ln_x5unempl+ln_x6gini+ln_x7dec+ln_x10ir+ln_x1div+ln_x12gdp, data=baza)
summary(log_lin)

library(MASS)
y<-baza$y
x1<-baza$x3sal
x2<-baza$x11we
x3<-baza$x9hous
x4<-baza$x5unempl
x5<-baza$x6gini
x6<-baza$x7dec
x7<-baza$x10ir
x8<-baza$x1div
x9<-baza$x12gdp
bc<-boxcox(y~x1+x2+x3+x4+x5+x6+x7+x8+x9)
lambda<-bc$x[which.max(bc$y)]
lambda

#мультиколленарность 
library(car)
vif(model_full)

new_full<-lm(y~+x11we+x9hous+x5unempl+x6gini+x10ir+x1div+x12gdp, data=baza)
summary(new_full)

#тест уайта
white_test<-bptest(new_full,~fitted(new_full)^2)
white_test

#тест голфельда кванта
gq_test<-gqtest(new_full)
gq_test

#тест бройша пагана
bp_test<-bptest(new_full)
bp_test

#взвеш мнк
bp<-1/lm(abs(new_full$residuals)~new_full$fitted.values)$fitted.values^2 #вес
vz_bp_new_full<-lm(y ~ +x11we + x9hous + x5unempl + x6gini + x10ir + x1div + x12gdp,data=baza,weights = bp)
summary(vz_bp_new_full)

#робастые ошибки
install.packages("estimatr")
library(estimatr)
HC_rob_new_model<-lm_robust(y ~ +x11we + x9hous + x5unempl + x6gini + x10ir + x1div + x12gdp, data=baza,se_type = "HC3")
summary(HC_rob_new_model)

#дарбин вотсон
dwt(vz_bp_new_full)

#бройша годфри
bgtest(vz_bp_new_full,order=2)
b<-read_excel('C:/Users/Home/Desktop/БД.xlsx',sheet=1) #добавили в бд инстументы

#эндогенность
library(AER)
model_iv <- ivreg(y ~ x11we + x9hous + x5unempl + x6gini + x10ir + x1div + x12gdp |x11we + x9hous + x6gini + x10ir+x1div + x12gdp +inst_wage,data=b)
summary(model_iv)

model_vi <- ivreg(y ~ x11we + x9hous + x5unempl + x6gini + x10ir + x1div + x12gdp |x11we + x9hous + x6gini +x5unempl+ x10ir + x12gdp +vio,data=b)
summary(model_vi)

#доп кластерный анализ
library(cluster)
library(factoextra)

#стандартизация
data_num <- baza[, sapply(baza, is.numeric)]
data_scaled <- scale(na.omit(data_num))

#метод силуэта
fviz_nbclust(data_scaled, pam, method = "silhouette", k.max = 10) + 
  ggtitle("Оптимальное k (силуэт)")


#метод локтя
fviz_nbclust(data_scaled, pam, method = "wss", k.max = 10) +
  ggtitle("Оптимальное k (локоть)")

#k=2
k <-2
pam_res <- pam(data_scaled, k = k)


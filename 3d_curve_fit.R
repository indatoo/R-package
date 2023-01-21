library(xlsx)
# setwd("D:/围封meta/文献整理")#分析用excel所在的位置
# dat1 <- read.xlsx("#MATA.xlsx",sheetIndex = 6) 
# R语言使用complete.cases函数筛选出dataframe中不包含缺失值的所有数据行
# dat_ANPP <- dat1[complete.cases(dat1[,14]),]
# dat_SOC <- dat1[complete.cases(dat1[,22]),]
dat_ANPP = iris
head(iris)

library(mgcv)
library(dplyr)
library(plotly)

x <- dat_ANPP$Sepal.Length#x
y <- dat_ANPP$Sepal.Width#y
z <- dat_ANPP$Petal.Length#z

# gam函数 - 广义加性模型 
mod <- gam(z ~ te(x) + te(y) + ti(x, y), data=dat_ANPP)
# seq 函数的作用是生成一串数字
x.seq <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length=25)
y.seq <- seq(min(y, na.rm=TRUE), max(y, na.rm=TRUE), length=25)

# 这个函数应该是生成拟合情况的
predfun <- function(x,y){
  newdat <- data.frame(x = x, y=y)
  predict(mod, newdata=newdat)
}
# outer 是用来求外积
# 整个这个就是向量化输出结果# 需要确认
fit <- outer(x.seq, y.seq, Vectorize(predfun))

a <- plot_ly() %>% 
  add_markers(x = ~x, y=y, z=z) %>% 
  add_surface(x = ~x.seq, y = ~y.seq, z = t(fit))
a
a <- lm(z~x+y+x*y,dat_ANPP)
summary(a)


a = read_csv('~/Downloads/student-alcohol-consumption/student-mat.csv')
b = read_csv('~/Downloads/student-alcohol-consumption/student-por.csv')

dim(a)
dim(b)
a
b


with(a, plot(age, G1))

require(tidyverse)

a = read_csv('~/Downloads/avocado.csv')
a = subset(a, year == 2017)

r = lm(AveragePrice ~ Date,data=a)
a$resid = r$residuals
a$predi = predict(r)

set.seed(27)
ggplot(a[sample(1:nrow(a),200),], mapping = aes(Date, AveragePrice)) + geom_point() + theme_bw() +
  labs(y = 'Average price') + 
  geom_segment(aes(x = Date, xend=Date,y = AveragePrice, yend = predi),col='red')+
  geom_smooth(method='lm', col = 'steelblue', lwd=2, se=FALSE)


cor.test(formula = ChickWeight, data = ChickWeight)

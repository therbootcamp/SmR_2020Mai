
colTrans = function(col,perc) {
  rgb(t(grDevices::col2rgb(col)), maxColorValue=255, alpha = perc*255)
  }

Cyan = "#235A97" 
Pink = "#EA4B68" 
Gray = "#606060" 
Green = "#6ABA9A"
yellow = "#EACC48"

m1 = 80
m2 = 120
s1 = 25
s2 = 25

png('_sessions/NewStats/image/power.png',width=1200, height=800)

par(mar=c(0,0,20,0))

plot.new();plot.window(xlim=c(0,200),ylim=c(0,.016))

mtext('Small N', side=3, cex=6, col = Green, line=15, font=2)

xs = seq(0,200,.01)

lines(xs, dnorm(xs, m2, s2), lwd = 8, col = Pink)

xlim = qnorm(.95, m1, s1)
xs = seq(0, xlim, .01)
ys = dnorm(xs, m2, s2)
polygon(c(xs, rev(xs)),c(ys, rep(-.00015, length(ys))), border=NA, col=Pink)

xs = seq(0,200,.01)
lines(xs, dnorm(xs, m1, s1), lwd = 8, col = Gray)


xlim = qnorm(.95, m1, s1)
xs = seq(xlim,200,.01)
ys = dnorm(xs, m1, s1)
polygon(c(xs, rev(xs)),c(ys, rep(-.00015, length(ys))), border=NA, col=Gray)

text(100, .005, label = expression(beta), cex=8, col = 'white')
text(129, .0009, label = expression(alpha), cex=8, col = 'white')
text(61, .00523, label = expression(1-alpha), cex=4, col = Gray)
text(139, .005, label = expression(1-beta), cex=4, col = Pink)

dev.off()

m1 = 80
m2 = 120
s1 = 12
s2 = 12

png('_sessions/NewStats/image/power_large.png',width=1200, height=800)

par(mar=c(0,0,6,0))

plot.new();plot.window(xlim=c(0,200),ylim=c(0,.035))

mtext('Large N', side=3, cex=6, col = Green, line=1, font=2)

xs = seq(0,200,.01)

lines(xs, dnorm(xs, m2, s2), lwd = 8, col = Pink)

xlim = qnorm(.95, m1, s1)
xs = seq(0, xlim, .01)
ys = dnorm(xs, m2, s2)
polygon(c(xs, rev(xs)),c(ys, rep(-.0003, length(ys))), border=NA, col=Pink)

xs = seq(0,200,.01)
lines(xs, dnorm(xs, m1, s1), lwd = 8, col = Gray)


xlim = qnorm(.95, m1, s1)
xs = seq(xlim,200,.01)
ys = dnorm(xs, m1, s1)
polygon(c(xs, rev(xs)),c(ys, rep(-.0003, length(ys))), border=NA, col=Gray)

text(95.8, .0013, label = expression(beta), cex=6, col = 'white')
text(104.8, .0013, label = expression(alpha), cex=6, col = 'white')
text(75, .00523, label = expression(1-alpha), cex=4, col = Gray)
text(125, .005, label = expression(1-beta), cex=4, col = Pink)

dev.off()





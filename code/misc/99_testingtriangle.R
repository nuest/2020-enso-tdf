library(ade4)

data2= data.frame(R = 0:10) 
head(RGB0)  

    
data2$G <- 10:20
data2$B <- 20:30
head(data2)


triangle.plot <- triangle.plot(data2, clab = 0, cpoi = 2, 
                               addmean = F, 
                               show = F, addaxes = F, scale = T)



#plot(NA,NA,xlim=c(0,1),ylim=c(0,1),asp=1,bty="n",axes=F,xlab="",ylab="")
# bigger number makes it smoother
sm <- 150
x <- do.call(c, sapply(1:(sm*sqrt(3)/2)/sm,
                       function(i) (i*sm/sqrt(3)):(sm-i*sm/sqrt(3))/sm))
y <- do.call(c, sapply(1:(sm*sqrt(3)/2)/sm,
                       function(i) rep(i, length((i*sm/sqrt(3)):(sm-i*sm/sqrt(3))))))
d.red = y
d.green = abs(sqrt(3) * x - y) / sqrt(3 + 1)
d.blue = abs(- sqrt(3) * x - y + sqrt(3)) / sqrt(3 + 1)
points(x, y, col=rgb(1-d.red,1 - d.green,1 - d.blue), pch=19)
points(x, y, col=rgb(d.red,d.green,d.blue), pch=19)



points(triangle.plot, pch = 19, cex = 1, col = nacreHEX)  









library(colorscience)
Maxwell.triangle()
xl<-yl<-0:1
Maxwell.triangle(xlim=xl,ylim=yl)
Maxwell.triangle(conversionFunction=CIE1931XYZ2CIE1976uv, 
                 xlim=xl,ylim=yl,xlab="u'",ylab="v'")


library(trifield)

grid.size = 128
par(mar = rep(2, 4), oma = rep(0, 4))
tg = ternary.grid(grid.size)
f = function(x)
       sin(2 * pi * x[1]) +
       sin(3 * pi * x[2]) +
       sin(4 * pi * x[3])
z = ternary.apply(tg, f)
tf = ternary.field(tg, z)
plot(tf)
ternary.legend()

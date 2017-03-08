x = c(1,2,3)
y = c(5,1,7)
z = rbind(x,y)
mydata <- read.table("D:/P3A/data/hour2.csv",sep=";",header=TRUE)


for(i in mydata["dteday"]){
  a = as.Date(i, "%d/%m/%Y")
  z = as.numeric(format(a, "%d"))
}
mydata["day"]=z

write.table(mydata,file = "D:/P3A/data/hour2.csv",sep=";")

z = mydata[c("season","yr","mnth","holiday","weekday","workingday","weathersit")]

library(dummy)
for(i in 1:17379){
  
  z[i,"weathersit"]=toString(z[i,"weathersit"])
  
}
w = toString(z["season"])
ds= dummy(z,int = TRUE)
a = toString(1)

data = cbind(ds,mydata)
write.table(data,file = "D:/P3A/data/data4.csv",sep=";")

curves = cbind(low_curve,up_curve)
write.table(curves,file = "D:/P3A/P3a-master/confidence_int_data2.csv",sep=";")
a = 1:5
b = c(1,3,4,6,7)
c = c(2,4,5,8,9)
d = c("y","y","y","n","n")
df = data.frame(cbind(a,b,c))
names(df)= c("x","ymin","ymax")
g1 <- ggplot(df) + 
  stat_smooth(aes(x = x, y = ymin, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = ymax, colour = "max"), method = "loess", se = FALSE)
g1

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
df2 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y,yn = c(rep("n",20),rep("y",dim(gg1$data[[1]])[1]-20))) 

# use the loess data to add the 'ribbon' to plot 
g1+
  geom_ribbon(data = df2[df2$yn=="y",], aes(x = x, ymin = ymin, ymax = ymax),
              fill = "grey", alpha = 0.4)
for(i in 1:20){
  df2[i,"yn"]
}

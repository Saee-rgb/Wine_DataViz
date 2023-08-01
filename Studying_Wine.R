##Wine Datasets
## Unsupervised final for wine 
library(rpart)
library(rpart.plot)

set.seed(123)  # for reproducibility
df <- na.omit(Wine_dataset)
n <- nrow(df)
s <- sample(n, n)  
df_train <- df[s, ]
df_test <- df[-s, ]
dtm <- rpart(Color_intensity ~ .-class, df_train, method = "anova", control = rpart.control(maxdepth = 3))
rpart.plot(dtm, type = 4, extra = 101, main = "Best predictors for Color intenisty of  Wine")

##
library(ggplot2)
library(ggpubr)

#Kernel Densities 
#Alcohol
k1 = ggplot(Wine_dataset, aes(x = Alcohol)) +
  geom_histogram(aes(y = ..density..),
                 fill = "darkgreen",
                 color = "black") +
  geom_density( alpha = 0.5,fill = "lightgreen") +
  labs( x = "Alcohol",
        y = "Density")

#Flavanoids
k2 = ggplot(Wine_dataset, aes(x = Flavanoids)) +
  geom_histogram(aes(y = ..density..),
                 fill = "darkblue",
                 color = "black") +
  geom_density( alpha = 0.5 ,  fill = "lightblue")+ 
  labs(x = "Flavanoids",
       y = "Density")

#OD_dil
k3 = ggplot(Wine_dataset, aes(x = OD_dil)) +
  geom_histogram(aes(y = ..density..),
                 fill = "violet",
                 color = "black") +
  geom_density( alpha = 0.5 ,  fill = "violet")+ 
  labs(x = "OD_dil",
       y = "Density")


#Hue
k4 = ggplot(Wine_dataset, aes(x =Hue)) +
  geom_histogram(aes(y = ..density..),
                 fill = "pink",
                 color = "black") +
  geom_density( alpha = 0.5 , fill = "pink")+ 
  labs(x = "Hue",
       y = "Density")


ggarrange(k1,k2,k3,k4,nrow=2,ncol = 2) + labs(title = "Two or more clusters....from composition, Hue and OD_dil ")

#Kernel density for color intensity
ggplot(Wine_dataset, aes(x =Color_intensity)) +
  geom_histogram(aes(y = ..density..),
                 fill = "red",
                 color = "black") +
  geom_density( alpha = 0.5 , fill = "red")+ 
  labs(title = "Two or more clusters....from intensity", 
       x = "Color Intensity",
       y = "Density")

##Contour Plots 
library(ggplot2)
library(MASS)
library(ggpubr)

con_plot <- function(var1, var2 ,shades,plot_title ) {
  dens <- kde2d({{var1}}, {{var2}})
  filled.contour(dens$x, dens$y, dens$z, plot.axes = {
    axis(1)
    axis(2)
    contour(dens, add = TRUE)
  }, color.palette = colorRampPalette(shades), 
  xlab = deparse(substitute(var1)), ylab = deparse(substitute(var2)), key.title = "Density" , main = plot_title)
}


wine_cont1 = data.frame(df$Alcohol ,df$Hue,df$Color_intensity)
attach(wine_cont1)
cols = c("white", "violet")
t1 = "Contour Plot for Color intensity of Wine"
con_plot(df.Alcohol,df.Hue, cols,t1)

wine_cont2 = data.frame(df$Alcohol ,df$Flavanoids,df$Color_intensity)
attach(wine_cont2)
cols = c("white", "red")
t2 = "Contour Plot for Colour Intensity of Wine"
con_plot(df.Alcohol,df.Flavanoids, cols,t2)

wine_cont3 = data.frame(df$Color_intensity ,df$OD_dil,df$Alcohol)
attach(wine_cont3)
cols = c("white", "blue")
t3 = "Contour Plot for color intensity"
con_plot(df.OD_dil,df.Alcohol, cols,t3)








#Is staring at glass of wine , enough to label its classes  ? 
##Supervised final 
#Wine_dataset -classification
library(rpart)
library(rpart.plot)

set.seed(123)  # for reproducibility

df <- na.omit(Wine_dataset)
n <- nrow(df)
s <- sample(n, n)  
df_train <- df[s, ]
df_test <- df[-s, ]
dtm <- rpart(class ~ ., df_train, method = "class")
rpart.plot(dtm, type = 4, extra = 101, main = "Classification tree for Wine")


##Contour Plots 
library(ggplot2)
library(MASS)
library(ggpubr)


con_plot <- function(var1, var2 ,shades,plot_title ) {
  dens <- kde2d({{var1}}, {{var2}})
  filled.contour(dens$x, dens$y, dens$z, plot.axes = {
    axis(1)
    axis(2)
    contour(dens, add = TRUE)
  }, color.palette = colorRampPalette(shades), 
  xlab = deparse(substitute(var1)), ylab = deparse(substitute(var2)), key.title = "Density" , main = plot_title)
}




wine_cont1 = data.frame(df$Proline ,df$Hue,df$Flavanoids)
attach(wine_cont1)
cols = c("white", "violet")
t1 = "Contour Plot for Hue of Wine"
con_plot(df.Proline,df.Flavanoids, cols,t1)



wine_cont2 = data.frame(df$Proline ,df$OD_dil,df$Hue)
attach(wine_cont2)
cols = c("white", "red")
t2 = "Contour Plot for Hue of Wine"
con_plot(df.Proline,df.Flavanoids, cols,t2)


wine_cont3 = data.frame(df$Flavanoids ,df$OD_dil,df$Hue)
attach(wine_cont3)
cols = c("white", "blue")
t3 = "Contour Plot for Hue of Wine"
con_plot(df.Proline,df.Flavanoids, cols,t3)



#Kernel Densities 
#Proline
k1 = ggplot(Wine_dataset, aes(x = Proline)) +
  geom_histogram(aes(y = ..density..),
                 fill = "darkgreen",
                 color = "black") +
  geom_density( alpha = 0.5,fill = "lightgreen") +
  labs(title = "Kernel Density for Proline",
       x = "Proline",
       y = "Density")

#Flavanoids
k2 = ggplot(Wine_dataset, aes(x = Flavanoids)) +
  geom_histogram(aes(y = ..density..),
                 fill = "darkblue",
                 color = "black") +
  geom_density( alpha = 0.5 ,  fill = "lightblue")+ 
  labs(title = "Kernel Density for Flavanoids",
       x = "Flavanoids",
       y = "Density")

#OD_dil
k3 = ggplot(Wine_dataset, aes(x = OD_dil)) +
  geom_histogram(aes(y = ..density..),
                 fill = "darkblue",
                 color = "black") +
  geom_density( alpha = 0.5 ,  fill = "lightblue")+ 
  labs(title = "Kernel Density for OD_dil",
       x = "OD_dil",
       y = "Density")


#Hue
k4 = ggplot(Wine_dataset, aes(x =Hue)) +
  geom_histogram(aes(y = ..density..),
                 fill = "pink",
                 color = "black") +
  geom_density( alpha = 0.5 , fill = "pink")+ 
  labs(title = "Kernel Density for Hue ",
       x = "Hue",
       y = "Density")

ggarrange(k1,k2,k3,k4,nrow=2,ncol = 2)



##Violin plots 
Wine_dataset$class = as.factor(Wine_dataset$class)
p1 =ggplot(Wine_dataset, aes(x = class, y = Flavanoids , fill = class))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title="Flavanoids")

p2 = ggplot(Wine_dataset, aes(x = class, y = Proline , fill = class))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title= "Proline")

p3 = ggplot(Wine_dataset, aes(x = class, y = OD_dil , fill = class))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title="OD_dil")

p4 = ggplot(Wine_dataset, aes(x = class, y = Hue , fill = class))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title="Hue")

ggarrange(p1,p2,p3,p4 ,nrow=2,ncol=2)


#Scatterplot : considering all decision making variables
theme_set(theme_bw())

Hue_OD_dil = 
  ggplot(Wine_dataset, aes(y = Hue, x = OD_dil, color = class )) +
  geom_point() +labs(title = "Only Hue and OD_dil are not good classifiers") 

Pro_Flav = ggplot(Wine_dataset, aes(y = Flavanoids,x= OD_dil, color = class )) +
  geom_point() +labs(title = " Proline and Flavanoids are slightly better classifiers") 

scatter_plot0 = ggplot(Wine_dataset, aes(y =Flavanoids, x = Proline , color = Hue ,size = OD_dil, shape = class)) + geom_point()+
  labs(title = "Wine Classes" , subtitle = "Shape : Class , Color :Hue ,Size : OD_dil")

#What did decision tree exactly do ? 
#Step 1 : 
scatter_plot1= 
  ggplot(Wine_dataset, aes(y = Flavanoids, x = Proline, color = Hue, size = OD_dil, shape = class)) +
  geom_point() +labs(title = "Partitions based on Proline and Flavanoids") +
  geom_vline(xintercept = 755, color = "red") +
  
  geom_segment(aes(x = 755, y = 2.2, xend=max(Proline), yend = 2.2), color = "red",size =0.5)+
  geom_text(aes(label = "R1" ,x = 1500 , y =1),col= "red",size = 5)+
  geom_text(aes(label = "R2" ,x = 1500 , y =4.5),col= "Violet",size = 5)
scatter_plot1
scatter_plot2 = ggplot(Wine_dataset, aes(y = Flavanoids, x = Proline, color = Hue, size = OD_dil, shape = class)) +
  geom_point() +
  geom_vline(xintercept = 755, color = "red") +
  geom_segment(aes(x = 755, y = 2.2, xend=max(Proline), yend = 2.2), color = "red",size =0.5)+
  geom_text(aes(label = "R1" ,x = 1500 , y =1),col= "red",size = 5)+
  geom_text(aes(label = "R2" ,x = 1500 , y =4.5),col= "Violet",size = 5)+
  geom_segment(aes(x = 0, y = 1.5, xend=755, yend = 1.5), color = "yellow",size =0.5)+
  geom_text(aes(label = "R3" ,x = 450 , y =4),col= "darkgreen",size = 4) +   labs(title = "Partitions based on Spectroscopical property : Od_dil")
  

scatter_plot = 
ggplot(Wine_dataset, aes(y = Flavanoids, x = Proline, color = Hue, size = OD_dil, shape = class)) +
  geom_point() +
  geom_vline(xintercept = 755, color = "red") +
  labs(title = "All partitions ", subtitle = "R3,R4,R5: Visual inspection of Hue and OD_dil") +
  geom_segment(aes(x = 755, y = 2.2, xend=max(Proline), yend = 2.2), color = "red",size =0.5)+
  geom_text(aes(label = "R1" ,x = 1500 , y =1),col= "red",size = 5)+
  geom_text(aes(label = "R2" ,x = 1500 , y =4.5),col= "Violet",size = 5)+
  
  geom_segment(aes(x = 0, y = 1.5, xend=755, yend = 1.5), color = "yellow",size =0.5)+
  geom_segment(aes(x = 755, y = 1.5, xend=max(Proline), yend = 1.5), color = "yellow",size =0.5,type= "dashed")+ 
  geom_text(aes(label = "R3" ,x = 450 , y =4),col= "darkgreen",size = 4)+
  geom_text(aes(label = "R4" ,x = 400 , y =1),col= "pink",size = 4)+
  geom_text(aes(label = "R5" ,x = 900 , y =1.3),col= "black",size = 4)
 


Hue_OD_dil
Pro_Flav
scatter_plot0
scatter_plot1
scatter_plot2
scatter_plot
  

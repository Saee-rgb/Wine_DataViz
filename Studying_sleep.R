
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ggpubr)
set.seed(123)  # for reproducibility

df <- na.omit(Sleep_health_xl)
n <- nrow(df)
s <- sample(n, n)  
df_train <- df[s, ]
df_test <- df[-s, ]


#Regression tree 
dtm <- rpart(Quality~. , df_train, method = "anova")
rpart.plot(dtm, type = 4, extra = 101, main = "Regression tree for Sleep Quality")

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
df = data.frame(Sleep_health_xl)



sleep_cont1 = data.frame(df$Sleep_Duration ,df$Physical_Activity_Level,df$Quality)
attach(sleep_cont1)
cols = c("white", "violet")
t1 = "Contour Plot for Quality of Sleep"
con_plot(df.Sleep_Duration,df.Physical_Activity_Level, cols,t1)

sleep_cont2 = data.frame(df$Sleep_Duration ,df$Physical_Activity_Level,df$Stress_Level)
attach(sleep_cont2)
cols = c("white", "red")
t2 = "Contour Plot for Stress Level"
con_plot(df.Sleep_Duration,df.Physical_Activity_Level, cols,t2)

sleep_cont3 = data.frame(df$Sleep_Duration,df$Quality,df$Stress_Level)
attach(sleep_cont3)
cols = c("white", "yellow")
t3 = "Contour Plot for Stress Level"
con_plot(df.Sleep_Duration,df.Quality, cols,t3)

#Kernel Densities 
k1 = ggplot(Sleep_health_xl, aes(x = Sleep_Duration)) +
  geom_histogram(aes(y = ..density..),
                 fill = "darkgreen",
                 color = "black") +
  geom_density( alpha = 0.5,fill = "lightgreen") +
  labs(title = "Kernel Density for Sleep Duration",
       x = "Sleep Duration",
       y = "Density")

#Flavanoids
k2 = ggplot(Sleep_health_xl, aes(x = Daily_Steps)) +
  geom_histogram(aes(y = ..density..),
                 fill = "darkblue",
                 color = "black") +
  geom_density( alpha = 0.5 ,  fill = "lightblue")+ 
  labs(title = "Kernel Density for Daily_Steps",
       x = "Daily_Steps",
       y = "Density")

#OD_dil
k3 = ggplot(Sleep_health_xl, aes(x = Quality)) +
  geom_histogram(aes(y = ..density..),
                 fill = "darkblue",
                 color = "black") +
  geom_density( alpha = 0.5 ,  fill = "lightblue")+ 
  labs(title = "Kernel Density for Quality",
       x = "Quality",
       y = "Density")


#Hue
k4 = ggplot(Sleep_health_xl, aes(x =Stress_Level)) +
  geom_histogram(aes(y = ..density..),
                 fill = "pink",
                 color = "black") +
  geom_density( alpha = 0.5 , fill = "pink")+ 
  labs(title = "Kernel Density for Stress_Level ",
       x = "Stress Level",
       y = "Density")

ggarrange(k1,k2,k3,k4,nrow=2,ncol = 2)

##Scatter plot 

theme_set(theme_bw())
ggplot(Sleep_health_xl, aes(y =Sleep_Duration, x = Heart_Rate , color = Stress_Level ,size = Quality, shape = Occupation)) + geom_point()+
  labs(title = "Lifestyle ")

theme_set(theme_bw())
ggplot(Sleep_health_xl, aes(y =Sleep_Duration, x = Heart_Rate , color = Stress_Level ,size = Quality, shape = Gender)) + geom_point()+
  labs(title = "Lifestyle" , xlab ="Heart rate " , ylab = "Sleep Duration")




##Violin plots 

p1 =ggplot(Sleep_health_xl, aes(x = Disorder, y = Sleep_Duration , fill = Disorder))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title="Sleep Duration")


p2 = ggplot(Sleep_health_xl, aes(x = Disorder , y = Quality , fill = Disorder))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title= "Quality of sleep")

p3 = ggplot(Sleep_health_xl, aes(x = Disorder , y = BP_Sys , fill = Disorder))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title= "Systolic Blood Pressure ")


p4 = ggplot(Sleep_health_xl, aes(x = Disorder , y =Stress_Level , fill = Disorder))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title= "Stress level")

ggarrange(p1,p2,p3,p4 ,nrow=2,ncol=2)





# #Classification tree
dtm <- rpart(Disorder ~., df_train, method = "class")
rpart.plot(dtm, type = 4, extra = 101, main = "Classification tree for Sleep DIsorder")



##Kernel Densities of normal , obese, overweight  
##Violin plots 

v1 =ggplot(Sleep_health_xl, aes(x = BMI_Category, y = Sleep_Duration , fill = BMI_Category))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title="Sleep Duration")


v2 = ggplot(Sleep_health_xl, aes(x = BMI_Category , y = Quality , fill = BMI_Category))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title= "Quality of sleep")

v3 = ggplot(Sleep_health_xl, aes(x = BMI_Category , y = BP_Sys , fill = BMI_Category))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
  labs(title= "Systolic Blood Pressure ")


v4 = ggplot(Sleep_health_xl, aes(x = BMI_Category, y =Stress_Level , fill = BMI_Category))+
  geom_violin(trim = FALSE)+guides(fill = "none")+geom_boxplot(width = 0.1)+
 labs(title= "Stress level")

ggarrange(v1,v2,v3,v4 ,nrow=2,ncol=2)



#Scatterplot 
theme_set(theme_bw())
ggplot(Sleep_health_xl, aes(y =Sleep_Duration, x = BP_Sys , color = Disorder ,size = Quality, shape = Occupation)) + geom_point()+
  labs(title = "Lifestyle ")

theme_set(theme_bw())
ggplot(Sleep_health_xl, aes(y =Sleep_Duration, x = BP_Sys , color = Disorder ,size = Quality, shape = Gender)) + geom_point()+
  labs(title = "Lifestyle ")



library(dplyr)
library(ggplot2)
library(grid)
library(ggpubr)


##ATLANTICO----
data<- read.csv2("C:/Users/jrobledo/Desktop/Citricos atlántico.csv", encoding = "UTF8", dec=",")
colnames(data)<- c("Departamento", "Cultivo", "Año", "Área Sembrada (ha)", 
                   "Área Cosechada (ha)", "Producción (t)", "Rendimiento (t/ha)")
dinamic_table<- data %>% 
  group_by(Cultivo, Año) %>%
  summarise("Área Sembrada (ha)" = sum(`Área Sembrada (ha)`), "Área Cosechada (ha)" = sum(`Área Cosechada (ha)`), "Producción (t)" = sum(`Producción (t)`))
dinamic_table$`Rendimiento (t/ha)`<- dinamic_table$`Producción (t)`/dinamic_table$`Área Cosechada (ha)`
p<-ggplot(dinamic_table, aes(x=Año, y=`Área Sembrada (ha)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position="none")+
  scale_x_continuous("Año", labels = as.character(dinamic_table$Año), breaks = dinamic_table$Año)
p2<-ggplot(dinamic_table, aes(x=Año, y=`Área Cosechada (ha)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position="none")+
  scale_x_continuous("Año", labels = as.character(dinamic_table$Año), breaks = dinamic_table$Año)
p3<-ggplot(dinamic_table, aes(x=Año, y=`Producción (t)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position="none")+
  scale_x_continuous("Año", labels = as.character(dinamic_table$Año), breaks = dinamic_table$Año)
p4<-ggplot(dinamic_table, aes(x=Año, y=`Rendimiento (t/ha)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position="none")+
  scale_x_continuous("Año", labels = as.character(dinamic_table$Año), breaks = dinamic_table$Año)

#PONEDERA----  

dataP<- read.csv2("C:/Users/jrobledo/Desktop/Citricos ponedera.csv", encoding = "UTF8", dec=",")
colnames(dataP)<- c("Municipio", "Cultivo", "Año", "Área Sembrada (ha)", 
                   "Área Cosechada (ha)", "Producción (t)", "Rendimiento (t/ha)")
dinamic_tableP<- dataP %>% 
  filter(Municipio=="PONEDERA") %>%
  group_by(Cultivo, Año) %>%
  summarise("Área Sembrada (ha)" = sum(`Área Sembrada (ha)`), "Área Cosechada (ha)" = sum(`Área Cosechada (ha)`), "Producción (t)" = sum(`Producción (t)`))
dinamic_tableP$`Rendimiento (t/ha)`<- dinamic_tableP$`Producción (t)`/dinamic_tableP$`Área Cosechada (ha)`
dinamic_tableP$`Rendimiento (t/ha)`[3:4]<- 0
p5<-ggplot(dinamic_tableP, aes(x=Año, y=`Área Sembrada (ha)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position="none")+
  scale_x_continuous("Año", labels = as.character(dinamic_tableP$Año), breaks = dinamic_tableP$Año)
p6<-ggplot(dinamic_tableP, aes(x=Año, y=`Área Cosechada (ha)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position="none")+
  scale_x_continuous("Año", labels = as.character(dinamic_tableP$Año), breaks = dinamic_tableP$Año)
p7<-ggplot(dinamic_tableP, aes(x=Año, y=`Producción (t)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position="none")+
  scale_x_continuous("Año", labels = as.character(dinamic_tableP$Año), breaks = dinamic_tableP$Año)
p8<-ggplot(dinamic_tableP, aes(x=Año, y=`Rendimiento (t/ha)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position="none")+
  scale_x_continuous("Año", labels = as.character(dinamic_tableP$Año), breaks = dinamic_tableP$Año)


#Graficos----
pn<-ggplot(dinamic_table, aes(x=Año, y=`Área Sembrada (ha)`, group=Cultivo)) +
  geom_line(aes(color=Cultivo))+
  geom_point(aes(color=Cultivo))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_x_continuous("Año", labels = as.character(dinamic_table$Año), breaks = dinamic_table$Año)
leg<- get_legend(pn)
leg<- as_ggplot(leg)
leg2<- as_ggplot(NULL)
grid.newpage()
pdf(file = "C:/Users/jrobledo/Desktop/popo.pdf")
grid.draw(cbind(rbind(ggplotGrob(p), ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4)),
                rbind(ggplotGrob(p5), ggplotGrob(p6), ggplotGrob(p7), ggplotGrob(p8)),
                rbind(ggplotGrob(leg), ggplotGrob(leg2), ggplotGrob(leg2), ggplotGrob(leg2))
                ))
dev.off()



library(flexdashboard)
library(ggplot2)
library(readr)
library(highcharter)
library(dplyr)
library(plotly)
library(tidyr)
library(DT)
library(tidyverse)
library(tidyquant)
library(shiny)
library(rsconnect)
library(colourpicker)
library(readxl)
library(scales)
library(gganimate)
library(dygraphs)

#######################################
#######################################
inf_general <- read_excel('C:/Users/uticona/Documents/Office_feb_2021/Dashboard_inflación_abril/madre.xlsx')
inf_general$date <- as.Date(inf_general$fecha, format = "%Y-%m-%d")

mytable <- read_excel('C:/Users/uticona/Documents/Office_feb_2021/Dashboard_inflación_abril/tabla.xlsx')
mytable_m <- read_excel('C:/Users/uticona/Documents/Office_feb_2021/Dashboard_inflación_abril/tabla_m.xlsx')
mytable_acum <- read_excel('C:/Users/uticona/Documents/Office_feb_2021/Dashboard_inflación_abril/tabla_acum.xlsx')

#Grafico anual
chartA <- ggplot(data = inf_general, aes(x=date, y=anual, group=1,
          text = paste(" ", format(date, "%b %Y"),
          "<br> ", round(anual, digits=2),"%")
          )) +
          geom_line(color = "#deaa00", size = 1) +
          scale_x_date(date_labels = "%Y", date_breaks = "1 years",
          date_minor_breaks = "1 years",
          limits=as.Date(c('2015-01-01', NA))) +
          scale_y_continuous(breaks=seq(-2,6, by=1))+
  theme(
  axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust=1),
  axis.text.y = element_text(size = 12),
  axis.title.x=element_blank())+
  ylab("%") +
  coord_cartesian(ylim=c(-2, 6)) +
  geom_hline(yintercept=0, size=0.25)
  
f <- list(
  side = "right"
)

chartAA <- ggplotly(chartA, tooltip = "text") %>% layout(yaxis = f)
chartAA

#############################################
#############################################
##########prueba dygraph anual###############

prueba <- data.frame(inf_general$date, inf_general$anual)
prueba.dy <- xts(x = prueba$inf_general.anual, order.by = prueba$inf_general.date)

wow <- dygraph(prueba.dy) %>% 
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(dateWindow = c("2015-01-01", "2021-04-01")) %>%
  dyAxis("y", label = "%")
wow

#############################################
#############################################
#############################################

#Grafico mensual - línea
chartB <- ggplot(data = inf_general, aes(x=date, y=mensual, group=1,
          text = paste(" ", format(date, "%b %Y"),
          "<br> ", round(mensual, digits=2),"%")
          )) +                              
          geom_line(color = "#deaa00", size = 1) +
          scale_x_date(date_labels = "%Y", date_breaks = "1 years",
          date_minor_breaks = "1 years",
          limits=as.Date(c('2010-01-01', NA))) +
          scale_y_continuous(breaks=seq(-2,2, by=0.5)) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 12),
    axis.title.x=element_blank())+
  ylab("%") +
  coord_cartesian(ylim=c(-2, 2)) +
  geom_hline(yintercept=0, size=0.25)

chartBB <- ggplotly(chartB, tooltip="text") %>% layout(yaxis = f)
chartBB

#Grafico mensual - barra

inf_longm = gather(inf_general, variable, valor, 3, factor_key=T)
inf_longm <- inf_longm[62:64]

chartBm <- ggplot(data = inf_longm, aes(x=date, y=valor, fill=variable, group=1,
                                        text = paste(" ", format(date, "%b %Y"),
                                                     "<br> ", round(valor, digits=2),"%")
)) +  
  geom_bar(stat="identity", fill = "#deaa00") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-2,1.5, by=0.5)) +
  theme(
    axis.text.x = element_text(size=12, angle = 30, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-2, 1.5))+
  geom_hline(yintercept=0, size=0.25)

chartBBm <- ggplotly(chartBm, tooltip = "text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))

chartBBm

#Grafico acumulada
chartC <- ggplot(data = inf_general, aes(x=date, y=acumulada, group=1,
          text = paste(" ", format(date, "%b %Y"),
          "<br> ", round(acumulada, digits=2),"%")
          )) + 
          geom_line(color = "#deaa00", size = 1) +
          scale_x_date(date_labels = "%Y", date_breaks = "1 years",
          date_minor_breaks = "1 years",
          limits=as.Date(c('2015-01-01', NA))) +
          scale_y_continuous(breaks=seq(-0.5,4, by=0.5)) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 12),
    axis.title.x=element_blank())+
  ylab("%") +
  coord_cartesian(ylim=c(-0.5, 4)) +
  geom_hline(yintercept=0, size=0.25)

chartCC <- ggplotly(chartC, tooltip="text") %>% layout(yaxis = f)
chartCC

#######################################
#######################################
###############Anual###################

#Tendencia inflacionaria
chartD <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
          text = paste(" ", format(date, "%b %Y"),
          "<br>General: ", round(anual, digits=2),"%",
          "<br>Núcleo: ", round(y_nucleo, digits=2),"%",
          #"<br>Suby.: ", round(y_subyacente, digits=2),"%",
          "<br>Sin alim.: ", round(y_sin_alimentos, digits=2),"%")
          )) +                     
          geom_line(aes(y=anual , col='Gral.'), size=1, alpha=.5) +
          geom_line(aes(y=y_nucleo, col='Núcleo'),  size=1, alpha=0.5) +
          #geom_line(aes(y=y_subyacente, col='Suby.'),  size=1, alpha=0.5) +
          geom_line(aes(y=y_sin_alimentos, col='Sin alimentos'),  size=1, alpha=0.5) +
          scale_color_brewer(palette="Dark2") +
          scale_x_date(date_labels = "%Y", date_breaks = "1 years",
          date_minor_breaks = "1 years",
          limits=as.Date(c('2015-01-01', NA))) +
          scale_y_continuous(breaks=seq(-2,6, by=1)) +
                       theme(
                         legend.position = "bottom",
                         axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
                         axis.text.y = element_text(size = 12),
                         legend.title = element_blank(),
                         axis.title.x= element_blank())+
                       ylab("%")+
                       coord_cartesian(ylim=c(-2, 6))+
                       geom_hline(yintercept=0, size=0.25)
                       
chartDD <- ggplotly(chartD, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.3))
chartDD

#######################################
#######################################
###############Mensual###################

#Tendencia inflacionaria
chartDDD <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
            text = paste(" ", format(date, "%b %Y"),
            "<br>General: ", round(mensual, digits=2),"%",
            "<br>Núcleo: ", round(m_nucleo, digits=2),"%",
            #"<br>Suby.: ", round(m_subyacente, digits=2),"%",
            "<br>Sin alim.: ", round(m_sin_alimentos, digits=2),"%")
            )) +
  geom_line(aes(y=mensual , col='Gral.'), size=1, alpha=.5) +
  geom_line(aes(y=m_nucleo, col='Núcleo'),  size=1, alpha=0.5) +
  #geom_line(aes(y=m_subyacente, col='Suby.'),  size=1, alpha=0.5) +
  geom_line(aes(y=m_sin_alimentos, col='Sin alimentos'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-2,1.5, by=0.5)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-2, 1.5))+
  geom_hline(yintercept=0, size=0.25)

chartDDD1 <- ggplotly(chartDDD, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartDDD1

#######################################
#######################################
###############Acumulado###############

#Tendencia inflacionaria
chartEEE <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
            text = paste(" ", format(date, "%b %Y"),
            "<br>General: ", round(acumulada, digits=2),"%",
            "<br>Núcleo: ", round(acum_nucleo, digits=2),"%",
            #"<br>Suby.: ", round(acum_y_subyacente, digits=2),"%",
            "<br>Sin alim.: ", round(acum_sin_alimentos, digits=2),"%")
            )) +
  geom_line(aes(y=acumulada , col='Gral.'), size=1, alpha=.5) +
  geom_line(aes(y=acum_nucleo, col='Núcleo'),  size=1, alpha=0.5) +
 #geom_line(aes(y=acum_y_subyacente, col='Suby.'),  size=1, alpha=0.5) +
  geom_line(aes(y=acum_sin_alimentos, col='Sin alimentos'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-0.5,4, by=0.5)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 30, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-0.5, 4))+
  geom_hline(yintercept=0, size=0.25)

chartEEE1 <- ggplotly(chartEEE, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartEEE1

#######################################
#######################################
####Núcleo, fuera del núcleo###########

#anual

inf_long = gather(inf_general, variable, valor, 24:25, factor_key=T)
inf_long <- inf_long[61:63]

chartNuc <- ggplot(data = inf_long, aes(x=date, y=valor, fill=variable, group=1,
            text = paste(" ", format(date, "%b %Y"),
            "<br> ", round(valor, digits=2),"%")
            )) +  
            geom_bar(stat="identity") +
            scale_x_date(date_labels = "%Y", date_breaks = "1 years",
            date_minor_breaks = "1 years",
            limits=as.Date(c('2015-01-01', NA))) +
            scale_y_continuous(breaks=seq(-2,6, by=1)) +
            theme(
              axis.text.x = element_text(size=12, angle = 30, vjust = 1, hjust = 1),
              axis.text.y = element_text(size = 12),
              legend.title = element_blank(),
              axis.title.x= element_blank())+
              ylab("%")+
              coord_cartesian(ylim=c(-2, 6))+
              geom_hline(yintercept=0, size=0.25)

chartNuc1 <- ggplotly(chartNuc, tooltip = "text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))

chartNuc1


#Mensual

inf_long1 = gather(inf_general, variable, valor, 26:27, factor_key=T)
inf_long1 <- inf_long1[61:63]

chartNuc2 <- ggplot(data = inf_long1, aes(x=date, y=valor, fill=variable, group=1,
             text = paste(" ", format(date, "%b %Y"),
             "<br> ", round(valor, digits=2),"%")
             )) +                      
  geom_bar(stat="identity") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
               scale_y_continuous(breaks=seq(-2,1.5, by=0.5)) +
  theme(
    axis.text.x = element_text(size=12, angle = 30, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
    ylab("%")+
    coord_cartesian(ylim=c(-2, 1.5))+
    geom_hline(yintercept=0, size=0.25)

chartNuc22 <- ggplotly(chartNuc2, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))

chartNuc22

#Acumulada

inf_long2 = gather(inf_general, variable, valor, 28:29, factor_key=T)
inf_long2 <- inf_long2[61:63]

chartNuc3 <- ggplot(data = inf_long2, aes(x=date, y=valor, fill=variable, group=1,
             text = paste(" ", format(date, "%b %Y"),
             "<br> ", round(valor, digits=2),"%")
             )) +
  geom_bar(stat="identity") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
               scale_y_continuous(breaks=seq(-1,4, by=0.5))+
  theme(
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-1, 4))+
  geom_hline(yintercept=0, size=0.25)

chartNuc33 <- ggplotly(chartNuc3, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.3))

chartNuc33



#######################################
#######################################
####Incidencias#######################

#Anual
division <- read_excel('C:/Users/uticona/Documents/Office_feb_2021/Dashboard_inflación_abril/division.xlsx')

chartDiv <- ggplot(data = division, aes(x=value, y=reorder(name, value), group=1, 
                  hoverinfo = 'text',
                  text = paste("<br> Inc (pp):", round(value, digits=2), 
                                "<br> Var (%):",  round(variacion, digits=2))))  + 
            geom_bar(stat = "identity")+
            labs(x = "pp", y = "")

chartDivDiv <- ggplotly(chartDiv, tooltip="text")
chartDivDiv

#Mensual
division_m <- read_excel('C:/Users/uticona/Documents/Office_feb_2021/Dashboard_inflación_abril/division_m.xlsx')

chartDiv_m <- ggplot(data = division_m, aes(x=value, y=reorder(name, value), group=1,
                    hoverinfo = 'text',                      
                    text = paste("<br> Inc (pp):", round(value, digits=2), 
                                 "<br> Var (%):",  round(variacion, digits=2))))  + 
               geom_bar(stat = "identity")+
               labs(x = "pp", y = "")

chartDivDiv_m <- ggplotly(chartDiv_m, tooltip="text")


#acumulada
division_acum <- read_excel('C:/Users/uticona/Documents/Office_feb_2021/Dashboard_inflación_abril/division_acum.xlsx')

chartDiv_acum <- ggplot(data = division_acum, aes(x=value, y=reorder(name, value), group=1,
                       hoverinfo = 'text',
                       text = paste("<br> Inc (pp):", round(value, digits=2), 
                                    "<br> Var (%):",  round(variacion, digits=2))))  + 
                 geom_bar(stat = "identity")+
                 labs(x = "pp", y = "")

chartDivDiv_acum <- ggplotly(chartDiv_acum, tooltip="text")


###########################################
###########################################
##################IPE######################

#Anual
chartIPE <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
                                         text = paste(" ", format(date, "%b %Y"),
                                                      "<br>IPE: ", round(y_ipe, digits=2),"%",
                                                      "<br>IPE stc: ", round(y_ipe_stc, digits=2),"%")
)) +                     
  geom_line(aes(y=y_ipe , col='Inflación externa relevante'), size=1, alpha=.5) +
  geom_line(aes(y=y_ipe_stc, col='IPE sin tipo de cambio'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-14,10, by=2))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-14, 10))+
  geom_hline(yintercept=0, size=0.25)

chartIPE1 <- ggplotly(chartIPE, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartIPE1

#Mensual
chartIPEm <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
                                           text = paste(" ", format(date, "%b %Y"),
                                                        "<br>IPE: ", round(m_ipe, digits=2),"%",
                                                        "<br>IPE stc: ", round(m_ipe_stc, digits=2),"%")
)) +                     
  geom_line(aes(y=m_ipe , col='Inflación externa relevante'), size=1, alpha=.5) +
  geom_line(aes(y=m_ipe_stc, col='IPE sin tipo de cambio'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-5,6, by=1))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-5, 6))+
  geom_hline(yintercept=0, size=0.25)

chartIPE1m <- ggplotly(chartIPEm, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartIPE1m

#Acumulada
chartIPEacum <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
                                            text = paste(" ", format(date, "%b %Y"),
                                                         "<br>IPE: ", round(acum_ipe, digits=2),"%",
                                                         "<br>IPE stc: ", round(acum_ipe_stc, digits=2),"%")
)) +                     
  geom_line(aes(y=acum_ipe , col='Inflación externa relevante'), size=1, alpha=.5) +
  geom_line(aes(y=acum_ipe_stc, col='IPE sin tipo de cambio'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-10,8, by=2))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-10, 8))+
  geom_hline(yintercept=0, size=0.25)

chartIPE1acum <- ggplotly(chartIPEacum, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartIPE1acum


###########################################
###########################################
##################Inflación importada######

#Anual
chartIMP <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
                                           text = paste(" ", format(date, "%b %Y"),
                                                        "<br>Inf. Importada: ", round(y_ipc_importado, digits=2),"%",
                                                        "<br>Alimentos: ", round(y_imp_alimentos, digits=2),"%",
                                                        "<br>Prendas: ", round(y_imp_prendas, digits=2),"%",
                                                        "<br>BBDD: ", round(y_imp_bbdd, digits=2),"%",
                                                        "<br>Otros: ", round(y_imp_otros, digits=2),"%")
                                                        )) +                     
  geom_line(aes(y=y_ipc_importado , col='Inflación importada'), size=2, alpha=0.75) +
  geom_line(aes(y=y_imp_alimentos, col='Alimentos'),  size=1, alpha=0.5) +
  geom_line(aes(y=y_imp_prendas, col='Prendas de vestir y textiles'),  size=1, alpha=0.5) +
  geom_line(aes(y=y_imp_bbdd, col='Bienes duraderos'),  size=1, alpha=0.5) +
  geom_line(aes(y=y_imp_otros, col='Otros bienes importados'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-4,6, by=1))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-4, 6))+
  geom_hline(yintercept=0, size=0.25)

chartIMP1 <- ggplotly(chartIMP, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartIMP1

#Mensual
chartIMPm <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
                                           text = paste(" ", format(date, "%b %Y"),
                                                        "<br>Inf. Importada: ", round(m_ipc_importado, digits=2),"%",
                                                        "<br>Alimentos: ", round(m_imp_alimentos, digits=2),"%",
                                                        "<br>Prendas: ", round(m_imp_prendas, digits=2),"%",
                                                        "<br>BBDD: ", round(m_imp_bbdd, digits=2),"%",
                                                        "<br>Otros: ", round(m_imp_otros, digits=2),"%")
)) +                     
  geom_line(aes(y=m_ipc_importado , col='Inflación importada'), size=2, alpha=0.75) +
  geom_line(aes(y=m_imp_alimentos, col='Alimentos'),  size=1, alpha=0.5) +
  geom_line(aes(y=m_imp_prendas, col='Prendas de vestir y textiles'),  size=1, alpha=0.5) +
  geom_line(aes(y=m_imp_bbdd, col='Bienes duraderos'),  size=1, alpha=0.5) +
  geom_line(aes(y=m_imp_otros, col='Otros bienes importados'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-1.5,2.5, by=0.5))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-1.5, 2.5))+
  geom_hline(yintercept=0, size=0.25)

chartIMP1m <- ggplotly(chartIMPm, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartIMP1m


#Acumulada
chartIMPacum <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
                                            text = paste(" ", format(date, "%b %Y"),
                                                         "<br>Inf. Importada: ", round(acum_ipc_importado, digits=2),"%",
                                                         "<br>Alimentos: ", round(acum_imp_alimentos, digits=2),"%",
                                                         "<br>Prendas: ", round(acum_imp_prendas, digits=2),"%",
                                                         "<br>BBDD: ", round(acum_imp_bbdd, digits=2),"%",
                                                         "<br>Otros: ", round(acum_imp_otros, digits=2),"%")
)) +                     
  geom_line(aes(y=acum_ipc_importado , col='Inflación importada'), size=2, alpha=0.75) +
  geom_line(aes(y=acum_imp_alimentos, col='Alimentos'),  size=1, alpha=0.5) +
  geom_line(aes(y=acum_imp_prendas, col='Prendas de vestir y textiles'),  size=1, alpha=0.5) +
  geom_line(aes(y=acum_imp_bbdd, col='Bienes duraderos'),  size=1, alpha=0.5) +
  geom_line(aes(y=acum_imp_otros, col='Otros bienes importados'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-4,4, by=1))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-4, 4))+
  geom_hline(yintercept=0, size=0.25)

chartIMP1acum <- ggplotly(chartIMPacum, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartIMP1acum

###########################################
###########################################
##################Expectativas############

#A once meses
chartEXP <- ggplot(data = inf_general, aes(x=date, y=value, color=variable, group=1,
                                           text = paste(" ", format(date, "%b %Y"),
                                                        "<br>Exp. inflación: ", round(exp_inf_11, digits=2),"%",
                                                        "<br>Inflación: ", round(anual, digits=2),"%")
                                                          ))+                     
  geom_line(aes(y=exp_inf_11, col='Expectativas de inflación'), size=1, alpha=0.5) +
  geom_line(aes(y=anual, col='Inflación'),  size=1, alpha=0.5) +
  scale_color_brewer(palette="Dark2") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               date_minor_breaks = "1 years",
               limits=as.Date(c('2015-01-01', NA))) +
  scale_y_continuous(breaks=seq(-1,6, by=1))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size=12, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x= element_blank())+
  ylab("%")+
  coord_cartesian(ylim=c(-1, 6))+
  geom_hline(yintercept=0, size=0.25)

chartEXP1 <- ggplotly(chartEXP, tooltip="text") %>% layout(yaxis = f, legend = list(orientation="h", x = 0, y = -0.2))
chartEXP1


library(janitor)


library(readxl)
saneamento <- read_excel("other/setores_analise.xlsx", 
                              sheet = "Saneamento")

saneamento<- saneamento[-c(1:2),]

saneamento <- janitor::clean_names(saneamento)

names(saneamento)[]

saneamento_trabalho<-
saneamento %>%
  pivot_longer(
    cols =  investimento_2019:reforco_de_capital_2021,
    names_to = "conta",
    values_to = "valor"
  ) %>%
  mutate(ano = str_sub(conta,str_length(conta)-3,str_length(conta))) %>%
  mutate(conta= str_sub(conta,1,str_length(conta)-5))

medianas<-
  saneamento_trabalho %>%
  filter(conta!="patrimonio_liquido") %>%
  group_by(conta,ano) %>%
  summarise(
    valor= median(valor,na.rm=TRUE)) %>%
  ungroup()

maximos<-
  saneamento_trabalho %>%
  filter(conta!="patrimonio_liquido") %>%
  group_by(conta,ano) %>%
  summarise(
    valor= max(valor,na.rm=TRUE)) %>%
  ungroup() %>%
  inner_join(saneamento_trabalho)


library(ggrepel)

library(colorspace)

#Corte por conta
saneamento_trabalho %>%
  filter(conta!="patrimonio_liquido") %>%
  ggplot()+
  geom_point(aes(x=ano, y=valor, fill=dependencia),
             #fill="white",
             pch=21, 
             color="#444444", 
             size=1)+
  geom_text_repel(data=maximos, aes(x=ano,y=valor,color=dependencia,label=sigla),size=2)+
  geom_line(data=medianas,aes(x=ano, y=valor, group=conta),color="white")+
  scale_color_discrete_qualitative(palette = "Dark 3")+
  scale_fill_discrete_qualitative(palette= "Dark 3")+
  facet_wrap(conta~., scales = "free_y")+ 
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white"),
    legend.position = "bottom"
  )

saneamento_trabalho %>%
  filter(conta!="patrimonio_liquido") %>%
  ggplot()+
  geom_point(aes(x=ano, y=valor, fill=dependencia),
             #fill="white",
             pch=21, 
             color="#444444", 
             size=1)+
  geom_text_repel(data=maximos, aes(x=ano,y=valor,color=dependencia,label=sigla),size=2)+
  geom_line(data=medianas,aes(x=ano, y=valor, group=conta),color="white")+
  scale_color_discrete_qualitative(palette = "Dark 3")+
  scale_fill_discrete_qualitative(palette= "Dark 3")+
  facet_wrap(conta~.)+ 
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white"),
    legend.position = "bottom"
  )




saneamento_trabalho %>%
  filter(conta!="patrimonio_liquido") %>%
  ggplot()+
  geom_line(data=medianas,aes(x=ano, y=valor, group=conta, color= conta ))+
  geom_point(aes(x=ano, y=valor, fill=conta),
             #fill="white",
             pch=21, 
             color="#444444", 
             size=1)+
  geom_text_repel(data=maximos, aes(x=ano,y=valor,color=conta,label=sigla),size=2)+
  scale_color_discrete_qualitative(palette = "Dark 3")+
  scale_fill_discrete_qualitative(palette= "Dark 3")+
  #facet_wrap(conta~., scales = "free_y")+ 
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white"),
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  labs(
    x=""
  )


saneamento_trabalho %>%
  filter(conta!="patrimonio_liquido") %>%
  ggplot()+
  #geom_line(data=medianas,aes(x=ano, y=valor, group=conta, color= conta ))+
  geom_point(aes(x=ano, y=valor, fill=conta),
             #fill="white",
             pch=21, 
             color="#444444", 
             size=1)+
  geom_text_repel(data=maximos, aes(x=ano,y=valor,color=conta,label=sigla),size=2)+
  scale_color_discrete_qualitative(palette = "Dark 3")+
  scale_fill_discrete_qualitative(palette= "Dark 3")+
  #facet_wrap(conta~., scales = "free_y")+ 
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white"),
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  labs(
    x=""
  )




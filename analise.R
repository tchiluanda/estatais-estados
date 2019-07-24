# https://github.com/tylermorganwall/rayshader

# https://www.curso-r.com/blog/2019-02-10-sf-miojo/

# https://www.tylermw.com/3d-ggplots-with-rayshader/


######### TO DO
# * retirar ticks dos gráficos de barra

library(rayshader)
library(ggplot2)
library(readxl)
library(tidyverse)
library(brazilmaps)
library(sf)
library(viridis)
library(extrafont)
library(gganimate)
library(scales)

loadfonts()

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Lora", colour = "grey20"),
      title = element_text(size = 10, color = "dimgrey", face = "plain"), 
      plot.subtitle = element_text(color = "grey20", face = "plain", size = 10),
      axis.text = element_text(colour = "grey20", size = 8, family = "Source Sans Pro"),
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(size = 0.5),
      axis.ticks.length = unit(.25, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none',
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9, family = "Source Sans Pro")
      )
}

setwd("~/GitHub/estatais-estados")

dados_empresas_raw <- read_excel("./dados/Estatais_rev.xlsx") %>%
  mutate(PL = as.numeric(PL),
         lucros = as.numeric(`Lucros / Prejuízos`)) %>%
  rename(dep = `Dependência`,
         seg = Segmento,
         emp = Empresa) %>%
  mutate(seg = case_when(
    seg == "Informática" ~ "INFORMÁTICA",
    seg == "ASSIS, TÉCNICA" ~ "ASSISTÊNCIA TÉCNICA",
    TRUE ~ seg))

tab_uf <- read_excel("./dados/tab_ufs.xlsx")

dados_empresas <- dados_empresas_raw %>% left_join(tab_uf, by = c("Estado" = "UF"))

# mapas -------------------------------------------------------------------

mapa <- get_brmap("State") 

mapa_dados <- mapa %>% 
  inner_join(dados_empresas, by = c("State" = "CODUF"))

mapa_qde <- mapa_dados %>%
  group_by(seg, State) %>%
  summarise(qde = n())

# graf_mapa <- ggplot(mapa_qde %>% filter(seg == "SANEAMENTO")) + 
#   geom_sf(aes(fill = qde > 0), color = "coral") + 
#   scale_fill_manual(values = c("TRUE" = "lightcoral", "FALSE" = NA)) +
#   # scale_fill_viridis_d(direction = 1,
#   #                    option = "magma")+#,
#   #                    #breaks = c(1e3, 100e3, 10000e3),
#   #                    #trans = "log", #para usar uma escala de log
#   #                    #labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) + 
#   #labs(fill = "População \n(milhões)") +
#   tema() + 
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         text = element_text(family = "Source Sans Pro"),
#         legend.position = "none",
#         legend.text = element_text(size = 10),
#         plot.background = element_blank(),
#         panel.background = element_blank())

dados_qde <- dados_empresas %>%
  group_by(Estado, seg) %>%
  summarise(qde = n()) %>%
  filter(!is.na(seg))


# gif ---------------------------------------------------------------------



segmentos <- data.frame("seg" = unique(dados_empresas$seg))

combinacao_est_seg <- merge(segmentos, tab_uf, by = NULL) %>%
  rename(Estado = UF,
         State = CODUF) %>%
  left_join(dados_qde) %>%
  left_join(mapa) %>%
  filter(!is.na(seg)) %>%
  arrange(seg)

graf_mapa_comp <- ggplot(combinacao_est_seg, aes(group = State))+# %>% filter(seg == "OUTRO")) + 
  geom_sf(aes(fill = ifelse(qde > 0, seg, NA), geometry = geometry), color = "ghostwhite") + 
  # scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = NA)) +
  scale_fill_viridis_d(direction = 1,
                     option = "plasma", na.value = "#EFEFEF")+#,
                     #breaks = c(1e3, 100e3, 10000e3),
                     #trans = "log", #para usar uma escala de log
                     #labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) +
  #labs(fill = "População \n(milhões)") +
  tema() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        legend.position = "none",
        legend.text = element_text(size = 10),
        plot.background = element_blank(),
        panel.background = element_blank())

# graf_mapa_comp + geom_text(aes(label = State), x = -15, y = -47, size = 10)

graf_mapa_gif <- graf_mapa_comp + transition_states(states = seg,
                                    transition_length = 1,
                                    state_length = 3) +
  labs(title = "Estados que possuem empresas do setor de {closest_state}") +
  theme(title = element_text(size = 13, face = "plain"))

animate(graf_mapa_gif, nframes = nrow(segmentos)*20, fps = 8, type = "cairo")

anim_save("mapa.gif", animation = last_animation())

# graf_mapa_comp2 <- ggplot(combinacao_est_seg)+ #%>% filter(seg == "SANEAMENTO")) + 
#   geom_sf(aes(fill = seg, geometry = geometry)) + 
#   scale_fill_viridis() +
#   # scale_fill_viridis_d(direction = 1,
#   #                    option = "magma")+#,
#   #                    #breaks = c(1e3, 100e3, 10000e3),
#   #                    #trans = "log", #para usar uma escala de log
#   #                    #labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) + 
#   #labs(fill = "População \n(milhões)") +
#   tema() + 
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         text = element_text(family = "Source Sans Pro"),
#         legend.position = "none",
#         legend.text = element_text(size = 10),
#         plot.background = element_blank(),
#         panel.background = element_blank())
# 
# graf_mapa_comp2 + transition_states(states = seg,
#                                 transition_length = 1,
#                                 state_length = 3) +
#   labs(title = "Estados que possuem empresas do setor de {closest_state}") +
#   theme(title = element_text(size = 12))
# 
# # quanto mais frames, mais se vê a movimentação dos estados
# 
# animate(gif_segmentos, nframes = nrow(segmentos)*30, fps = 10, type = "cairo")


# heatmap estados x setor -------------------------------------------------


graf_empXsetor <- ggplot(combinacao_est_seg%>%select(-geometry)) +
  geom_tile(aes(x = seg, y = reorder(nome, desc(nome)), fill = qde), color = "white") +
  scale_fill_viridis(direction = -1, na.value="ghostwhite", breaks = 1:max(combinacao_est_seg$qde, na.rm = T)) +
  labs(x = NULL, y = NULL, title = "Quantidade de empresas estatais por estado e setor", fill = "Quantidade") +
  tema() + theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5),
        axis.ticks = element_blank())

ggsave(plot = graf_empXsetor, "heatmap.png", h = 7.5, w = 6, type = "cairo-png")

plot_gg(graf_empXsetor,multicore=TRUE,width=6,height=8,scale=400)



# graficos de barra - quantidades -----------------------------------------



qde_empresas_seg <- dados_empresas %>% 
  group_by(seg, dep) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(seg) %>%
  mutate(qde_tot = sum(qde),
         dep = factor(dep, levels = c("Dependente", "Não Dependente"))) %>%
  filter(!is.na(seg))

graf_qde_emp <- 
  ggplot(qde_empresas_seg, aes(x = reorder(seg, qde_tot), y = qde, fill = dep)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = qde, y = qde), 
            vjust = 0.35, position = position_stack(vjust = 0.5, reverse = TRUE),
            family = "Source Sans Pro", size = 3, color = "darkgrey") +
  geom_text(aes(label = qde_tot, y = qde_tot + 1), 
            vjust = 0.35, check_overlap = TRUE,
            family = "Source Sans Pro", size = 4, color = "dimgrey") +  
  coord_flip() +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(x = NULL, y = NULL, title = "Quantidade de empresas por segmento", fill = NULL) +
  tema() + theme(axis.text = element_text(size = 9),
                 legend.position = "top")

ggsave(plot = graf_qde_emp, "qde_seg.png", h = 7.5, w = 6, type = "cairo-png")


qde_empresas_est <- mapa_dados %>% 
  select(-geometry) %>%
  group_by(nome, dep) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(nome) %>%
  mutate(qde_tot = sum(qde),
         dep = factor(dep, levels = c("Dependente", "Não Dependente"))) %>%
  filter(!is.na(dep))

graf_qde_emp_est <- 
  ggplot(qde_empresas_est, aes(x = reorder(nome, qde_tot), y = qde, fill = dep)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = qde, y = qde), 
            vjust = 0.35, position = position_stack(vjust = 0.5, reverse = TRUE),
            family = "Source Sans Pro", size = 3, color = "darkgrey") +
  geom_text(aes(label = qde_tot, y = qde_tot + 1), 
            vjust = 0.35, check_overlap = TRUE,
            family = "Source Sans Pro", size = 4, color = "dimgrey") +  
  coord_flip() +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(x = NULL, y = NULL, title = "Quantidade de empresas por Estado", fill = NULL,
       caption = "Ceará, Minas Gerais e Amapá não enviaram as ") +
  tema() + theme(axis.text = element_text(size = 9),
                 legend.position = "top")

ggsave(plot = graf_qde_emp_est, "qde_est.png", h = 7.5, w = 6, type = "cairo-png")



# ROE ---------------------------------------------------------------------

dados_roe <- dados_empresas %>%
  filter(PL != 0) %>%
  mutate(result = as.numeric(`Lucros / Prejuízos`),
         ROE = result / PL) %>%
  filter(!is.na(ROE)) %>%
  mutate(texto_hover = paste0(emp, ' (', Estado, ')\n',
                              'PL: R$ ', format(PL, big.mark = '.', decimal.mark = ","), '\n',
                              'Lucros / Prejuízos no ano: R$ ', format(result, big.mark = '.', decimal.mark = ","), '\n',
                              'ROE: ', percent(round(ROE,4))))

library(ggbeeswarm)

roe <- ggplot(dados_roe, aes(y = ROE, color = ROE, x = dep)) +
  #geom_jitter() +
  annotate("rect", ymin = quantile(dados_roe$ROE, 0.1), ymax = quantile(dados_roe$ROE, 0.9), fill = "lavender", xmin = -Inf, xmax = +Inf, alpha = 0.5) +
  geom_beeswarm() +
  scale_color_viridis(option = "magma") +
  geom_hline(yintercept = quantile(dados_roe$ROE, 0.9), linetype = "dotted") +
  geom_hline(yintercept = quantile(dados_roe$ROE, 0.1), linetype = "dotted") +
  annotate("text", x = 0.2, y = quantile(dados_roe$ROE, 0.9), vjust = -0.5,
           label = percent(quantile(dados_roe$ROE, 0.9), accuracy = 0.1), hjust = "inward", family = "Lora", color = "dimgrey", size = 3.5) +
  annotate("text", x = 0.2, y = quantile(dados_roe$ROE, 0.1), vjust = 1.4,
           label = percent(quantile(dados_roe$ROE, 0.1), accuracy = 0.1), hjust = "inward", family = "Lora", color = "dimgrey", size = 3.5) +
  annotate("curve", x = 0.5, xend = 0.75, 
           yend = 5, y = quantile(dados_roe$ROE, 0.5),
           curvature = -0.2, linetype = "dotted", color = "grey20") +
  labs(title = "Distribuição do ROE das empresas") +
  scale_y_continuous(labels = percent) +
  annotate("text", x = 0.75, y = 5,
           label = "80% das empresas têm ROE nessa faixa", hjust = "inward", family = "Lora", size = 3, color = "dimgrey", fontface = "italic") +
  tema()

ggsave(plot = roe, "roe.png", h = 7.5, w = 6, type = "cairo-png")

# plotly ------------------------------------------------------------------



library(plotly)

roe_plotly_log <- plot_ly(dados_roe, 
        x = ~result, 
        y = ~PL, 
        text = ~texto_hover, 
        color = ~dep, 
        size = ~ROE,
        fill = "black",
        hoverinfo = "text",
        alpha = 0.95) %>% 
  add_markers(sizes = c(1, 100),
              colors = viridis(2)) %>%
  layout(xaxis = list(title = "Lucros / Prejuízos (R$)",
                      type = 'log'),
         yaxis = list(title = "Patrimônio Líquido (R$)", 
                      type = 'log')) %>%
  config(displayModeBar = FALSE)


roe_plotly <- plot_ly(dados_roe, 
        x = ~result, 
        y = ~PL, 
        text = ~texto_hover, 
        color = ~dep, 
        size = ~ROE,
        fill = "black",
        hoverinfo = "text",
        alpha = 0.95) %>% 
  add_markers(sizes = c(1, 100),
              colors = viridis(2)) %>%
  layout(xaxis = list(title = "Lucros / Prejuízos (R$)"),
         yaxis = list(title = "Patrimônio Líquido (R$)")) %>%
  config(displayModeBar = FALSE)

htmlwidgets::saveWidget(partial_bundle(roe_plotly_log), file = "roe_log.html")
htmlwidgets::saveWidget(partial_bundle(roe_plotly), file = "roe.html")

ggplot(dados_roe, aes(x = result, y = PL, size = ROE, fill = dep)) +
  geom_point() +
  scale_fill_viridis_d() +
  tema()
  

mapa_qde <- mapa_dados %>%
  group_by(State) %>%
  summarise(qde = n())

graf_mapa_qde <- ggplot(mapa_qde)+ #%>% filter(seg == "SANEAMENTO")) + 
  geom_sf(aes(fill = qde, geometry = geometry), color = NA) + 
  scale_fill_viridis(direction = -1,
                     option = "magma",
                     na.value="ghostwhite"
                     )+
  labs(title = "Quantidade de empresas estatais em cada estado",
       fill = "Quantidade") +
  tema() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.background = element_blank())


library(ggbeeswarm)

ggplot(dados_empresas, aes(y = PL, x = `Dependência`, color = (PL > 0), size = PL,
                           alpha = 0.5)) +
         #geom_jitter()+
  geom_beeswarm()+
        #geom_quasirandom(varwidth = TRUE, alpha = 0.5, size = 2) +
  scale_y_log10() +
         theme_minimal()



ggplot(dados_empresas, aes(y = ))

ggplot(dados_empresas, aes(x = Segmento))


    
mapa_dados <- mapa %>% 
  inner_join(dados_ibge, c("City" = "CodMun")) %>% 
  rename(pop = `POP EST`,
         catPop = `CLASSE POP`)

# como a área inteira do município é projetada, o mais lógico talvez fosse usar a densidade média no município, e não a população.

mapa <- ggplot(mapa_dados) + 
  geom_sf(aes(fill = pop), color = NA) + 
  scale_fill_viridis(direction = -1,
                     option = "magma",
                     #breaks = c(1e3, 100e3, 10000e3),
                     #trans = "log", #para usar uma escala de log
                     labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) + 
  labs(fill = "População \n(milhões)") +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        legend.position = "none",
        legend.text = element_text(size = 10),
        plot.background = element_blank(),
        panel.background = element_blank())

#render_depth(focallength=100,focus=0.72)

plot_gg(mapa,multicore=TRUE,width=12,height=12,scale=350)
render_camera(fov = 70, zoom = 0.5, theta = 20, phi = 35)
render_camera(fov = 75, zoom = 0.45, theta = 0, phi = 40)
render_camera(fov = 45, zoom = 0.35, theta = 0, phi = 30)
render_camera(fov = 45, zoom = 0.35, theta = 10, phi = 50)
render_camera(fov = 90, zoom = 0.15, theta = 10, phi = 20)

render_camera(fov = 45, zoom = 0.25, theta = 0, phi = 30)
render_camera(fov = 15, zoom = 0.25, theta = 0, phi = 30)
render_camera(fov = 45, zoom = 0.45, theta = 10, phi = 40)
render_camera(fov = 45, zoom = 0.35, theta = 10, phi = 40)
# phi: azimuth
# theta: rotação
# dá para passar vetores de zoom, fov, theta e phi para fazer a câmera passear.
render_snapshot("brasil3d.png")
render_movie("heatmap.mp4")

#
#render_camera()



# gg = ggplot(diamonds, aes(x, depth)) +
#   stat_density_2d(aes(fill = stat(nlevel)), 
#                   geom = "polygon",
#                   n = 100,bins = 10,contour = TRUE) +
#   facet_wrap(clarity~.) +
#   scale_fill_viridis_c(option = "A")
# plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)

# ver:
# https://github.com/kraaijenbrink/warmingstripes-3d/blob/master/animate.r

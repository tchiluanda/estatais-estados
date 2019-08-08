# https://github.com/tylermorganwall/rayshader

# https://www.curso-r.com/blog/2019-02-10-sf-miojo/

# https://www.tylermw.com/3d-ggplots-with-rayshader/


######### TO DO
# * retirar ticks dos gráficos de barra
# * companhia de mineração e a agência de fomento de TO estão como "OUTRO".

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
library(plotly)
library(ggbeeswarm)
library(colorspace)
library(RColorBrewer)

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
      axis.ticks = element_line(size = 0.4),
      axis.ticks.length = unit(.2, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none',
      legend.text = element_text(size = 8, family = "Source Sans Pro"),
      legend.title = element_text(size = 9, family = "Source Sans Pro")
      )
}

tema_barra <- function(){
  tema() +
    theme(
      axis.ticks.y = element_blank()
      )
}

tema_mapa <- function() {
  tema() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 10),
          plot.background = element_blank(),
          panel.background = element_blank())
}

setwd("~/GitHub/estatais-estados")
#load("workspace.RData")

dados_empresas_raw <- read_excel("./dados/Estatais_rev2.xlsx") %>%
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
# mapa %>% as.data.frame() %>% .[c("State", "Region")] %>% write.csv2("reg.csv")

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

graf_mapa_facet <- graf_mapa_comp + facet_wrap(~seg)
ggsave(plot = graf_mapa_facet, "./plots/segmentos_facet.png", width = 9, height = 8, dpi = 300, type = "cairo-png")

## teste labels

graf_mapa_labels <- ggplot(combinacao_est_seg, aes(group = State)) +
  geom_sf(aes(fill = ifelse(qde > 0, seg, NA), geometry = geometry), color = "ghostwhite") + 
  geom_text(aes(label = "Estados com empresas do setor de ", 
                y = 9.5, x = -73.5), 
            color = "dimgrey", check_overlap = TRUE,
            family = "Lora", fontface = "plain", size = 5, 
            hjust = "left") +
  geom_text(aes(label = seg, y = 9.5, x = -50, color = seg), # no chute
            check_overlap = TRUE, family = "Lora", fontface = "bold",
            size = 5, hjust = "left") +
  scale_fill_viridis_d(direction = 1,
                       option = "plasma", na.value = "#EFEFEF") +
  scale_color_viridis_d(direction = 1,
                       option = "plasma", na.value = "#EFEFEF") +
  labs(x = NULL, y = NULL) +
  tema_mapa()

## fim teste

graf_mapa_gif <- graf_mapa_labels + transition_states(states = seg,
                                    transition_length = 1,
                                    state_length = 3) #+
  # labs(title = "Estados que possuem empresas do setor de {closest_state}") +
  # theme(title = element_text(size = 13, face = "plain"))

animate(graf_mapa_gif, nframes = nrow(segmentos)*20, fps = 8, type = "cairo")

anim_save("./gifs/mapa.gif", animation = last_animation())

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
  geom_tile(aes(x = seg, y = reorder(nome, desc(nome)), fill = factor(qde, levels = 1:7)), color = "white") +
  scale_fill_viridis_d(direction = -1, na.value="ghostwhite", breaks = 1:max(combinacao_est_seg$qde, na.rm = T), drop = FALSE) +
  labs(x = NULL, y = NULL,
       fill = "Quantidade",
       title = NULL)+ #"Quantidade de empresas estatais por estado e setor"
  tema() + theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5),
        axis.ticks = element_blank())

# no grafico acima, forcei um factor para a legenda ficar mais bonita. só por isso que repeti todo o código :/

graf_empXsetor_ray <- ggplot(combinacao_est_seg%>%select(-geometry)) +
  geom_tile(aes(x = seg, y = reorder(nome, desc(nome)), fill = qde), color = "white") +
  scale_fill_viridis(direction = -1, na.value="ghostwhite", breaks = 1:max(combinacao_est_seg$qde, na.rm = T)) +
  labs(x = NULL, y = NULL, title = "Quantidade de empresas estatais por estado e setor", fill = "Quantidade") +
  tema() + theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5),
        axis.ticks = element_blank())

ggsave(plot = graf_empXsetor, "./plots/heatmap.png", h = 7.5, w = 6, type = "cairo-png")

#  rayshader --------------------------------------------------------------

plot_gg(graf_empXsetor_ray+theme(legend.position = 'none'),multicore=TRUE,width=5.7,height=8,scale=400)

render_camera(fov = 80, zoom = .65, theta = 0, phi = 90)
# só muda zoom
render_camera(fov = 80, zoom = .55, theta = 0, phi = 90)
# só muda theta
render_camera(fov = 80, zoom = .55, theta = -90, phi = 90)
# so muda phi
render_camera(fov = 80, zoom = .55, theta = -90, phi = 0)

render_camera(fov = 80, zoom = .55, theta = -90, phi = 30) # tirar

render_camera(fov = 80, zoom = .55, theta = -45, phi = 30)

render_camera(fov = 80, zoom = .55, theta = 0, phi = 30)

render_camera(fov = 80, zoom = .55, theta = 0, phi = 0) # tirar

render_camera(fov = 80, zoom = .55, theta = 0, phi = 90) # tirar

render_camera(fov = 80, zoom = .65, theta = 0, phi = 90)

# phi: azimuth
# theta: rotação
# dá para passar vetores de zoom, fov, theta e phi para fazer a câmera passear.

# como gerar os vetores?

# vetores de "check points":
pontos_zoom  <- c(.65, .55, .55, .55, .55, .65)
pontos_theta <- c(0, 0, -90, -90, -45, 0)
pontos_phi   <- c(90, 90, 90, 0, 30, 90)

# parâmetros
qde_frames <- 360

tamanho_int <- round(qde_frames / (length(pontos_zoom)-1),0)
qde_frames <- tamanho_int * (length(pontos_zoom)-1)

# função para gerar vetores
gera_vetor_interpolado <- function(vetor, tamanho_int){
  result <- NULL
  for (i in 1:(length(vetor)-1)) {
    inicio    <- vetor[i]
    fim       <- vetor[i+1]
    intervalo <- fim-inicio
    passo     <- intervalo / tamanho_int
    
    if (inicio == fim) sequencia <- rep(inicio, tamanho_int+1)
    else sequencia <- seq(inicio, fim, by = passo)
    
    #print(paste(inicio, fim, intervalo, passo, length(sequencia)))
    result <- c(result, sequencia[-(tamanho_int+1)])
  }
  return(result)
}

vet_zoom <- gera_vetor_interpolado(pontos_zoom, tamanho_int)
vet_theta <- gera_vetor_interpolado(pontos_theta, tamanho_int)
vet_phi <- gera_vetor_interpolado(pontos_phi, tamanho_int)

render_snapshot("heatmap_perspectiva.png")
render_depth(focallength=40,focus=0.69)
render_movie("heatmap1.mp4", type = "custom", frames = qde_frames, fps = 30,
             phi = vet_phi, theta = vet_theta, zoom = vet_zoom, fov = 80)

#
#render_camera()



# graficos de barra - quantidades -----------------------------------------

qde_empresas_seg <- dados_empresas %>% 
  group_by(seg, dep) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(seg) %>%
  mutate(qde_tot = sum(qde),
         dep = factor(dep, levels = c("Dependente", "Não Dependente", "Não Informado"))) %>%
  filter(!is.na(seg))

vetor_cores_dep <- c("Dependente" = "#f2ac29",
                     "Não Dependente" = "#718c35",
                     "Não Informado" = "#5c4b51")

graf_qde_emp <- 
  ggplot(qde_empresas_seg, aes(x = reorder(seg, qde_tot), y = qde, fill = dep)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = qde, y = qde), 
            vjust = 0.4, position = position_stack(vjust = 0.5, reverse = TRUE),
            family = "Source Sans Pro", size = 3, color = "#ebf2f2") +
  geom_text(aes(label = qde_tot), y = -1,
            vjust = 0.4, check_overlap = TRUE,
            family = "Source Sans Pro", size = 3.5, color = "grey") +  
  coord_flip() +
  scale_fill_manual(values = vetor_cores_dep) +
  #scale_fill_viridis_d() +
  #scale_color_viridis_d() +
  labs(x = NULL, y = NULL, 
       title = NULL, #"Quantidade de empresas por segmento", 
       fill = NULL) +
  tema_barra() + theme(axis.text = element_text(size = 9))

ggsave(plot = graf_qde_emp, "./plots/qde_seg.png", h = 6, w = 5, type = "cairo-png")


qde_empresas_est <- mapa_dados %>% 
  select(-geometry) %>%
  group_by(nome, dep) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(nome) %>%
  mutate(qde_tot = sum(qde),
         dep = factor(dep, levels = c("Dependente", "Não Dependente", "Não Informado"))) %>%
  filter(!is.na(dep))

graf_qde_emp_est <- 
  ggplot(qde_empresas_est, aes(x = reorder(nome, qde_tot), y = qde, fill = dep)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = qde, y = qde), 
            vjust = 0.4, position = position_stack(vjust = 0.5, reverse = TRUE),
            family = "Source Sans Pro", size = 3, color = "#ebf2f2") +
  geom_text(aes(label = qde_tot), y = -.5, 
            vjust = 0.4, check_overlap = TRUE,
            family = "Source Sans Pro", size = 3.5, color = "grey") +  
  coord_flip() +
  scale_fill_manual(values = vetor_cores_dep) +
  #scale_fill_viridis_d() +
  #scale_color_viridis_d() +
  labs(x = NULL, y = NULL, 
       title = NULL, #"Quantidade de empresas por Estado", 
       fill = NULL) +
  tema_barra() + theme(axis.text = element_text(size = 9))

ggsave(plot = graf_qde_emp_est, "./plots/qde_est.png", h = 6.5, w = 5, type = "cairo-png")


# cartogram ---------------------------------------------------------------

# ggplot(qde_empresas_est) + geom_sf(aes(fill = factor(qde)), color = NA) + scale_fill_viridis_d(direction = -1) + tema_mapa()

library(cartogram)
# https://github.com/sjewo/cartogram

mapa_regiao <- get_brmap("Region") 

qde_regiao <- dados_empresas %>%
  group_by(CODUF) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  right_join(mapa, by = c("CODUF" = "State")) %>% # Nota
  group_by(Region) %>%
  summarise(qde = sum(qde))

# Nota: pura preguiça aqui de aproveitar a região do df "mapa".

mapa_cartograma <- mapa_regiao %>% left_join(qde_regiao)
mp_sf <- as_Spatial(mapa_cartograma)
mapa_deform <- cartogram_cont(mp_sf, 'qde', 3)

mp_def <- sf::st_as_sf(mapa_deform)

# testa gif transição entre mapas normal e deformado.

mp_def_join <- mp_def %>%
  mutate(tipo_geometria = "deformada")

mp_nor_join <- mapa_cartograma %>%
  mutate(tipo_geometria = "normal")

mp_duplo <- rbind(mp_def_join, mp_nor_join)

mapa_duplo_gif <- ggplot(data = mp_duplo, aes(geometry = geometry, fill = Region, group = Region)) +
  geom_sf(color = NA) +
  geom_sf_label(aes(label = ifelse(tipo_geometria == "deformada", qde, NA)),
                size = 6,    
                color = "grey20",
                family = "Source Sans Pro",
                fill = "ghostwhite", label.size = 0,
                label.r = unit(0.67, 'lines'),
                label.padding = unit(0.35, "lines")) +
  ease_aes("cubic-in-out") +
  scale_fill_viridis(option = "viridis", direction = -1) +
  labs(x = NULL, y = NULL) +
  tema_mapa() +
  transition_states(states = tipo_geometria,
                    transition_length = 1,
                    state_length = 2)

animate(mapa_duplo_gif, fps = 8, type = "cairo")

anim_save("./gifs/cartograma.gif", animation = last_animation())

# # tentativa de usar um gather para ter as duas geometrias.
# 
# mapa_regiao_duplo <- mp_def %>%
#   rename(geometry_deformada = geometry) %>%
#   mutate(geometry_normal = mapa_cartograma$geometry) #%>%
#   #gather(geometry_deformada, geometry_normal, key = "tipo_geometria", value = "geometrias")
# 
# mp_duplo_df <- as.data.frame(mapa_regiao_duplo) %>%
#   gather(geometry_deformada, geometry_normal, key = "tipo_geometria", value = "geometrias")
# 
# ggplot(mp_duplo_df %>% filter(tipo_geometria == "geometry_normal")) +
#   geom_sf(aes(geometry = geometrias))

# plot(mapa_deform)

cartograma <- ggplot(mp_def, aes(geometry = geometry)) + 
  geom_sf(aes(fill = qde), color = NA) +
  geom_sf_label(aes(label = qde), #color = qde), 
                color = "grey20",
                family = "Source Sans Pro",
                fill = "ghostwhite", label.size = 0, 
                label.r = unit(0.67, 'lines'),
                label.padding = unit(0.35, "lines")) +
  scale_fill_viridis(option = "viridis", direction = -1) +
  #scale_color_viridis(option = "viridis", direction = -1, guides) +
  labs(fill = NULL, #"Quantidade", 
       title = NULL, #"O Brasil conforme a quantidade de estatais estaduais por Região", 
       x = NULL, y = NULL) +
  #guides(color = "none") +
  tema_mapa()# + theme(legend.position = 'left')



mapa_regiao_normal <- ggplot(mapa_cartograma, aes(geometry = geometry)) + 
  geom_sf(aes(fill = qde), color = NA) +
  geom_sf_label(aes(label = qde), #color = qde), 
                color = "grey20",
                family = "Source Sans Pro",
                fill = "ghostwhite", label.size = 0, 
                label.r = unit(0.67, 'lines'),
                label.padding = unit(0.35, "lines")) +
  scale_fill_viridis(option = "viridis", direction = -1) +
  #scale_color_viridis(option = "viridis", direction = -1, guides) +
  labs(fill = NULL, title = NULL, x = NULL, y = NULL) +
  #guides(color = "none") +
  tema_mapa()

ggsave(plot = cartograma, file = "./plots/cartograma.png", type = "cairo-png", width = 7, height = 7)
ggsave(plot = mapa_regiao_normal, file = "./plots/cartograma_normal.png", type = "cairo-png", width = 7, height = 7)


# ROE - beeswarm--------------------------------------------------------------

library(plotly)

qde_empresas_PL_neg <- length(which(dados_empresas$PL<=0))

top_segs <- dados_qde %>% 
  group_by(seg) %>% 
  summarise(qde = sum(qde)) %>% 
  arrange(desc(qde)) %>%
  filter(qde >= 10 & seg != "OUTRO")

principais_segmentos <- top_segs$seg

dados_roe <- dados_empresas %>%
  filter(PL > 0 & dep != "Não Informado") %>%
  mutate(result = as.numeric(`Lucros / Prejuízos`),
         ROE = result / PL,
         PL_formatado = format(PL, big.mark = ".", decimal.mark = ',', scientific = FALSE)) %>%
  filter(!is.na(ROE)) %>%
  mutate(Empresa = paste0(emp, ' (', Estado, ')\n',
                              'Dependência: ', dep, '\n',
                              'Setor: ', seg, '\n',
                              'PL: R$ ', PL_formatado, '\n',
                              'Lucros / Prejuízos no ano: R$ ', format(result, big.mark = '.', decimal.mark = ","), '\n',
                              'ROE: ', percent(round(ROE,4))),
         cat_ROE = cut(ROE, breaks = c(-Inf, -0.5, 0, 0.5, Inf), 
                       labels = c("bem_neg", "neg", "pos", "bem_pos")),
         seg_principais = ifelse(seg %in% principais_segmentos, seg, "Demais"))

summary(dados_roe$ROE)[c("Min.", "Max.")]

seq(summary(dados_roe$ROE)[c("Min.")], 
    summary(dados_roe$ROE)[c("Max.")],
    by = 0.5)
    

define_breaks <- function(limits) {
  seq(round(limits[1],0), round(limits[2],0), by = 0.5)
  }

cor_anotacoes <- "#3b7302"

cores_escala <- c("bem_neg" = "#912849",
                  "neg"     = "#ff7270",
                  "pos"     = "#91c1cc",
                  "bem_pos" = "#375e8b")

sumario_roe <- dados_roe %>%
  group_by(cat_ROE, dep) %>%
  summarise(qde = n()) %>%
  group_by(dep) %>%
  mutate(pct_qde = percent(qde/sum(qde))) %>%
  ungroup() %>%
  mutate(y = case_when(cat_ROE == "bem_neg" ~ -0.75,
                  cat_ROE == "neg" ~ -0.25,
                  cat_ROE == "pos" ~  0.25,
                  cat_ROE == "bem_pos" ~  0.75))

dados_roe %>% filter(ROE > 2 | ROE < -2) %>% select(emp, Estado, ROE)

# dados_roe %>% ggplot() + 
#   #geom_histogram(aes(ROE), bins = 100) +
#   geom_density(aes(ROE, fill = dep)) +
#   scale_x_continuous(labels = percent) +
#   tema()

# esse sim 
roe <- ggplot(dados_roe, aes(y = ROE, color = cat_ROE, x = dep, 
                             label = Empresa)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "Gainsboro") +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "Gainsboro") +
  geom_hline(yintercept = -0.5, linetype = "dotted", color = "Gainsboro") +
  geom_beeswarm() + #aes(size = PL), 
  scale_color_manual(values = cores_escala) +
  annotate("rect", xmin = 0, xmax = 1.5, ymin = -0.5, ymax = 0, alpha = 0.2, fill = "khaki") +
  annotate("rect", xmin = 1.5, xmax = 2.9, ymin = 0, ymax = 0.5, alpha = 0.2, fill = "khaki") +
  geom_text(data = sumario_roe, 
            aes(y = ifelse(dep == "Dependente", y, NA),
                label = paste0(pct_qde, ' das \nDependentes'),
                color = cat_ROE),
            x = 0.6, # 0.8 para estático
            hjust = "right", vjust = "center", family = "Lora", size = 3) +
  geom_text(data = sumario_roe, 
            aes(y = ifelse(dep == "Não Dependente", y, NA),
                label = paste0(pct_qde, ' das não\nDependentes'),
                color = cat_ROE),
            x = 2.6, # 2.4 para estático
            hjust = "left", vjust = "center", family = "Lora", size = 3) +
  labs(title = "Distribuição do ROE das empresas do estados", x = NULL, y = NULL,
       subtitle = "Mais de 60% das dependentes têm ROE negativo, mais de 60% das não dependentes têm ROE positivo",
       caption = "Não inclui a Agência Goiana de Habitação (GO), a Empresa Paraibana de Turismo S/A (PB) e a Companhia de Desenvolvimento\n Rodoviário e Terminais do RJ, todas com ROE abaixo de -200%, além de outras 50 empresas com Patrimônio Líquido negativo.") +
  scale_y_continuous(labels = percent, breaks = define_breaks, limits = c(-2,2)) +
  tema()

ggsave(plot = roe, "roe.png", h = 7, w = 10, type = "cairo-png")

# teste facet

roe_facet <- ggplot(dados_roe %>% filter(dep != "Não Informado"), aes(y = ROE, color = cat_ROE, x = dep, 
                             label = Empresa)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "Gainsboro") +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "Gainsboro") +
  geom_hline(yintercept = -0.5, linetype = "dotted", color = "Gainsboro") +
  geom_beeswarm() + #aes(size = PL), 
  scale_color_manual(values = cores_escala) +
  labs(title = "Distribuição do ROE das empresas do estados", x = NULL, y = NULL,
       subtitle = "Detalhando por setor",
       caption = "Não inclui a Agência Goiana de Habitação (GO), a Empresa Paraibana de Turismo S/A (PB) e a Companhia de Desenvolvimento\n Rodoviário e Terminais do RJ, todas com ROE abaixo de -200%, além de outras 50 empresas com Patrimônio Líquido negativo.") +
  scale_y_continuous(labels = percent, limits = c(-2,2), breaks = c(-0.5, 0, 0.5)) +
  tema() + facet_wrap(~seg_principais)

ggsave(plot = roe_facet, "roe_facet.png", h = 7, w = 10, type = "cairo-png")


# para plotly, copiei o código e tirei as anotações

roe_plotly <- ggplot(dados_roe, aes(y = ROE, color = cat_ROE, x = dep, 
                             label = Empresa)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "Gainsboro") +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "Gainsboro") +
  geom_hline(yintercept = -0.5, linetype = "dotted", color = "Gainsboro") +
  geom_beeswarm() + #aes(size = PL), 
  scale_color_manual(values = cores_escala) +
  geom_text(data = sumario_roe, 
            aes(y = ifelse(dep == "Dependente", y, NA),
                label = paste0(pct_qde, ' das \nDependentes'),
                color = cat_ROE),
            x = 0.6, # 0.8 para estático
            hjust = "right", vjust = "center", family = "Lora", size = 3) +
  geom_text(data = sumario_roe, 
            aes(y = ifelse(dep == "Não Dependente", y, NA),
                label = paste0(pct_qde, ' das não\nDependentes'),
                color = cat_ROE),
            x = 2.6, # 2.4 para estático
            hjust = "left", vjust = "center", family = "Lora", size = 3) +
  labs(title = "Distribuição do ROE das empresas do estados", x = NULL, y = NULL,
       subtitle = "Mais de 60% das dependentes têm ROE negativo, mais de 60% das não dependentes têm ROE positivo",
       caption = "Não inclui a Agência Goiana de Habitação (GO), a Empresa Paraibana de Turismo S/A (PB) e a Companhia de Desenvolvimento\n Rodoviário e Terminais do RJ, todas com ROE abaixo de -200%, além de outras 50 empresas com Patrimônio Líquido negativo.") +
  scale_y_continuous(labels = percent, breaks = define_breaks, limits = c(-2,2)) +
  tema()

roe_bee <- ggplotly(roe_plotly, tooltip = 'Empresa') %>%
  config(displayModeBar = FALSE)

htmlwidgets::saveWidget(partial_bundle(roe_bee), file = "roe_bee.html")


# ggplot(dados_roe, aes(y = ROE, color = cat_ROE, x = seg_principais, shape = dep)) + 
#   scale_color_manual(values = cores_escala) +
#   geom_jitter() +
#   tema()


# ROE - sumário -----------------------------------------------------------

dados_roe_sum <- dados_roe %>%
  filter(dep != "Não Informado") %>%
  mutate(ROE_pos_neg = ifelse(ROE > 0, "ROE Positivo", "ROE Negativo")) %>%
  group_by(seg_principais, dep, ROE_pos_neg) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(seg_principais, dep) %>%
  mutate(qde_seg_dep = sum(qde),
         pct = qde/qde_seg_dep)
  

roe_sumario <- ggplot(dados_roe_sum, aes(y = qde, x = seg_principais, fill = ROE_pos_neg)) + 
  geom_col(width = 0.5) + 
  geom_text(aes(label = qde), vjust = 0.35, position = position_stack(vjust = 0.5), family = "Source Sans Pro", size = 3, color = "#ebf2f2") +
  geom_text(aes(label = percent(pct), 
                y = ifelse(ROE_pos_neg == "ROE Positivo", qde_seg_dep + 0.5, NA)),
            vjust = 0.4, hjust = "left", family = "Source Sans Pro", size = 3, color = cores_escala[4]) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .15))) +
  coord_flip() + 
  tema_barra() + 
  theme(legend.position = "bottom", axis.line.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Quantitativo de empresas com ROE positivo ou negativo por setor",
       subtitle = "O percentual indica a proporção de empresas com ROE positivo no setor, por tipo de dependência") +
  scale_fill_manual(values = c("ROE Negativo" = "#912849", 
                               "ROE Positivo" = "#375e8b")) +
  facet_wrap(~dep)

ggsave(plot = roe_sumario, "roe_sumario.png", h = 7, w = 9, type = "cairo-png")

ggplot(dados_roe_sum, aes(x = ROE, 
                          y = reorder(seg_principais, ROE_med), 
                          color = cat_ROE)) + 
  # annotate("rect", xmin = -2, xmax = -0.5, 
  #          ymin = -Inf, ymax = Inf, fill = cores_escala[1], alpha = 0.1) +
  # annotate("rect", xmin = -0.5, xmax = 0, 
  #          ymin = -Inf, ymax = Inf, fill = cores_escala[2], alpha = 0.1) +
  # annotate("rect", xmin = 0, xmax = 0.5, 
  #          ymin = -Inf, ymax = Inf, fill = cores_escala[3], alpha = 0.1) +
  # annotate("rect", xmin = 0.5, xmax = Inf, 
  #          ymin = -Inf, ymax = Inf, fill = cores_escala[4], alpha = 0.1) +
  geom_tile(aes(x = ROE_med), color = "grey") +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = cores_escala) +
  scale_x_continuous(limits = c(-2,2)) +
  tema_barra() + facet_wrap(~dep)

# ROE - scatter ------------------------------------------------------------------

roe_plotly_log <- plot_ly(dados_roe, 
                          x = ~result, 
                          y = ~PL, 
                          text = ~Empresa, 
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
                      text = ~Empresa, 
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


# mapa resultado----------------------------------------------------------------

mapa_res <- mapa_dados %>% group_by(nome) %>% summarise(res = sum(`Resultado para o Estado Acionista`, na.rm = TRUE))

mapa_res_graf <- ggplot(mapa_res) + 
  geom_sf(aes(fill = -res), color = NA) + 
  scale_fill_continuous_sequential(palette = "Peach", 
                                   rev = TRUE, 
                                   labels = function(x){
                                     format(round(x/1e6, 0), big.mark = ".",
                                            decimal.mark = ",")}) + 
  labs(fill = "Resultado negativo\n(R$ milhões)", title = "Resultado Consolidado das estatais para o Estado") +
  tema_mapa() + 
  theme(legend.position = 'left')

ggsave(plot = mapa_res_graf, file = "mapa_res.png", type = "cairo-png", width = 8, height = 7)





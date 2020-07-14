###########################################################################
## Neutralidade da moeda animada                                         ##
## Autor: Guilherme Viegas                                               ##
## https://www.linkedin.com/in/guilherme-viegas-1b5b0495/                ##
###########################################################################

# Setup -------------------------------------------------------------------

rm(list = ls())
gc(verbose = T, reset = T)

# Libraries ---------------------------------------------------------------

if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gganimate)){install.packages("gganimate")}
if(!require(magick)){install.packages("magick")}

# IS-LM -------------------------------------------------------------------

# Dominio da função (Possíveis valores no eixo das abcissas)
x <- c(0:1000)

# Imagem das funções IS (Possíveis valores no eixo das ordenadas)
is1 <- c(-0.8 * x + 900)
is2 <- c(-0.8 * x + 900)
is3 <- c(-0.8 * x + 900)

# Imagem das funções LM (Possíveis valores no eixo das ordenadas)
lm1 <- c(0.9 * x + 100)
lm2 <- c(0.9 * x - 100)
lm3 <- c(0.9 * x + 100)

# Matriz de coeficientes IS-LM 1
m1coef_islm <- matrix(
  c(-1, coef(lm(is1 ~ x))[[2]], 
    -1, coef(lm(lm1 ~ x))[[2]]), 
  nrow = 2, ncol = 2, byrow = T)

# Matriz de constantes IS-LM 1
m1const_islm <- matrix(
  c(-coef(lm(is1 ~ x))[[1]], 
    -coef(lm(lm1 ~ x))[[1]]), 
  nrow = 2, ncol = 1, byrow = T)

# Ponto de Equilíbrio IS-LM 1
ep1_islm <- solve(a = m1coef_islm, b = m1const_islm)

# Matriz de coeficientes IS-LM 2
m2coef_islm <- matrix(
  c(-1, coef(lm(is2 ~ x))[[2]], 
    -1, coef(lm(lm2 ~ x))[[2]]), 
  nrow = 2, ncol = 2, byrow = T)

# Matriz de constantes IS-LM 2
m2const_islm <- matrix(
  c(-coef(lm(is2 ~ x))[[1]], 
    -coef(lm(lm2 ~ x))[[1]]), 
  nrow = 2, ncol = 1, byrow = T)

# Ponto de Equilíbrio IS-LM 2
ep2_islm <- solve(m2coef_islm, m2const_islm)

# Matriz de coeficientes IS-LM 3
m3coef_islm <- matrix(
  c(-1, coef(lm(is3 ~ x))[[2]], 
    -1, coef(lm(lm3 ~ x))[[2]]), 
  nrow = 2, ncol = 2, byrow = T)

# Matriz de constantes IS-LM 3
m3const_islm <- matrix(
  c(-coef(lm(is3 ~ x))[[1]], 
    -coef(lm(lm3 ~ x))[[1]]), 
  nrow = 2, ncol = 1, byrow = T)

# Ponto de Equilíbrio IS-LM
ep3_islm <- solve(m3coef_islm, m3const_islm)

# Tabela referência IS-LM
ref_islm <- data.frame(
  is_x = c(x[1], x[1], x[1]),
  is_xend = c(x[1000], x[1000], x[1000]),
  is_y = c(is1[1], is2[1], is3[1]),
  is_yend = c(is1[1000], is2[1000], is3[1000]),
  lm_x = c(x[1], x[1], x[1]),
  lm_xend = c(x[1000], x[1000], x[1000]),
  lm_y = c(lm1[1], lm2[1], lm3[1]),
  lm_yend = c(lm1[1000], lm2[1000], lm3[1000]),
  lm_s_x = c(x[1], x[1], x[1]),
  lm_s_xend = c(x[1000], x[1000], x[1000]),
  lm_s_y1 = c(lm1[1], lm1[1], lm1[1]),
  lm_s_yend1 = c(lm1[1000], lm1[1000], lm1[1000]),
  lm_s_y2 = c(lm1[1], lm2[1], lm2[1]),
  lm_s_yend2 = c(lm1[1000], lm2[1000], lm2[1000]),
  ep_y = c(ep1_islm[1], ep2_islm[1], ep3_islm[1]),
  ep_x = c(ep1_islm[2], ep2_islm[2], ep3_islm[2]),
  frames = c(1, 2, 3),
  caption_time = c("Curto prazo", "Médio prazo", "Médio prazo")
)

# attach(ref_islm)

# Gráfico IS-LM
islm <- ggplot(ref_islm)+
  geom_segment(aes(x = lm_s_x, xend = lm_s_xend, y = lm_s_y1, yend = lm_s_yend1), size = 1.6, color = "gray")+
  geom_segment(aes(x = lm_s_x, xend = lm_s_xend, y = lm_s_y2, yend = lm_s_yend2), size = 1.6, color = "gray")+
  geom_segment(aes(x = is_x, xend = is_xend, y = is_y, yend = is_yend), size = 1.6)+
  geom_segment(aes(x = lm_x, xend = lm_xend, y = lm_y, yend = lm_yend), size = 1.6)+
  geom_segment(aes(x = 0, xend = 1000, y = 0,yend = 0), size = 1.1)+
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1000), size = 1.1)+
  geom_segment(aes(x = ep_x, xend = ep_x, y = 0, yend = ep_y), linetype = 2)+
  geom_segment(aes(x = 0, xend = ep_x, y = ep_y, yend = ep_y), linetype = 2)+
  geom_text(aes(x = c(ep_x +300), y = c(ep_y + 200)), label = "LM", size = 8)+
  geom_text(aes(x = 780, y = 200), label = "IS", size = 8)+
  coord_cartesian(ylim = c(0, 1000), expand = FALSE, xlim = c(0, 1000))+
  theme_classic()+
  labs(y = "Juros real, r",
       x = "Renda, Y",
       subtitle = '')+
  theme(axis.title = element_text(size = 19, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  transition_reveal(frames)

# Renderiza a animação
islm_gif <- animate(islm, width = 600, height = 360)

# Lê a animação
islm_mgif <- image_read(islm_gif)
islm_mgif

# OA-DA -------------------------------------------------------------------

# Dominio da função (Possíveis valores no eixo das abcissas)
x <- c(0:1000)

# Imagem das funções DA (Possíveis valores no eixo das ordenadas)
da1 <- c(-0.8 * x +900)
da2 <- c(-0.8 * x +1100)
da3 <- c(-0.8 * x +1100)

# Imagem das funções OA (Possíveis valores no eixo das ordenadas)
oa1 <- c(0.9 * x +100)
oa2 <- c(0.9 * x +100)
oa3 <- c(0.9 * x +300)

# Matriz de coeficientes OA-DA 1
m1coef_oada <- matrix(
  c(-1, coef(lm(da1 ~ x))[[2]], 
    -1, coef(lm(oa1 ~ x))[[2]]), 
  nrow = 2, ncol = 2, byrow = T)

# Matriz de constantes OA-DA1
m1const_oada <- matrix(
  c(-coef(lm(da1 ~ x))[[1]], 
    -coef(lm(oa1 ~ x))[[1]]), 
  nrow = 2, ncol = 1, byrow = T)

# Ponto de Equilíbrio OA-DA 1
ep1_oada <- solve(m1coef_oada, m1const_oada)

# Matriz de coeficientes OA-DA 2
m2coef_oada <- matrix(
  c(-1, coef(lm(da2 ~ x))[[2]], 
    -1, coef(lm(oa2 ~ x))[[2]]), 
  nrow = 2, ncol = 2, byrow = T)

# Matriz de contantes OA-DA 2
m2const_oada <- matrix(
  c(-coef(lm(da2 ~ x))[[1]], 
    -coef(lm(oa2 ~ x))[[1]]), 
  nrow = 2, ncol = 1, byrow = T)

# Ponto de Equilíbrio OA-DA 2
ep2_oada <- solve(m2coef_oada, m2const_oada)

# Matriz de coeficientes OA-DA 3
m3coef_oada <- matrix(
  c(-1, coef(lm(da3 ~ x))[[2]], 
    -1, coef(lm(oa3 ~ x))[[2]]), 
  nrow = 2, ncol = 2, byrow = T)

# Matriz de contantes OA-DA 3
m3const_oada <- matrix(
  c(-coef(lm(da3 ~ x))[[1]], 
    -coef(lm(oa3 ~ x))[[1]]), 
  nrow = 2, ncol = 1, byrow = T)

# Ponto de Equilíbrio OA-DA 3
ep3_oada <- solve(m3coef_oada, m3const_oada)

# Tabela referência OA-DA
ref_oada <- data.frame(
  da_x = c(x[1], x[1], x[1]),
  da_xend = c(x[1000], x[1000], x[1000]),
  da_y = c(da1[1], da2[1], da3[1]),
  da_yend = c(da1[1000], da2[1000], da3[1000]),
  da_s_y = c(da1[1], da1[1], da1[1]),
  da_s_yend = c(da1[1000], da1[1000], da1[1000]),
  oa_x = c(x[1], x[1], x[1]),
  oa_xend = c(x[1000], x[1000], x[1000]),
  oa_y = c(oa1[1], oa2[1], oa3[1]),
  oa_yend = c(oa1[1000], oa2[1000], oa3[1000]),
  oa_s_x = c(x[1], x[1], x[1]),
  oa_s_xend = c(x[1000], x[1000], x[1000]),
  oa_s_y = c(oa1[1], oa2[1], oa2[1]),
  oa_s_yend = c(oa1[1000], oa2[1000], oa2[1000]),
  ep_y = c(ep1_oada[1], ep2_oada[1], ep3_oada[1]),
  ep_x = c(ep1_oada[2], ep2_oada[2], ep3_oada[2]),
  frames = c(1, 2, 3),
  caption_time = c("Curto prazo", "Médio prazo", "Médio prazo")
)

# attach(ref_oada)

# Gráfico OA-DA
oada <- ggplot(ref_oada)+
  geom_segment(aes(x = oa_s_x, xend = oa_s_xend, y = oa_s_y, yend = oa_s_yend), size = 1.6, color = "gray")+
  geom_segment(aes(x = da_x, xend = da_xend, y = da_s_y, yend = da_s_yend), size = 1.6, color = "gray")+
  geom_segment(aes(x = da_x, xend = da_xend, y = da_y, yend = da_yend), size = 1.6)+
  geom_segment(aes(x = oa_x, xend = oa_xend, y = oa_y, yend = oa_yend), size = 1.6)+
  geom_segment(aes(x = 0, xend = 1000, y = 0,yend = 0), size = 1.1)+
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1000), size = 1.1)+
  geom_segment(aes(x = ep_x, xend = ep_x, y = 0, yend = 1000), linetype = 2)+
  geom_segment(aes(x = 0, xend = ep_x, y = ep_y, yend = ep_y), linetype = 2)+
  geom_text(aes(x = c(900, 900, 812), y = c(830, 830, 930)), label = "OA", size = 8)+
  geom_text(aes(x = c(740, 790, 790), y = c(da_yend + 100)), label = "DA", size = 8)+
  coord_cartesian(ylim = c(0, 1000), expand = FALSE, xlim = c(0, 1000))+
  theme_classic()+
  labs(y = "Nível de preços, P",
       x = "Renda, Y")+
  theme(axis.title = element_text(size = 19, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  transition_reveal(frames)

# Renderiza a animação
oada_gif <- animate(oada, width = 600, height = 360)

# Lê a animação
oada_mgif <- image_read(oada_gif)
oada_mgif

# MD-MS -------------------------------------------------------------------

# Dominio da função (Possíveis valores no eixo das abcissas)
x <- c(0:1000)

# Valores para a função MS
ms_xs <- ref_islm$ep_x
ms_xe <- ref_islm$ep_x
ms_ys <- c(0, 0, 0)
ms_ye <- c(1000, 1000, 1000)
ms_xs_s1 <- c(ref_islm$ep_x[1], ref_islm$ep_x[1], ref_islm$ep_x[1])
ms_xe_s1 <- c(ref_islm$ep_x[1], ref_islm$ep_x[1], ref_islm$ep_x[1])
ms_xs_s2 <- c(ref_islm$ep_x[1], ref_islm$ep_x[2], ref_islm$ep_x[2])
ms_xe_s2 <- c(ref_islm$ep_x[1], ref_islm$ep_x[2], ref_islm$ep_x[2])

# Imagem das funções MD (Possíveis valores no eixo das ordenadas)
md1 <- c(-0.8 * x + 900)
md2 <- c(-0.8 * x + 900)
md3 <- c(-0.8 * x + 900)

# Preço de equilíbrio entre as funções MD e MS
ype <- c(-0.8 * ms_xs + 900)

# Tabela referência MD-MS
ref_mdms <- data.frame(
  md_x = c(x[1], x[1], x[1]),
  md_xend = c(x[1000], x[1000], x[1000]),
  md_y = c(md1[1], md2[1], md3[1]),
  md_yend = c(md1[1000], md2[1000], md3[1000]),
  ms_xs = ms_xs,
  ms_xe = ms_xe,
  ms_ys = ms_ys,
  ms_ye = ms_ye,
  ms_xs_s1 = ms_xs_s1,
  ms_xe_s1 = ms_xe_s1,
  ms_xs_s2 = ms_xs_s2,
  ms_xe_s2 = ms_xe_s2,
  ype = ype,
  frames = c(1, 2, 3),
  caption_time = c("Curto prazo", "Médio prazo", "Médio prazo")
)

# attach(ref_mdms)

# Gráfico MD-MS
mdms <- ggplot(ref_mdms)+
  geom_segment(aes(x = ms_xs_s1, xend = ms_xe_s1, y = ms_ys, yend = ms_ye), size = 1.6, color = "gray")+
  geom_segment(aes(x = ms_xs_s2, xend = ms_xe_s2, y = ms_ys, yend = ms_ye), size = 1.6, color = "gray")+
  geom_segment(aes(x = md_x, xend = md_xend, y = md_y, yend = md_yend), size = 1.6)+
  geom_segment(aes(x = ms_xs, xend = ms_xe, y = ms_ys, yend = ms_ye), size = 1.6)+
  geom_point(aes(x=ms_xs, y=ype), size = 5)+
  geom_segment(aes(x = 0, xend = 1000, y = 0,yend = 0), size = 1.1)+
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1000), size = 1.1)+
  geom_segment(aes(x = 0, xend = 1000, y = ype, yend = ype), linetype = 2)+
  geom_text(aes(x = c(ms_xs +60), y = c(ype + 300)), label = "MS", size = 8)+
  geom_text(aes(x = 100, y = 700), label = "MD", size = 8)+
  coord_cartesian(ylim = c(0, 1000), expand = FALSE, xlim = c(0, 1000))+
  theme_classic()+
  labs(y = "Juros real, r",
       x = "Ms, Md",
       subtitle = '')+
  theme(axis.title = element_text(size = 19, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  transition_reveal(frames)

# Renderiza a animação
mdms_gif <- animate(mdms, width = 600, height = 360)

# Lê a animação
mdms_mgif <- image_read(mdms_gif)
mdms_mgif

# Title -------------------------------------------------------------------

# Tabela referência Title
ref_tt <- data.frame(
  ttx1 = rep(500, 3),
  tty1 = rep(700, 3),
  ttx2 = rep(260, 3),
  tty2 = rep(50, 3),
  ttx3 = rep(500, 3),
  tty3 = rep(500, 3),
  frames = c(1, 2, 3),
  caption_time = c("Curto prazo", "Médio prazo", "Médio prazo")
)

# attach(ref_tt)

# Gráfico Title
tt <- ggplot(ref_tt)+
  geom_text(aes(x = ttx1, y = tty1), label = "Neutralidade da Moeda", size = 16)+
  geom_text(aes(x = ttx2, y = tty2), label = "Guilherme Viegas", size = 8)+
  geom_text(aes(x = ttx3, y = tty3, label = "Animado"), size = 8)+
  coord_cartesian(ylim = c(0, 1000), expand = FALSE, xlim = c(0, 1000))+
  theme_void()+
  labs(subtitle = "",
       x = "",
       y = "",
       caption = "{frame}/{nframes}")+
  theme(axis.title = element_text(size = 19, face = "bold"),
        plot.caption = element_text(size = 13, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  transition_reveal(frames)

# Renderiza a animação
tt_gif <- animate(tt, width = 600, height = 360)

# Lê a animação
tt_mgif <- image_read(tt_gif)
tt_mgif

# Cleaning up -------------------------------------------------------------

rm(list=setdiff(ls(), c("islm_mgif", "oada_mgif", "mdms_mgif","tt_mgif")))

# Joining the gifs --------------------------------------------------------

# Juntando MD-MS e o título no mesmo plot
mdms_tt_gif <- image_append(c(mdms_mgif[1], tt_mgif[1]), stack = TRUE)
for(i in 2:100){
  combined <- image_append(c(mdms_mgif[i], tt_mgif[i]), stack = TRUE)
  mdms_tt_gif <- c(mdms_tt_gif, combined)
}; rm(combined)
mdms_tt_gif

# Juntando IS-LM e OA-DA no mesmo plot 
islm_oada_gif <- magick::image_append(c(islm_mgif[1], oada_mgif[1]), stack = TRUE)
for(i in 2:100){
  combined <- image_append(c(islm_mgif[i], oada_mgif[i]), stack = TRUE)
  islm_oada_gif <- c(islm_oada_gif, combined)
}; rm(combined)
islm_oada_gif

# Juntando tudo num mesmo plot
mdmstt_islmoada_gif <- magick::image_append(c(mdms_tt_gif[1], islm_oada_gif[1]), stack = FALSE)
for(i in 2:100){
  combined <- image_append(c(mdms_tt_gif[i], islm_oada_gif[i]), stack = FALSE)
  mdmstt_islmoada_gif <- c(mdmstt_islmoada_gif, combined)
}; rm(combined)
mdmstt_islmoada_gif

# save_animation(mdmstt_islmoada_gif, "Neutralidade_da_moeda_gif.gif")


# Bugs --------------------------------------------------------------------

# Possivelmente vc precisará de mais RAM disponível para o pacote {magick}, espero que este link ajude.
# https://github.com/phw/peek/issues/112
# Vc deve alterar a seguinte linha de código'<policy domain="resource" name="disk" value="1GiB"/>' no arquivo /etc/ImageMagick-6/policy.xml
# Eu recomendo apliciar como default 8GiB



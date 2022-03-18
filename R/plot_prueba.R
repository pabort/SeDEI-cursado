##--------------------------------------------------------------------------
## Gráfico dona - Participación estudiantes con actuación
##--------------------------------------------------------------------------

# definir colores (Cba y resto del pais)
pal_col <- c("#B50E1A", "#FED105")
carreras <- c('Ciencias Económicas', 'Contador Público', 'Licenciatura en Administración', 'Licenciatura en Economía')

f <- list(
  size = 20,
  family = 'sans-serif')

# Valores
table1 <- df_cursadas %>% 
  mutate(resultado_desc2 = if_else(resultado_desc == "Sin Actuación", "Sin actuación", "Con actuación")) %>% 
  filter(propuesta_formativa_desc %in% carreras) %>% 
  group_by(anio_academico, periodo_lectivo, resultado_desc2) %>% 
  summarise(cantidad = sum(cantidad_alumnos)) %>% 
  mutate(time = paste(anio_academico, periodo_lectivo),
         prop_actuacion = cantidad/sum(cantidad)*100)

# 
periodos <- unique(table1$time)
plot_lst <- vector("list", length = length(periodos) - 1)


m <- list(
  l = 5,
  r = 5,
  b = 40,
  t = 40,
  pad = 4
)


for (i in periodos[periodos!="2019 NIVELACION"]) {
  temp = table1[table1$time == i, c("resultado_desc2", "cantidad")]
  
fig <- table1 %>% 
    filter(periodo_lectivo == '11 - SEGUNDO SEMESTRE', anio_academico == 2019) %>% 
    plot_ly(width = 440, height = 440,
                          labels = ~resultado_desc2, values = ~cantidad,
                          text = ~paste(format_n(cantidad), 'inscripciones'),
                          textinfo = 'label+percent',
                          hoverinfo = 'label+text',
                          marker = list(colors = pal_col,
                                        line = list(color = '#FFFFFF', width = 1)),
                          rotation = 45,
            showlegend = FALSE) %>%
  add_pie(hole = 0.6, domain = list(row = 0, column = 0))
  

temp <- table1 %>% 
  filter(periodo_lectivo == '11 - SEGUNDO SEMESTRE', anio_academico == 2020)

fig <- fig %>% 
  add_pie(data = temp,
          labels = ~resultado_desc2, values = ~cantidad,
          text = ~paste(format_n(cantidad), 'inscripciones'),
          textinfo = 'label+percent',
          hoverinfo = 'label+text',
          marker = list(colors = pal_col,
                        line = list(color = '#FFFFFF', width = 1)),
          rotation = 45,
          showlegend = FALSE,
          hole = 0.6, domain = list(row = 0, column = 1))
  
fig <- fig %>% layout(title = "Pie Charts with Subplots", showlegend = F,
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
  add_annotations(x=1/4, y=0.5,text = paste(format_n(50000), '\n inscripciones'), "showarrow"=F,
                  xanchor='center', yanchor='center', xshift=0, yshift=0) %>%  
  add_annotations(x=3/4, y=0.5,text = paste(format_n(35000), '\n inscripciones'), "showarrow"=F,
                  xanchor='center', yanchor='center', xshift=0, yshift=0)
  
  
fig <- fig %>% layout(separators = ',',
                        showlegend = F,
                        #autosize = T,
                        font = f,
                        annotations=list(text = paste(format_n(sum(temp$cantidad)), '\n inscripciones'), "showarrow"=F),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        hoverlabel = list(font=list(size=20)),
                        autosize = F, margin = m
  ) %>%
    config(displayModeBar = F)
  fig <- fig %>% 
    add_annotations(x = 1, y = -0.1, text = "Fuente: SeDEI en base a datos de Guaraní", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size =14, color="gray"))
  plot_lst[[i]] <- fig
}

# plot_lst[['2021 11 - SEGUNDO SEMESTRE']]
# plot_lst[['2019 11 - CURSOS VERANO']]


# ------------------------------------------------------------------------------
# Plot para informe pdf
# ------------------------------------------------------------------------------

# requiere source(file = "R/plot_donut.R")
# pdf_particip <- ggDonut(temp, "region", "value")
# pdf_particip <- pdf_particip + 
#   annotate("text", x = 0, y = 0, label = paste0(format_n(expo.arg.zona.acum), "\n", t.valor), size=3.3)
# pdf_particip <- pdf_particip + theme(rect = element_rect(fill = "transparent"))
# pdf_particip <- pdf_particip + theme(plot.background = element_rect(fill = "transparent", colour = NA))
# 
# # ggsave(paste0("plot1.png"), plot1, width = 4, height = 4, dpi=600) #, bg ="transparent"

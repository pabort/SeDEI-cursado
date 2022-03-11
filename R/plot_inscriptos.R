pal_col <- c("#B50E1A", "#FED105")
carreras <- c('Ciencias Económicas', 'Contador Público', 'Licenciatura en Administración', 'Licenciatura en Economía')

f <- list(
  size = 20,
  family = 'sans-serif')


df_inscriptos <- df_cursadas %>% 
  filter(propuesta_formativa_desc %in% carreras) %>% 
  select(anio_academico, periodo_lectivo, cantidad_alumnos) %>% 
  group_by(anio_academico, periodo_lectivo) %>% 
  summarise(cantidad = sum(cantidad_alumnos)) %>% 
  mutate(anio_academico = as.factor(anio_academico))


m <- list(
  l = 5,
  r = 5,
  b = 25,
  t = 20,
  pad = 4
)

fig_inscrip <- df_inscriptos %>% 
    filter(periodo_lectivo %in% c("11 - PRIMER SEMESTRE", "11 - SEGUNDO SEMESTRE")) %>% 
  mutate(periodo_lectivo = factor(periodo_lectivo, levels = c("11 - PRIMER SEMESTRE", "11 - SEGUNDO SEMESTRE"), labels = c("1er semestre", "2do semestre"))) %>% 
    plot_ly(width = 700, height = 400, 
            x = ~anio_academico, y = ~cantidad, color = ~periodo_lectivo, 
            type = 'bar', colors = rev(pal_col),
            text = ~paste(glue('({format_n(cantidad,0)} inscripciones)')),
            #"Participación: ",format_n(particip,1), '%', "<br>"),
            hoverinfo = 'text') %>% 
    layout(separators = ',',
           showlegend = T,
           legend = list(title=list(text=''),
                         orientation = "h",   # show entries horizontally
                         xanchor = "right",  # use center of legend as anchor
                         x = 1, y=1.1),
           autosize = T,
           font = f,
           annotations = 
             list(x = 1, y = -0.17, text = "Fuente: SeDEI en base a datos de Guaraní", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size =14, color="gray")),
           xaxis = list(title = ''), yaxis = list(title = ''),
           hoverlabel = list(font=list(size=20)),
           margin = m) %>%
    config(displayModeBar = F)
# plot_lst_reg[['11 - PRIMER SEMESTRE']]

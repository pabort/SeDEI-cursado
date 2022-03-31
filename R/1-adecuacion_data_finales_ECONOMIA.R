carreras <- c('Licenciatura en Economía') #' , 'Licenciatura en Administración', 'Licenciatura en Economía'
turnos <- c('Febrero','Mayo','Julio','Septiembre', 'Noviembre', 'Nivelación')

vars_names <-  c('anio_academico',
                 'propuesta_formativa_desc',
                 'periodo_lectivo',
                 'resultado',
                 'cantidad_alumnos')

# df_finales <- read_csv("data/data_finales_19_21_agregado.csv") %>% 
#   setNames(vars_names) %>% 
#   mutate(turno = str_extract(periodo_lectivo, paste(turnos, collapse = '|')),
#          resultado = ifelse(resultado == 'VACIO/NULO', 'Ausente', resultado),
#          resultado2 = ifelse(resultado %in% c('Aprobado', 'Reprobado'), 'Presentes', 'Ausentes')) %>% 
#   filter(propuesta_formativa_desc %in% carreras, turno != 'Nivelación')

df_finales <- read_csv("data/data_finales_examen_materia.csv") %>% 
  mutate(turno = str_extract(turno, paste(turnos, collapse = '|')),
         resultado2 = resultado_desc,
         cantidad_alumnos = q,
         resultado =resultado_desc,
         resultado2 = ifelse(resultado_desc %in% c('Aprobado', 'Reprobado'), 'Presentes', 'Ausentes')) %>% 
  filter(turno != 'Nivelación',
         propuesta_formativa_desc %in% carreras)

# separate(col = `Alumno - Legajo`, into = c("alumno", "legajo"), sep = " - ")


df_finales_count <- df_finales %>% 
  group_by(anio_academico,turno,resultado2) %>% #,resultado
  summarise(cantidad = sum(cantidad_alumnos), .groups ='drop_last') %>% 
  mutate(porcentaje = cantidad/sum(cantidad)*100,
         resultado2 = factor(resultado2, levels = c('Presentes', 'Ausentes'), labels = c('Presentes', 'Ausentes')),
         anio_academico = factor(anio_academico))

# Plot: cantidad de finales rendidos ----
# incluimos presentes

pal_col <- c("#D3D3D3", "#FED105")

f <- list(
  size = 20,
  family = 'sans-serif')

m <- list(
  l = 5,
  r = 5,
  b = 25,
  t = 20,
  pad = 4
)

plot_lst_evol_finales <- vector("list", length = 6)

for (i in turnos[turnos != 'Nivelación']) {
  
  fig_finales_evol <- df_finales_count %>% 
    filter(turno == i) %>% 
    plot_ly(width = 700, height = 400, 
            x = ~anio_academico, y = ~cantidad, color = ~resultado2, 
            type = 'bar', colors = rev(pal_col),
            text = ~paste(glue('{format_n(cantidad,0)} inscripciones ({format_n(porcentaje,1)} %)')),
            #"Participación: ",format_n(particip,1), '%', "<br>"),
            hoverinfo = 'text') %>% 
    layout(barmode = 'stack',
           separators = ',',
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
  
  plot_lst_evol_finales[[i]] <- fig_finales_evol
}


# Plot: aprobados y reprobados ----
pal_col <- c("#B50E1A", "#FED105")

df_finales_aprobados <- df_finales %>% 
  filter(resultado != 'Ausente') %>% 
  group_by(anio_academico,turno,resultado) %>% #,resultado
  summarise(cantidad = sum(cantidad_alumnos), .groups ='drop_last') %>% 
  mutate(porcentaje = cantidad/sum(cantidad)*100,
         resultado = factor(resultado, levels = c('Aprobado', 'Reprobado'), labels = c('Aprobados', 'Reprobados')),
         anio_academico = factor(anio_academico))

plot_lst_finales_aprobados <- vector("list", length = 6)

for (i in turnos[turnos != 'Nivelación']) {
  
  fig_finales_aprobados <- df_finales_aprobados %>% 
    filter(turno == i) %>% 
    plot_ly(width = 700, height = 400, 
            x = ~anio_academico, y = ~porcentaje, color = ~resultado, 
            type = 'bar', colors = rev(pal_col),
            text = ~paste(glue('{format_n(porcentaje,2)} %'), '<br>',
                          glue('({format_n(cantidad,0)} inscripciones)')),
            #"Participación: ",format_n(particip,1), '%', "<br>"),
            hoverinfo = 'text') %>% 
    layout(barmode = 'stack',
           separators = ',',
           showlegend = T,
           legend = list(title=list(text=''),
                         orientation = "h",   # show entries horizontally
                         xanchor = "right",  # use center of legend as anchor
                         x = 1, y=1.15),
           autosize = T,
           font = f,
           annotations = 
             list(x = 1, y = -0.17, text = "Fuente: SeDEI en base a datos de Guaraní", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size =14, color="gray")),
           xaxis = list(title = ''), yaxis = list(title = list(text=''),
                                                  ticksuffix = "%",  range = c(0, 100)),
           hoverlabel = list(font=list(size=20)),
           margin = m) %>%
    config(displayModeBar = F)
  
  plot_lst_finales_aprobados[[i]] <- fig_finales_aprobados
}

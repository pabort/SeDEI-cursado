carreras <- c('Contador Público')
lab_carreras <- c('Cs. Económicas', 'Contador Público', 'Lic. en Administración', 'Lic. en Economía')


df_evol_actuacion_carr <- df_cursadas %>% 
  mutate(actuacion = if_else(resultado_desc == "Sin Actuación", "Sin actuación", "Con actuación")) %>% 
  select(anio_academico, periodo_lectivo, propuesta_formativa_desc,tipo_propuesta_formativa_desc, actuacion, cantidad_alumnos) %>% 
  filter(propuesta_formativa_desc %in% carreras,
         periodo_lectivo %in% c('11 - PRIMER SEMESTRE', '11 - SEGUNDO SEMESTRE')) %>% 
  group_by(anio_academico, periodo_lectivo, tipo_propuesta_formativa_desc, actuacion) %>% #,resultado
  summarise(cantidad = sum(cantidad_alumnos), .groups ='drop_last') %>% 
  mutate(porcentaje = cantidad/sum(cantidad)*100,
         anio_academico = as.factor(anio_academico),
         actuacion = factor(actuacion, levels = c('Con actuación', 'Sin actuación'), labels = c('Con actuación', 'Sin actuación')),
         time_carr = paste0(tipo_propuesta_formativa_desc, "-", periodo_lectivo))

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

periodos <- unique(df_evol_actuacion_carr$time_carr)
plot_lst_evol_act_carr <- vector("list", length = length(periodos))

for (i in periodos) {
  
  fig <- df_evol_actuacion_carr %>% 
    filter(time_carr == i) %>% 
    plot_ly(width = 700, height = 400, 
            x = ~anio_academico, y = ~porcentaje, color = ~actuacion, 
            type = 'bar', colors = rev(pal_col),
            text = ~paste(glue('{format_n(porcentaje,2)} %'), '<b>',
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
  
  plot_lst_evol_act_carr[[i]] <- fig
}

#plot_lst_evol_act_carr[['Ciclo Básico-11 - SEGUNDO SEMESTRE']]
#plot_lst_evol_act_carr[['Ciclo Profesional-11 - PRIMER SEMESTRE']]

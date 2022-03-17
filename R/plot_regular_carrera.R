
pal_col <- c("#B50E1A", "#FED105")
carreras <- c('Ciencias Económicas', 'Contador Público', 'Licenciatura en Administración', 'Licenciatura en Economía')
lab_carreras <- c('Cs. Económicas', 'Contador Público', 'Lic. en Administración', 'Lic. en Economía')

f <- list(
  size = 20,
  family = 'sans-serif')

df_regular_carr <- df_cursadas %>% 
  mutate(actuacion = if_else(resultado_desc == "Sin Actuación", "Sin actuación", "Con actuación")) %>% 
  filter(actuacion == "Con actuación", propuesta_formativa_desc %in% carreras) %>% 
  select(anio_academico, periodo_lectivo, propuesta_formativa_desc, resultado_desc, cantidad_alumnos) %>% 
  group_by(anio_academico, periodo_lectivo, propuesta_formativa_desc, resultado_desc) %>% 
  summarise(cantidad = sum(cantidad_alumnos)) %>% 
  mutate(porcentaje = cantidad / sum(cantidad)*100) %>% 
  #  mutate(time = paste(anio_academico)) %>% 
  #  spread(resultado_desc, cantidad) %>% 
  mutate(anio_academico = as.factor(anio_academico),
         resultado_desc = factor(resultado_desc, levels = c('Regular', 'Libre'), labels = c('Regulares', 'Libres')),
         propuesta_formativa_desc = factor(propuesta_formativa_desc, levels = carreras, labels = lab_carreras),
         time_carr = paste0(periodo_lectivo, "-", propuesta_formativa_desc))



m <- list(
  l = 5,
  r = 5,
  b = 30,
  t = 20,
  pad = 4
)

filter_periodo <- paste0(c(rep("11 - PRIMER SEMESTRE-", 4), rep("11 - SEGUNDO SEMESTRE-", 4)),
                         lab_carreras)

plot_lst_reg_carr <- vector("list", length = length(filter_periodo))

for (i in filter_periodo) {
  
  fig <- df_regular_carr %>% 
    filter(time_carr == i) %>% 
    plot_ly(width = 700, height = 400, 
            x = ~anio_academico, y = ~porcentaje, color = ~resultado_desc, 
            type = 'bar', colors = rev(pal_col),
            text = ~paste(
              '<b>',resultado_desc , '</b>', "<br>",
              glue('{format_n(porcentaje,2)} %'), "<br>",
              glue('({format_n(cantidad,0)} inscripciones)')),
            #"Participación: ",format_n(particip,1), '%', "<br>"),
            hoverinfo = 'text') %>% 
    layout(barmode = 'stack',
           separators = ',',
           showlegend = T,
           legend = list(title=list(text='<b> Condición </b>')),
           autosize = T,
           font = f,
           annotations = 
             list(x = 1, y = -0.17, text = "Fuente: SeDEI en base a datos de Guaraní", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size =14, color="gray")),
           xaxis = list(title = ''), yaxis = list(title = 'Porcentaje (%)'),
           hoverlabel = list(font=list(size=20)),
           margin = m) %>%
    config(displayModeBar = F)
  plot_lst_reg_carr[[i]] <- fig
  
}

# plot_lst_reg_carr[['11 - SEGUNDO SEMESTRE-Lic. en Economía']]

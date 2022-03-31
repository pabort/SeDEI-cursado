pal_col <- c("#B50E1A", "#FED105")
carreras <- c('Licenciatura en Administración') #  
lab_carreras <- c('Cs. Económicas', 'Contador Público', 'Lic. en Administración', 'Lic. en Economía') # 

f <- list(
  size = 20,
  family = 'sans-serif')

df_regular_carr <- df_cursadas %>% 
  mutate(actuacion = if_else(resultado_desc == "Sin Actuación", "Sin actuación", "Con actuación")) %>% 
  filter(actuacion == "Con actuación", propuesta_formativa_desc %in% carreras) %>% 
  select(anio_academico, periodo_lectivo, tipo_propuesta_formativa_desc, resultado_desc, cantidad_alumnos) %>% 
  group_by(anio_academico, periodo_lectivo, tipo_propuesta_formativa_desc, resultado_desc) %>% 
  summarise(cantidad = sum(cantidad_alumnos)) %>% 
  mutate(porcentaje = cantidad / sum(cantidad)*100) %>% 
  #  mutate(time = paste(anio_academico)) %>% 
  #  spread(resultado_desc, cantidad) %>% 
  mutate(anio_academico = as.factor(anio_academico),
         resultado_desc = factor(resultado_desc, levels = c('Regular', 'Libre'), labels = c('Regulares', 'Libres')),
         time_carr = paste0(anio_academico, "-", periodo_lectivo))



m <- list(
  l = 5,
  r = 5,
  b = 30,
  t = 20
)

filter_periodo <- paste0(c(2019:2021),
                         c(rep("-11 - PRIMER SEMESTRE", 3), rep("-11 - SEGUNDO SEMESTRE", 3))
)

plot_lst_reg_carr2 <- vector("list", length = length(filter_periodo))

for (i in filter_periodo) {
  
  fig <- df_regular_carr %>% 
    filter(time_carr == i) %>% 
    plot_ly(width = 800, height = 420,
            y = ~tipo_propuesta_formativa_desc, x = ~porcentaje, color = ~resultado_desc, 
            type = 'bar', orientation = 'h',
            colors = rev(pal_col),
            text = ~paste(
              '<b>',resultado_desc , '</b>', "<br>",
              glue('{format_n(porcentaje,2)} %'), "<br>",
              glue('({format_n(cantidad,0)} inscripciones)')),
            textposition="none",
            #"Participación: ",format_n(particip,1), '%', "<br>"),
            hoverinfo = 'text') %>% 
    layout(barmode = 'stack',
           separators = ',',
           showlegend = T,
           legend = list(title=list(text='<b> Condición </b>')),
           autosize = T,
           font = f,
           annotations = 
             list(x = 1, y = -0.13, text = "Fuente: SeDEI en base a datos de Guaraní", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='middle', xshift=0, yshift=0,
                  font=list(size =14, color="gray")),
           yaxis = list(title = ''), xaxis = list(title = list(text=''),
                                                  ticksuffix = "%",  range = c(0, 100)),
           hoverlabel = list(font=list(size=20)),
           margin = m) %>%
    config(displayModeBar = F)
  plot_lst_reg_carr2[[i]] <- fig
  
}

# plot_lst_reg_carr2[['2021-11 - SEGUNDO SEMESTRE']]
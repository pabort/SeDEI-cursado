pal_col <- c("#670000", 
             "#561D41",
             "#04B7CE",
             "#015360")

f <- list(
  size = 20,
  family = 'sans-serif')

m <- list(
  l = 5,
  r = 5,
  b = 25,
  t = 30,
  pad = 4
)

df_inscriptos_carr <- df_cursadas %>% 
  select(anio_academico, periodo_lectivo, cantidad_alumnos, propuesta_formativa_desc) %>% 
  filter(propuesta_formativa_desc %in% c('Ciencias Económicas', 'Contador Público', 'Licenciatura en Administración', 'Licenciatura en Economía')) %>% 
  group_by(anio_academico, periodo_lectivo, propuesta_formativa_desc) %>% 
  summarise(cantidad = sum(cantidad_alumnos)) %>% 
  mutate(porcentaje = cantidad / sum(cantidad)*100) %>% 
  mutate(anio_academico = as.factor(anio_academico),
         propuesta_formativa_desc = factor(propuesta_formativa_desc,
                                           levels = c('Ciencias Económicas', 'Contador Público', 'Licenciatura en Administración', 'Licenciatura en Economía'),
                                           labels = c('Cs. Económicas', 'Contador Público', 'Lic. en Administración', 'Lic. en Economía')))

periodos <- c('11 - PRIMER SEMESTRE', '11 - SEGUNDO SEMESTRE')
plot_lst_insc_carr <- vector("list", length = length(periodos))

for (i in periodos) {
  
 fig <- df_inscriptos_carr %>% 
    filter(periodo_lectivo == i) %>% 
    plot_ly(width = 850, height = 400, 
            x = ~anio_academico, y = ~porcentaje, color = ~propuesta_formativa_desc, 
            type = 'bar', colors = pal_col,
            text = ~paste(
              '<b>',propuesta_formativa_desc , '</b>', "<br>",
              glue('{format_n(porcentaje,2)} %'), "<br>",
              glue('({format_n(cantidad,0)} inscripciones)')),
            #"Participación: ",format_n(particip,1), '%', "<br>"),
            hoverinfo = 'text',
            textposition="none",
            marker = list(line=list(color='white',width=1.5))) %>% 
    layout(barmode = 'stack',
           separators = ',',
           showlegend = T,
           legend = list(title=list(text='<b> Carrera </b>')),
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
  plot_lst_insc_carr[[i]] <- fig
  
}

# plot_lst_insc_carr[['11 - SEGUNDO SEMESTRE']]

library(plotly)
library(RColorBrewer)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

accumulated_ageSpecificMarriageRate <- df_ageSpecificMarriageRate %>%
  accumulate_by(~year)

animated_plotly_linechart_marriageRateByAge <- accumulated_ageSpecificMarriageRate %>%
  plot_ly(
    x=~year, 
    y=~value,
    split=~age_group,
    frame=~frame, 
    type='scatter',
    mode='lines', 
    line=list(simplyfy = F),
    hoverinfo='text', 
    text=~paste('Age Group:', age_group, 
                '<br> Year:', year, 
                '<br> Marriages per 1,000 Residents:', value
                )
  ) %>% 
  layout(
    title='Marriage Rate by Age Group', 
    xaxis = list(
      title = "Years",
      zeroline = F
    ),
    yaxis = list(
      title = "Marriages per 1,000 Residents",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Year: "
    )
  ) 


plotly_linechart_marriageRateByAge <- plot_ly(
  data=df_ageSpecificMarriageRate, 
  x=~df_ageSpecificMarriageRate$year, 
  y=~df_ageSpecificMarriageRate$value, 
  type='scatter',
  mode='lines', 
  color=~df_ageSpecificMarriageRate$age_group, 
  hoverinfo='text', 
  text=~paste('Age Group:', age_group, 
                '<br> Year:', year, 
                '<br> Marriages per 1,000 Residents:', value
  )
) %>% layout(
  title='Marriage Rate by Age Group', 
  xaxis=list(title='Years'), 
  yaxis=list(title='Marriages per 1,000 Residents')
)

plotly_piechart_marriageRateByAge <- plot_ly(df_numMarriagesByAge, 
        labels = ~df_numMarriagesByAge$age_group, 
        values = ~df_numMarriagesByAge$number, 
        type = 'pie',
        marker = list(colors=brewer.pal(7,'Set3')),
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = c('#595959')),
        hoverinfo = 'text',
        text = ~paste(number, 'Marriages per 1,000 Residents')
) %>% layout(
  title='Marriage Rate by Age Group', 
  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
)

plotly_linechart_marriageAndConstructionRate <- plot_ly() %>%
  add_lines(x=df_numMarriagesByYear_f$year, 
            y=df_numMarriagesByYear_f$number,
            name="Marriage Rate by 25 - 34 Years",
            type="scatter", 
            mode="lines",
            line=list(color = '#E7298A'),
            hoverinfo='text', 
            text=~paste('Year:', df_numMarriagesByYear_f$year, 
                        '<br> Marriages per 1,000 Residents:', df_numMarriagesByYear_f$number
            )) %>%
  add_lines(x=df_flatsConstructedByHDB$year, 
            y=df_flatsConstructedByHDB$flats_constructed, 
            name="Flats Constructed", 
            yaxis="y2",
            type="scatter", 
            mode="lines",
            line=list(color = '#E6AB02'),
            hoverinfo='text', 
            text=~paste('Year:', df_flatsConstructedByHDB$year, 
                        '<br> Number of Flats:', df_flatsConstructedByHDB$flats_constructed
            )) %>%
  layout(
    title="Marriage and Flats Construction Rate", 
    yaxis=list(
      title="Marriages per 1,000 Residents"
    ),
    yaxis2=list(
      title="Number of Flats",
      overlaying = "y",
      side = "right"
    ),
    xaxis=list(title="Year", ticks=df_numMarriagesByYear_f$year)
  )

plotly_linechart_marriageAndDivorceRate <- plot_ly() %>%
  add_lines(x=df_numMarriagesByYear_f$year, 
            y=df_numMarriagesByYear_f$number,
            name="Marriage Rate by 25 - 34 Years",
            type="scatter", 
            mode="lines",
            hoverinfo='text', 
            line=list(color = '#E7298A'),
            text=~paste('Year:', df_numMarriagesByYear_f$year, 
                        '<br> Marriages per 1,000 Residents:', df_numMarriagesByYear_f$number
            )) %>%
  add_lines(x=df_numDivorces_f$year, 
            y=df_numDivorces_f$value, 
            name="Number of Divorces within 5-9 Years", 
            yaxis="y2",
            type="scatter", 
            mode="lines",
            line=list(color = '#666666'),
            hoverinfo='text', 
            text=~paste('Year:', df_numDivorces_f$year, 
                        '<br> Number of Divorces:', df_numDivorces_f$value
            )) %>%
  layout(
    title="Marriage and Divorce Rate", 
    yaxis=list(
      title="Marriages per 1,000 Residents"
    ),
    yaxis2=list(
      title="Number of Divorces",
      overlaying="y",
      side="right"
    ),
    xaxis=list(title="Year", ticks=df_numMarriagesByYear_f$year)
  )

plotly_barchart_grants <- plot_ly(df_grants, 
        x=~income_level, 
        y=~grant_amount, 
        type='bar',
        color=~income_level, 
        hoverinfo='text', 
        text=~paste('Income Level:', income_level, 
                    '<br> Grant Amount:', grant_amount
        )
) %>% layout(
  title = "Grants by Income Level",
  xaxis = list(title = "Income Level"),
  yaxis = list(title = "Grant Amount (S$)")
)

Sys.setenv("plotly_username"="rebekahlow") 
Sys.setenv("plotly_api_key"="RGNdq89F2IGdXKHJb2k9")

api_create(plotly_barchart_grants, filename = "grants-by-income-level")
api_create(plotly_linechart_marriageAndConstructionRate, filename = "marriage-and-construction-rate")
api_create(plotly_linechart_marriageAndDivorceRate, filename = "marriage-and-divorce-rate")
api_create(plotly_linechart_marriageRateByAge, filename = "marriage-by-age-line-chart")
api_create(plotly_piechart_marriageRateByAge, filename = "marriage-by-age-line-chart-pie-chart")
api_create(animated_plotly_linechart_marriageRateByAge, filename = "marriage-by-age-line-chart-animated-chart")



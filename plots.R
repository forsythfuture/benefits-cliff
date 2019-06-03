#########################################################
#
# This script creates Altair plots for the benefits cliff
#
#########################################################

library(tidyverse)
library(plotly)
#library(reticulate)
#library(altair)

benefits <- read_rds("benefits_tables/data/made_up.rds")

#benefits_slim <- benefits %>%
#  filter(composition == 'One adult, three children')

# unique benefits values for drop down
unique_composition <- as.character(unique(benefits$composition))

# create list of arguments for drop down arrows
drop_down_values <- map(unique_composition, function(x) {
  list(method = "restyle",
       args = list("transforms[0].value", x),
                   label = x)
})

# plotly

benefits %>%
  plot_ly(x=~monthly_income, y=~payment, color=~benefit,
          type = 'scatter', mode = 'lines',
          transforms = list(
            list(
              type = 'filter',
              target = ~composition,
              operation = '=',
              value = unique_composition[1])
          )
  ) %>%
  layout(
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = drop_down_values
      )
    )
  )


#####################################################################3

# create drop down menu
input_dropdown = alt$binding_select(options=unique_composition)

# filter based on drop down
drop_down_select = alt$selection_single(fields=list('composition'),
                                        bind=input_dropdown,
                                        name='Family ')

# Create a selection that chooses the nearest point & selects based on x-value
nearest = alt$selection(type='single', nearest=TRUE, on='mouseover',
                        fields=list('monthly_income'), empty='none')

line = alt$Chart(benefits_slim)$mark_line(interpolate = "basis")$encode(
    x=alt$X('monthly_income:Q', title='Monthly Wages',
            axis=alt$Axis(format="$,r")),
    y=alt$Y('payment:Q', title='Benefit Amount',
            axis=alt$Axis(format="$,r")),
    color=alt$Color('benefit:N', title='Benefit')
  )$add_selection(
    drop_down_select
  )$transform_filter(
    drop_down_select
  )

# Transparent selectors across the chart. This is what tells us
# the x-value of the cursor
selectors <- alt$Chart()$mark_point()$encode(
    x = 'monthly_income:Q',
    opacity = alt$value(0)
)$properties(
  selection = nearest
)$copy()

# Draw points on the line, and highlight based on selection
points = line$mark_point()$encode(
  opacity=alt$condition(nearest, alt$value(2), alt$value(0))
)

# Draw text labels near the points, and highlight based on selection
text = line$mark_text(
  align='left', dx=5, dy=-5
)$encode(
  text=alt$condition(nearest, 'payment:Q', alt$value(' '))
)

# Draw a rule at the location of the selection
rules = alt$Chart(benefits_slim)$mark_rule(color='gray')$encode(
  x='monthly_income:Q'
)$transform_filter(
  nearest$ref()
)

# Put the four layers into a chart and bind the data
final = alt$layer(
  line, selectors, points, rules, text
)

final

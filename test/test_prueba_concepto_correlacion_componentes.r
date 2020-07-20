
# correlacion componente principal largest, y componentes cantidad

#levantar_datos 
pacman::p_load("tidyverse")
base::load(here::here("data","temporal_acumulado_data.Rdata"))
base::load(here::here("data","temporal_basico_data.Rdata"))


#temporal_acumulado_estr_grafos

temporal_basico_estr_grafos %>%  
    select(periodo, componentes_cantidad,componentes_largest,componentes_largest_porc)
# componentes_cantidad = cantidad de componentes en la red.
# componentes_largest = cantidad de autores en el componente mayor.
# componentes_largest_porc = porcentaje que representa el componente mayor en cantidad de autores vs del total de autores


temporal_basico_estr_grafos %>% 
    select(componentes_cantidad,componentes_largest) %>% cor()

# hay una leve correlacion negativa entre la cantidad de componentes, 
# y cuanto representa el componente mayor respecto del total de autores.    
temporal_basico_estr_grafos %>% 
    select(componentes_cantidad, componentes_largest_porc) %>% cor()

temporal_basico_estr_grafos %>% 
    select(componentes_largest, componentes_largest_porc) %>% cor()
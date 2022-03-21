# hacer un join 
# Para caso 32 de Muestros 
# En relación a mapas


juntos <- merge(x = tabla_frec, y = localidades6, by.x = 'Category', by.y = 'Nom_Loc') %>%
  select(Category, Lat_Decimal, Lon_Decimal, rf) 
juntos
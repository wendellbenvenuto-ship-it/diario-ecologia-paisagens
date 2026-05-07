---
title: "Exercício 4 - Amostragem de Paisagens com Restrição de Borda — Murici-AL"
date: 2026-04-13
author: "Wendell Benvenuto"
---


::: {.cell}

```{.r .cell-code}
library(terra)
```

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'terra' foi compilado no R versão 4.5.1
```


:::

::: {.cell-output .cell-output-stderr}

```
terra 1.8.60
```


:::

```{.r .cell-code}
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'tidyverse' foi compilado no R versão 4.5.1
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'tibble' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'tidyr' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'readr' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'purrr' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'dplyr' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'stringr' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'forcats' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'lubridate' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```
── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.2.1     ✔ readr     2.2.0
✔ forcats   1.0.1     ✔ stringr   1.6.0
✔ ggplot2   4.0.3     ✔ tibble    3.3.1
✔ lubridate 1.9.5     ✔ tidyr     1.3.2
✔ purrr     1.2.2     
```


:::

::: {.cell-output .cell-output-stderr}

```
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ tidyr::extract() masks terra::extract()
✖ dplyr::filter()  masks stats::filter()
✖ dplyr::lag()     masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```


:::

```{.r .cell-code}
library(tidyterra)
```

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'tidyterra' foi compilado no R versão 4.5.3
```


:::

::: {.cell-output .cell-output-stderr}

```

Anexando pacote: 'tidyterra'

O seguinte objeto é mascarado por 'package:stats':

    filter
```


:::

```{.r .cell-code}
library(landscapemetrics)
```

::: {.cell-output .cell-output-stderr}

```
Warning: pacote 'landscapemetrics' foi compilado no R versão 4.5.3
```


:::

```{.r .cell-code}
setwd("C://Users/benve/OneDrive/Documentos/diario_ecologia_wendell/exercicios") # mude para o seu próprio diretório

r_murici <- rast("2024_murici.tif")

r_utm <- project(r_murici, "EPSG:31985", method = "near")

legenda <- data.frame(
  id = c(3, 4, 11, 15, 20, 21, 24, 25, 33),
  Categoria = c("Formação Florestal", "Formação Savânica", "Campo Alagado", 
                "Pastagem", "Cana", "Mosaico de Usos", "Área Urbanizada", 
                "Outras Áreas não Vegetadas", "Corpo d'Água"),
  Cores = c("#006400", "#7cfc00", "#00ffff", "#ffff00", "#ff4500", 
            "#f4a460", "#ff0000", "#d3d3d3", "#0000ff")
)

levels(r_utm) <- legenda[,1:2]

vetor_total <- as.polygons(r_utm, dissolve = TRUE)
municipio_limite <- aggregate(vetor_total)
limite_seguro <- buffer(municipio_limite, width = -2000)

floresta <- vetor_total[vetor_total$Categoria == "Formação Florestal", ]
area_sorteio <- crop(floresta, limite_seguro)

if (is.list(area_sorteio)) {
  area_sorteio <- do.call(rbind, area_sorteio)
}

if (nrow(area_sorteio) == 0) {
  stop("Erro: Nenhuma floresta encontrada a mais de 2km da borda.")
}
```
:::



::: {.cell}

```{.r .cell-code}
ggplot() +
  geom_spatraster(data = r_utm) +
  scale_fill_manual(values = legenda$Cores, na.value = "white", name = "Uso do Solo") +
  geom_spatvector(data = municipio_limite, fill = NA, color = "black", linewidth = 1) +
  geom_spatvector(data = limite_seguro, fill = NA, color = "black", linetype = "dashed", linewidth = 1) +
  geom_spatvector(data = floresta, fill = "darkgreen", alpha = 0.3, color = NA) +
  geom_spatvector(data = area_sorteio, fill = "forestgreen", alpha = 0.5, color = NA) +
  labs(title = "Área de sorteio (floresta > 2 km da borda)") +
  theme_minimal()
```

::: {.cell-output .cell-output-stderr}

```
<SpatRaster> resampled to 500736 cells.
```


:::

::: {.cell-output-display}
![](exercicio4_files/figure-html/mapa-murici-1.png){width=672}
:::
:::


![](/images/murici_mapa.jpg)


::: {.cell}

```{.r .cell-code}
set.seed(1234)
pontos_finais <- NULL
tentativas <- 0

while(is.null(pontos_finais) || nrow(pontos_finais) < 15) {
  tentativas <- tentativas + 1
  cand <- spatSample(area_sorteio, size = 1, method = "random")
  
  if (nrow(cand) > 0) {
    if (is.null(pontos_finais)) {
      pontos_finais <- cand
    } else {
      dists <- distance(cand, pontos_finais)
      if (min(dists) >= 1000) { 
        pontos_finais <- rbind(pontos_finais, cand)
      }
    }
  }
  
  if (tentativas > 5000) break
}

# Verificar CRS
if (!identical(crs(pontos_finais), crs(r_utm))) {
  pontos_finais <- project(pontos_finais, crs(r_utm))
}

buffers <- buffer(pontos_finais, width = 500)
buffers$id_paisagem <- 1:nrow(buffers)

# Extração e processamento
extracao <- terra::extract(r_utm, buffers)
nome_col_raster <- names(extracao)[2]

tabela_final <- extracao %>%
  rename(id_buffer = ID, categoria_bruta = !!sym(nome_col_raster)) %>%
  group_by(id_buffer) %>%
  mutate(total_px = n()) %>%
  group_by(id_buffer, categoria_bruta) %>%
  summarise(
    pixels = n(),
    percentagem = (pixels / first(total_px)) * 100,
    .groups = "drop"
  ) %>%
  mutate(Categoria_Limpa = as.character(categoria_bruta)) %>%
  left_join(legenda %>% select(Categoria, Cores), by = c("Categoria_Limpa" = "Categoria"))

write.csv(tabela_final, "uso_solo_murici_final.csv", row.names = FALSE)
writeVector(pontos_finais, "pontos_final.shp", overwrite=TRUE)

writeVector(buffers, "buffers_500m.shp", overwrite=TRUE)
```
:::



::: {.cell}

```{.r .cell-code}
ggplot() +
  geom_spatraster(data = r_utm) +
  scale_fill_manual(values = legenda$Cores, na.value = "white", name = "Uso do Solo") +
  geom_spatvector(data = buffers, fill = NA, color = "black", linewidth = 1) +
  geom_spatvector(data = pontos_finais, color = "red", size = 2, shape = 19) +
  geom_spatvector_text(data = pontos_finais, aes(label = 1:15), 
                       color = "white", size = 3, vjust = -0.8) +
  labs(title = "Pontos amostrais e buffers de 500 m") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

::: {.cell-output .cell-output-stderr}

```
<SpatRaster> resampled to 500736 cells.
```


:::

::: {.cell-output-display}
![](exercicio4_files/figure-html/mapa-pontos-1.png){width=672}
:::
:::


![](/images/Murici_pontos.jpg)


::: {.cell}

```{.r .cell-code}
# Identificar o valor numérico da classe Floresta
id_floresta <- legenda$id[legenda$Categoria == "Formação Florestal"]

# Lista para armazenar métricas de cada buffer
metricas_lista <- list()

for(i in 1:nrow(buffers)) {
  # Recortar o raster para o buffer i
  crop_i <- crop(r_utm, buffers[i,])
  mask_i <- mask(crop_i, buffers[i,])
  
  # Calcular Shannon diversity (nível da paisagem)
  shannon <- lsm_l_shdi(mask_i)
  
  # Calcular métricas de classe para todas as classes
  pland_all <- lsm_c_pland(mask_i)   # percentual de cada classe
  ed_all <- lsm_c_ed(mask_i)         # densidade de borda por classe
  
  # Filtrar apenas a classe floresta
  pland_floresta <- pland_all %>% filter(class == id_floresta)
  ed_floresta <- ed_all %>% filter(class == id_floresta)
  
  # Extrair valores (se não houver floresta, colocar NA ou 0)
  div_shannon <- shannon$value
  perc_floresta <- ifelse(nrow(pland_floresta) > 0, pland_floresta$value, 0)
  dens_borda <- ifelse(nrow(ed_floresta) > 0, ed_floresta$value, NA)
  
  # Guardar
  metricas_lista[[i]] <- data.frame(
    id_buffer = i,
    diversidade_shannon = div_shannon,
    perc_floresta = perc_floresta,
    densidade_borda_m_ha = dens_borda
  )
}

# Combinar tudo
metricas_paisagem <- bind_rows(metricas_lista)

print(metricas_paisagem)
```

::: {.cell-output .cell-output-stdout}

```
   id_buffer diversidade_shannon perc_floresta densidade_borda_m_ha
1          1           0.7507504      78.22410             44.41351
2          2           0.7551841      75.62028             53.66340
3          3           1.2318169      49.41922             53.94980
4          4           0.8906418      71.73679             33.35833
5          5           1.0747283      57.00326             63.86672
6          6           0.0000000     100.00000              0.00000
7          7           1.3336246      31.62939             74.45502
8          8           0.8789672      70.95745             25.38790
9          9           1.3141271      23.73247             48.22454
10        10           0.7153538      69.32907             24.34106
11        11           1.0003162      44.07684             58.11278
12        12           1.2808703      13.84452             44.74460
13        13           0.6093134      83.75527             24.81909
14        14           0.0000000     100.00000              0.00000
15        15           1.0722640      23.58591             53.09069
```


:::
:::



::: {.cell}

```{.r .cell-code}
lista_recortes <- list()
for(i in 1:nrow(buffers)) {
  crop_i <- crop(r_utm, buffers[i, ])
  mask_i <- mask(crop_i, buffers[i, ])
  df_i <- as.data.frame(mask_i, xy = TRUE, cells = TRUE)
  df_i$id_buffer <- paste("Paisagem", i)
  lista_recortes[[i]] <- df_i
}
df_painel <- bind_rows(lista_recortes)

ggplot(df_painel) +
  geom_tile(aes(x = x, y = y, fill = Categoria)) +
  scale_fill_manual(values = setNames(legenda$Cores, legenda$Categoria)) +
  facet_wrap(~id_buffer, nrow = 3, ncol = 5, scales = "free") +
  theme_minimal() +
  labs(title = "Painel de Amostragem: 15 Paisagens de Murici-AL",
       subtitle = "Recortes circulares de 500m de raio (Interior de Floresta)",
       fill = "Uso do Solo") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")
```

::: {.cell-output-display}
![](exercicio4_files/figure-html/continuar-analise-maparecortes-1.png){width=672}
:::
:::


![](/images/murici_recortes.jpg)


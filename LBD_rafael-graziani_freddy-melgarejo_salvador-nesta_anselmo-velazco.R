# Carga de librerías
library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(openxlsx)
library(stringr)

# Leer bases de datos
# Se establece como criterio usar data desde 2013 en adelante, ya que desde ese entonces,
# el Madrid ha destacado mucho en la competición

# Estadísticas por país
ranking_by_country <- read.csv("AllTimeRankingByCountry.csv") %>% 
  # Al explorar la data, se procede a limpiarla
  select(-"X") %>% 
  # Se seleccionan aquellos países que hayan ganado al menos 1 vez el título
  filter(Titles != 0)

# Estadísticas por club
ranking_by_club <- read_excel("Rankings by club.xlsx", sheet = 1)

# Eliminar fila con NAs
ranking_by_club <- na.omit(ranking_by_club)

# Se cambian los nombres de las columnas con los caracteres de la primera fila
colnames(ranking_by_club) <- ranking_by_club[1, ]
# Se elimina la primera fila para evitar nombres duplicados
ranking_by_club <- ranking_by_club[-1, ]

# Separación de tablas
primera_tabla <- ranking_by_club[, 1:13]
segunda_tabla <- ranking_by_club[,14:26]

# Unir las tablas una debajo de la otra
ranking_by_club <- rbind(primera_tabla, segunda_tabla)

# Estadísticas de la Champions más reciente del Madrid (2022)

# Ataque por jugador
attacking <- read.csv("attacking.csv") %>% 
  select(-"position", -"offsides", -"corner_taken")

# Intentos a puerta por jugador
attempts <- read.csv ("attempts.csv") %>%    
  select(-"position")

# Defensa por jugador
defending <- read.csv ("defending.csv") %>%    
  select(-"position",-"clearance_attempted")

# Pases por jugador
distributon <- read.csv ("distributon.csv") %>% 
  select(-"position", -"cross_accuracy", -"cross_attempted", -"cross_complted", -"freekicks_taken")

# Portería 
goalkeeping <- read.csv ("goalkeeping.csv") %>% 
  select(-"position",-"punches.made")

# Goles por jugador
goals <- read.csv ("goals.csv") %>% 
  select(-"position",-"right_foot",-"left_foot",-"headers", -"others", -"inside_area",-"outside_areas")

# Estadísticas clave por jugador
key_stats <- read.csv("key_stats.csv")

# Datos de entrenadores
CoachesAppearDetails <- read.csv ("CoachesAppearDetails.csv")                  

# Goleador por temporada desde 2013-2014 hasta 2022
TopGoalScorer <- read.csv("TopGoalScorer.csv") %>% 
  filter(X %in% c(24:34))

# Extraer solo los números de las columnas Goals y Appearances
TopGoalScorer$Goals <- as.numeric(gsub("[^0-9]", "", TopGoalScorer$Goals))
TopGoalScorer$Appearances <- as.numeric(gsub("[^0-9]", "", TopGoalScorer$Appearances))

# Extraer los valores de Goals y Appearances de la columna Club
TopGoalScorer$Goals <- ifelse(!is.na(str_extract(TopGoalScorer$Club, "\\d+(?=\\s*goals)")), as.numeric(str_extract(TopGoalScorer$Club, "\\d+(?=\\s*goals)")), TopGoalScorer$Goals)
TopGoalScorer$Appearances <- ifelse(!is.na(str_extract(TopGoalScorer$Club, "\\d+(?=\\s*appearances)")), as.numeric(str_extract(TopGoalScorer$Club, "\\d+(?=\\s*appearances)")), TopGoalScorer$Appearances)

# Eliminar los valores de Goals y Appearances de la columna Club
TopGoalScorer$Club <- str_replace(TopGoalScorer$Club, "\\s*\\d+\\s*goals.*", "")


# Estadísticas por equipo desde 1993 hasta 2020
ucl_stats <- read.csv("ucl_stats.csv") %>% 
  filter(year %in% c(2014:2020))

# Equipos que llegaron a cuartos desde 1981 hasta 2021. Tiene un error: Dice que Manchester City ganó (W) en el 2021
UCLQuarterFinals <- read.csv("UCLQuarterFinals.csv") %>% 
  filter(year %in% c(2014:2021)) %>% 
  select("year", "code", "name","round","league") %>% 
  rename(Teams = "name", country = "league")

# Equipos, jugadores, goles de esos jugadores y partidos jugados con sus resultados. Va desde 2016 hasta 2022

UCL_16_22_players <- read_excel("UEFA Champions League 2016-2022 Data.xlsx", sheet = "players") 

UCL_16_22_players <- UCL_16_22_players %>% 
  # Crear una nueva columna "FULL_NAME" que combine los valores de las columnas "FIRST_NAME" y "LAST_NAME"
  mutate(FULL_NAME = paste(ifelse(is.na(UCL_16_22_players$FIRST_NAME), "", UCL_16_22_players$FIRST_NAME),
                           ifelse(is.na(UCL_16_22_players$LAST_NAME), "", UCL_16_22_players$LAST_NAME))) %>% 
  # Seleccionar solo las columnas relevantes
  select("PLAYER_ID", "FULL_NAME", "TEAM") 

# Leer datos de goles de la UEFA Champions League desde 2016 hasta 2022
UCL_16_22_goals <- read_excel("UEFA Champions League 2016-2022 Data.xlsx", sheet = "goals") %>% 
  # Eliminar la columna "GOAL_DESC"
  select(-"GOAL_DESC")

# Leer datos de partidos de la UEFA Champions League desde 2016 hasta 2022
UCL_16_22_matches <- read_excel("UEFA Champions League 2016-2022 Data.xlsx", 
                                sheet = "matches") %>% 
  select(-"DATE_TIME", -"STADIUM",-"ATTENDANCE")

# Leer datos de equipos de la UEFA Champions League desde 2016 hasta 2022
UCL_16_22_teams <- read_excel("UEFA Champions League 2016-2022 Data.xlsx", 
                              sheet = "teams") %>% 
  # Eliminar la columna "HOME_STADIUM"
  select(-"HOME_STADIUM")


# Carga de estadísticas de jugadores desde el 2013 hasta 2020/2021
UCL_Player_stats <- read.csv("UEFA_CL_Player_stats.csv") 
  #sustituir NAs por 0
UCL_Player_stats[is.na(UCL_Player_stats)] <- 0

  # Creación de tabla para elegir a qué equipos vamos a utilizar:
  # UCLQuarterFinals. Contiene desde 2014, hasta 2021. Falta 2022 y 2023
  UCLQuarterFinals_21_22 <- UCL_16_22_matches %>% 
  filter(SEASON == "2021-2022") %>% 
  # Obtener aquellos equipos que llegaron a cuartos de final. El partido 125 es la final. Luego hay 4 partidos 
  # en semis. 8 partidos en cuartos. En total hay 13. 125-13=112. Pero los cuartos empizan en el 113.
  filter(MATCH_ID %in% c("mt113","mt114","mt115","mt116","mt117","mt118","mt119",
                         "mt120","mt121","mt122","mt123","mt124","mt125")) %>% 
  select("MATCH_ID","SEASON","HOME_TEAM","AWAY_TEAM","HOME_TEAM_SCORE","AWAY_TEAM_SCORE") %>% 
  rename(year = SEASON) %>% 
  mutate(year = ifelse(year == "2021-2022", "2022", NA))



# Agregar códigos, ronda y país a los equipos de la temporada 21-22
UCLQuarterFinals_21_22_teams <- UCLQuarterFinals_21_22 %>% 
  filter(MATCH_ID %in% c("mt113","mt114","mt115","mt116",
                         "mt117","mt118","mt119","mt120")) %>% 
  rename(Teams = HOME_TEAM) %>% 
  select(-"AWAY_TEAM",-"MATCH_ID",-"HOME_TEAM_SCORE", -"AWAY_TEAM_SCORE") %>% 
  mutate(code = c("MAC", "BEN", "CHE", "VIL", "RMA", "BMN", "ATM","LIV"), 
         round = c("SF", "QF", "QF", "SF", "W", "QF", "QF", "RU"), 
         country = c("England","Portugal","England","Spain", "Spain", "Germany","Spain","England")) %>% 
  select(year, code, Teams, round, country)

# Unir el database de UCLQuarterFinals con la temporada de UCLQuarterFinals_21_22_teams
UCLQuarterFinals <- UCLQuarterFinals %>% 
  rbind(UCLQuarterFinals_21_22_teams) %>% 
  mutate(Teams = case_when(
    Teams == "Atletico Madrid CF" ~ "Atlético Madrid",
    Teams == "Bayern München" ~ "FC Bayern",
    Teams == "Manchester City FC" ~ "Manchester City",
    Teams == "Real Madrid CF" ~ "Real Madrid", 
    TRUE ~ Teams
  ))

# Falta temporada 2022-2023

  #cargar el excel correspondiente y adecuar la base de datos al formato de UCLQuarterFinals

UCLQuarterFinals_22_23 <- read_excel("Top 16 Championes League 22 y 23.xlsx")%>% 
  filter(as.numeric(Partidos_Jugados) >= 10) %>% 
  rename(Teams = Club) %>%
  mutate(code = c("MAC", "INT", "RMA", "ACM", "BMN", "NAP", "BEN","CHE"), 
         round = c("W", "RU", "SF", "SF", "QF", "QF", "QF", "QF"), 
         country = c("England","Italy","Spain","Italy", "Germany", "Italy","Portugal","England"),
         year = 2023) %>%
  select(year, code, Teams, round, country, -Partidos_Jugados, -Ganados, -Empates, -Perdidos)


#Unir base de datos modificada de 2023 a QuarterFinals para completar los años

UCLQuarterFinals <- UCLQuarterFinals %>%
  rbind(UCLQuarterFinals_22_23) %>%
  mutate(Teams = case_when(
    Teams == "Benfica" ~ "SL Benfica",
    Teams == "Bayern" ~ "FC Bayern",
    Teams == "Man City" ~ "Manchester City",
    Teams == "Chelsea" ~ "Chelsea FC", 
    TRUE ~ Teams
  ))

#Escribir las bases de datos a Excel 


# Crear una lista de las bases de datos
tablas <- list(
  UCLQuarterFinals = UCLQuarterFinals,
  attacking = attacking,
  attempts = attempts,
  defending = defending,
  distributon = distributon,
  goalkeeping = goalkeeping,
  goals = goals,
  key_stats = key_stats,
  ranking_by_club = ranking_by_club,
  ranking_by_country = ranking_by_country,
  CoachesAppearDetails = CoachesAppearDetails,
  TopGoalScorer = TopGoalScorer,
  UCL_16_22_goals = UCL_16_22_goals,
  UCL_16_22_matches = UCL_16_22_matches,
  UCL_16_22_players = UCL_16_22_players,
  UCL_Player_stats = UCL_Player_stats,
  ucl_stats = ucl_stats
  )

# Crear un libro de trabajo
wb <- createWorkbook()

# Hacer un loop en las tablas y agregarlas al libro de trabajo
for (i in seq_along(tablas)) {
  addWorksheet(wb, sheetName = names(tablas)[i])
  writeData(wb, sheet = i, x = tablas[[i]])
}

# Save the workbook to a file
saveWorkbook(wb, file = "Proyecto_final_RM.xlsx")


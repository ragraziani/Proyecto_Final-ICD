# Carga de librerias

library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(openxlsx)
library(stringr)

# Leer bases de datos
# Se establece como criterio usar data desde 2013 en adelante, ya que desde ese entonces,
# el Madrid ha destacado mucho en la competicion

# Estadisticas por pais
ranking_by_country <- read.csv("AllTimeRankingByCountry.csv") %>% 
  
  # Al explorar la data, se procede a limpiarla
  select(-"X") %>% 
  
  # Se seleccionan aquellos paises que hayan ganado al menos 1 vez el titulo
  filter(Titles != 0)


# Estadisticas por club
ranking_by_club <- read_excel("Rankings by club.xlsx", sheet = 1)


# Eliminar fila con NAs
ranking_by_club <- na.omit(ranking_by_club)

# Se cambian los nombres de las columnas con los caracteres de la primera fila
colnames(ranking_by_club) <- ranking_by_club[1, ]

# Se elimina la primera fila para evitar nombres duplicados
ranking_by_club <- ranking_by_club[-1, ]

# Separacion de tablas
primera_tabla <- ranking_by_club[, 1:13]
segunda_tabla <- ranking_by_club[,14:26]

# Unir las tablas una debajo de la otra
ranking_by_club <- rbind(primera_tabla, segunda_tabla) %>% 
  mutate(Club = case_when(
    Club == "Club Atlético de Madrid" ~ "Atletico Madrid",
    Club == "FC Bayern München" ~ "FC Bayern",
    Club == "Manchester City FC" ~ "Manchester City",
    Club == "Real Madrid CF" ~ "Real Madrid", 
    TRUE ~ Club
  ))

# Estadisticas de la Champions mas reciente del Madrid (2022)

# Ataque por jugador
attacking <- read.csv("attacking.csv") %>% 
  select(-"position", -"offsides", -"corner_taken") %>% 
  mutate(club = case_when(
    club == "Atlético" ~ "Atletico Madrid",
    club == "Bayern" ~ "FC Bayern",
    club == "Man. City" ~ "Manchester City",
    club == "Barcelona" ~ "FC Barcelona", 
    TRUE ~ club
  ))

# Intentos a puerta por jugador
attempts <- read.csv ("attempts.csv") %>%    
  select(-"position")%>% 
  mutate(club = case_when(
    club == "Atlético" ~ "Atletico Madrid",
    club == "Bayern" ~ "FC Bayern",
    club == "Man. City" ~ "Manchester City",
    club == "Barcelona" ~ "FC Barcelona", 
    TRUE ~ club
  ))

# Defensa por jugador
defending <- read.csv ("defending.csv") %>%    
  select(-"position",-"clearance_attempted")%>% 
  mutate(club = case_when(
    club == "Atlético" ~ "Atletico Madrid",
    club == "Bayern" ~ "FC Bayern",
    club == "Man. City" ~ "Manchester City",
    club == "Barcelona" ~ "FC Barcelona", 
    TRUE ~ club
  ))

# Pases por jugador
distributon <- read.csv ("distributon.csv") %>% 
  select(-"position", -"cross_accuracy", -"cross_attempted", -"cross_complted", -"freekicks_taken")%>% 
  mutate(club = case_when(
    club == "Atlético" ~ "Atletico Madrid",
    club == "Bayern" ~ "FC Bayern",
    club == "Man. City" ~ "Manchester City",
    club == "Barcelona" ~ "FC Barcelona", 
    TRUE ~ club
  ))

# Porteria 
goalkeeping <- read.csv ("goalkeeping.csv") %>% 
  select(-"position",-"punches.made")%>% 
  mutate(club = case_when(
    club == "Atlético" ~ "Atletico Madrid",
    club == "Bayern" ~ "FC Bayern",
    club == "Man. City" ~ "Manchester City",
    club == "Barcelona" ~ "FC Barcelona", 
    TRUE ~ club
  ))

# Goles por jugador
goals <- read.csv ("goals.csv") %>% 
  select(-"position",-"right_foot",-"left_foot",-"headers", -"others", -"inside_area",-"outside_areas")%>% 
  mutate(club = case_when(
    club == "Atlético" ~ "Atletico Madrid",
    club == "Bayern" ~ "FC Bayern",
    club == "Man. City" ~ "Manchester City",
    club == "Barcelona" ~ "FC Barcelona", 
    TRUE ~ club
  ))

# Estadisticas clave por jugador
key_stats <- read.csv("key_stats.csv")%>% 
  mutate(club = case_when(
    club == "Atlético" ~ "Atletico Madrid",
    club == "Bayern" ~ "FC Bayern",
    club == "Man. City" ~ "Manchester City",
    club == "Barcelona" ~ "FC Barcelona", 
    TRUE ~ club
  ))

# Datos de entrenadores
Coaches_Appear_Details <- read.csv ("CoachesAppearDetails.csv") %>% 
  mutate(Club = case_when(
    Club == "Club Atlético de Madrid" ~ "Atletico Madrid",
    Club == "FC Bayern München" ~ "FC Bayern",
    Club == "Manchester City FC" ~ "Manchester City",
    Club == "Real Madrid CF" ~ "Real Madrid", 
    TRUE ~ Club
  ))                 

# Goleador por temporada desde 2013-2014 hasta 2022
Top_Goal_Scorer <- read.csv("TopGoalScorer.csv") %>% 
  filter(X %in% c(24:34)) %>% 
  select(-"X")%>% 
  mutate(Club = case_when(
    Club == "FC Bayern München" ~ "FC Bayern",
    Club == "Real Madrid CF" ~ "Real Madrid", 
    TRUE ~ Club
  ))                 


# Extraer solo los numeros de las columnas Goals y Appearances
Top_Goal_Scorer$Goals <- as.numeric(gsub("[^0-9]", "", Top_Goal_Scorer$Goals))

Top_Goal_Scorer$Appearances <- as.numeric(gsub("[^0-9]", "", Top_Goal_Scorer$Appearances))

# Extraer los valores de Goals y Appearances de la columna Club
Top_Goal_Scorer$Goals <- ifelse(!is.na(str_extract(Top_Goal_Scorer$Club, "\\d+(?=\\s*goals)")), 
                                as.numeric(str_extract(Top_Goal_Scorer$Club, "\\d+(?=\\s*goals)")), Top_Goal_Scorer$Goals)

Top_Goal_Scorer$Appearances <- ifelse(!is.na(str_extract(Top_Goal_Scorer$Club, "\\d+(?=\\s*appearances)")), 
                                      as.numeric(str_extract(Top_Goal_Scorer$Club, "\\d+(?=\\s*appearances)")), Top_Goal_Scorer$Appearances)

# Eliminar los valores de Goals y Appearances de la columna Club
Top_Goal_Scorer$Club <- str_replace(Top_Goal_Scorer$Club, "\\s*\\d+\\s*goals.*", "")


# Estadisticas por equipo desde 1993 hasta 2020
UCL_Stats <- read.csv("ucl_stats.csv") %>% 
  filter(year %in% c(2014:2020))%>%
  rename(club = "team") %>% 
  mutate(club = case_when(
    club == "Bayern Munich" ~ "FC Bayern",
    club == "Barcelona" ~ "FC Barcelona", 
    TRUE ~ club
  ))  

# Equipos que llegaron a cuartos desde 1981 hasta 2021. Tiene un error: Dice que Manchester City gano (W) en el 2021
UCL_Quarter_Finals <- read.csv("UCLQuarterFinals.csv") %>% 
  filter(year %in% c(2014:2021)) %>% 
  select("year", "code", "name","round","league") %>% 
  rename(club = "name", country = "league")

# Equipos, jugadores, goles de esos jugadores y partidos jugados con sus resultados. Va desde 2016 hasta 2022

UCL_16_22_players <- read_excel("UEFA Champions League 2016-2022 Data.xlsx", sheet = "players") 

UCL_16_22_players <- UCL_16_22_players %>% 
  # Crear una nueva columna "FULL_NAME" que combine los valores de las columnas "FIRST_NAME" y "LAST_NAME"
  mutate(FULL_NAME = paste(ifelse(is.na(UCL_16_22_players$FIRST_NAME), "", UCL_16_22_players$FIRST_NAME),
                           ifelse(is.na(UCL_16_22_players$LAST_NAME), "", UCL_16_22_players$LAST_NAME))) %>% 
  # Seleccionar solo las columnas relevantes
  select("PLAYER_ID", "FULL_NAME", "TEAM")%>% 
  mutate(TEAM = case_when(
    TEAM == "Atlético Madrid" ~ "Atletico Madrid",
    TEAM == "Bayern München" ~ "FC Bayern",
    TRUE ~ TEAM
  ))  
  

# Leer datos de goles de la UEFA Champions League desde 2016 hasta 2022
UCL_16_22_goals <- read_excel("UEFA Champions League 2016-2022 Data.xlsx", sheet = "goals") %>% 
  # Eliminar la columna "GOAL_DESC"
  select(-"GOAL_DESC")

# Leer datos de partidos de la UEFA Champions League desde 2016 hasta 2022
UCL_16_22_matches <- read_excel("UEFA Champions League 2016-2022 Data.xlsx", 
                                sheet = "matches")
#Modificar la columna de fechas y crear una nueva con el fin de poder organizarlas 
UCL_16_22_matches$DATE <- substr(UCL_16_22_matches$DATE_TIME, 1, 9)

UCL_16_22_matches$DATE <- as.Date(UCL_16_22_matches$DATE, 
                                  format = "%d-%b-%y")

UCL_16_22_matches <- UCL_16_22_matches %>% 
  arrange(DATE)

UCL_16_22_matches <- UCL_16_22_matches %>% 
    select(-"DATE_TIME", -"STADIUM",-"ATTENDANCE")

# Leer datos de equipos de la UEFA Champions League desde 2016 hasta 2022
UCL_16_22_teams <- read_excel("UEFA Champions League 2016-2022 Data.xlsx", 
                              sheet = "teams") %>% 
  # Eliminar la columna "HOME_STADIUM"
  select(-"HOME_STADIUM")%>% 
  mutate(TEAM_NAME = case_when(
    TEAM_NAME == "Atlético Madrid" ~ "Atletico Madrid",
    TEAM_NAME == "Bayern München" ~ "FC Bayern",
    TRUE ~ TEAM_NAME
  )) 

# Creacion de tabla para elegir a que equipos vamos a utilizar:
  # UCLQuarterFinals. Contiene desde 2014, hasta 2021. Falta 2022 y 2023
  UCL_Quarter_Finals_21_22 <- UCL_16_22_matches %>% 
  filter(SEASON == "2021-2022") %>% 
    
# Obtener aquellos equipos que llegaron a cuartos de final. El partido 125 es la final. Luego hay 4 partidos 
# en semis. 8 partidos en cuartos. En total hay 13. 125-13=112. Pero los cuartos empiezan en el 113.
  filter(MATCH_ID %in% c("mt113","mt114","mt115","mt116","mt117","mt118","mt119",
                         "mt120","mt121","mt122","mt123","mt124","mt125")) %>% 
  select("MATCH_ID","SEASON","HOME_TEAM","AWAY_TEAM","HOME_TEAM_SCORE","AWAY_TEAM_SCORE") %>% 
  rename(year = SEASON) %>% 
  mutate(year = ifelse(year == "2021-2022", "2022", NA))



# Agregar codigos, ronda y pais a los equipos de la temporada 21-22
UCL_Quarter_Finals_21_22 <- UCL_Quarter_Finals_21_22 %>% 
  filter(MATCH_ID %in% c("mt113","mt114","mt115","mt116",
                         "mt117","mt118","mt119","mt120")) %>% 
  rename(club = HOME_TEAM) %>% 
  select(-"AWAY_TEAM",-"MATCH_ID",-"HOME_TEAM_SCORE", -"AWAY_TEAM_SCORE") %>% 
  mutate(code = c("MAC", "BEN", "CHE", "VIL", "RMA", "BMN", "ATM","LIV"), 
         round = c("SF", "QF", "QF", "SF", "W", "QF", "QF", "RU"), 
         country = c("England","Portugal","England","Spain", "Spain", "Germany","Spain","England"),
         club = case_when(
           club == "Atlético Madrid" ~ "Atletico Madrid",
           TRUE ~ club)) %>% 
  select(year, code, club, round, country)

# Unir el database de UCLQuarterFinals con la temporada de UCLQuarterFinals_21_22_teams
UCL_Quarter_Finals <- UCL_Quarter_Finals %>% 
  rbind(UCL_Quarter_Finals_21_22) %>% 
  mutate(club = case_when(
    club == "Atletico Madrid CF" ~ "Atletico Madrid",
    club == "Bayern München" ~ "FC Bayern",
    club == "Manchester City FC" ~ "Manchester City",
    club == "Real Madrid CF" ~ "Real Madrid", 
    TRUE ~ club
  ))

# Falta temporada 2022-2023

  #cargar el excel correspondiente y adecuar la base de datos al formato de UCLQuarterFinals

UCL_Quarter_Finals_22_23 <- read_excel("Top 16 Championes League 22 y 23.xlsx")%>% 
  filter(as.numeric(Partidos_Jugados) >= 10) %>% 
  rename(club = Club) %>%
  mutate(code = c("MAC", "INT", "RMA", "ACM", "BMN", "NAP", "BEN","CHE"), 
         round = c("W", "RU", "SF", "SF", "QF", "QF", "QF", "QF"), 
         country = c("England","Italy","Spain","Italy", "Germany", "Italy","Portugal","England"),
         year = 2023) %>%
  select(year, code, club, round, country, -Partidos_Jugados, -Ganados, -Empates, -Perdidos)


#Unir base de datos modificada de 2023 a QuarterFinals para completar los anos

UCL_Quarter_Finals <- UCL_Quarter_Finals %>%
  rbind(UCL_Quarter_Finals_22_23) %>%
  mutate(club = case_when(
    club == "Benfica" ~ "SL Benfica",
    club == "Bayern" ~ "FC Bayern",
    club == "Man City" ~ "Manchester City",
    club == "Chelsea" ~ "Chelsea FC", 
    TRUE ~ club
  ))

#Asegurarse que las tablas tengan sus columnas numericas en num
ranking_by_club[, c("Pos", "Part", "Titles", "Pld", 
                    "W", "D", "L", "F", "A", "Pts", "GD")] <- lapply(
                      ranking_by_club[, c("Pos", "Part", "Titles", 
                                          "Pld", "W", "D", "L", "F", 
                                          "A", "Pts", "GD")], as.numeric)




#Escribir las bases de datos a Excel 


# Crear una lista de las bases de datos
tablas <- list(
  UCL_Quarter_Finals = UCL_Quarter_Finals,
  attacking = attacking,
  attempts = attempts,
  defending = defending,
  distributon = distributon,
  goalkeeping = goalkeeping,
  goals = goals,
  key_stats = key_stats,
  ranking_by_club = ranking_by_club,
  ranking_by_country = ranking_by_country,
  Coaches_Appear_Details = Coaches_Appear_Details,
  Top_Goal_Scorer = Top_Goal_Scorer,
  UCL_16_22_goals = UCL_16_22_goals,
  UCL_16_22_matches = UCL_16_22_matches,
  UCL_16_22_players = UCL_16_22_players,
  UCL_Stats = UCL_Stats
  )

# Crear un libro de trabajo
wb <- createWorkbook()

# Hacer un loop en las tablas y agregarlas al libro de trabajo
for (i in seq_along(tablas)) {
  addWorksheet(wb, sheetName = names(tablas)[i])
  writeData(wb, sheet = i, x = tablas[[i]])
}

# Guardar a un archivo de excel

setwd("C:/Users/Documents")

saveWorkbook(wb, file = "PF_BD_RMarkdown.xlsx")




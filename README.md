# FIFA_2021_DATA_CLEANING_CHALLENGE

![FIFA_21-1](https://user-images.githubusercontent.com/118398053/228782777-e8018842-b754-4a00-91b0-df15ab468c43.jpg)

## INTRODUCTION

One of the most important step in data analysis process is data cleaning, recently i participated in a data cleaning challenge organized in the data-tech space, the project was aimed at transforming FIFA 2021 messy data into clean and usable data that is ready for analysis. I transformed the data using R language and i will be sharing my process with you in this document.

#### DATA DESCRIPTION

The raw FIFA 21 dataset was sourced from [kaggle](https://www.kaggle.com/datasets/yagunnersya/fifa-21-messy-raw-dataset-for-cleaning-exploring),
the dataset obtained via web scrapping from sofifa.com contains two CSV files namely fifa21 raw data v2.csv and fifa21_raw_data.csv the former was 
used for the sake of this project. The fifa21 raw data v2.csv file contains 18,979 rows and 77 columns, it contains information about football players,
their abilities, and performance updated up till 2021.

#### DATA CLEANING GOAL

The goal of this challenge was to clean the dataset and make it ready for analysis and visualization

#### Data CLEANING OBJECTIVES

Look out for, Incorrect data types, Null entries, Missing values, Duplicate entries, Errors in spellings and values, Wrong calculations across rows and columns, Irrelevant data,  Outliers.

#### LOAD PACKAGES

Like i mentioned earlier the tool i used for this data cleaning process is R language. The packages used is show below
```R
install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(janitor)
library(scales)
```
#### LOAD DATASET
Loaded the dataset using the code below
![Dataset_raw](https://user-images.githubusercontent.com/118398053/228937986-9d05b6c7-286f-488c-8e5a-f4fad8ee4d1a.png)

```R
fifa_data_raw <- read.csv("fifa21_raw_remi.csv", header = TRUE)
```

#### KICKSTART CLEANING 
First i changed the column names to uppercase(just my preference) and removed the duplicate column
| Before  |  After                           |                                 
  ---------------------------:|:-----------------------------------
  ![Uppercase](https://user-images.githubusercontent.com/118398053/228939557-9f33cace-421f-4979-a189-e9e4a391757c.png)|![Uppercase_done](https://user-images.githubusercontent.com/118398053/228939591-80977690-c0f2-4bd2-a86b-48930f5e1703.png)

```R
fifa21 <- rename_with(fifa_data_raw, toupper) 
fifa21_<- select(fifa21, -c(NAME))
```

#### RENAME AND RE-ARRANGE COLUMN
Some column names were not named properly and some were in abbrevations. For easy understanding i renamed the columns and arranged them properly.
|         Before     |          After                |                                 
  ---------------------:|:----------------------
  ![Uppercase_done](https://user-images.githubusercontent.com/118398053/228941481-be344bb1-5766-4ee1-b2f9-8b48f0df392d.png)|![arranged](https://user-images.githubusercontent.com/118398053/228941565-46e7deec-1d3a-4b9a-b85b-1892e753a84d.png)

```R
fifa21_rename<- fifa21_ %>% rename(FULL_NAME=LONGNAME, PHOTO_URL=PHOTOURL, PLAYER_URL=PLAYERURL, PREFERRED_FOOT=PREFERRED.FOOT, BEST_POSITION=BEST.POSITION,  RELEASE_CLAUSE=RELEASE.CLAUSE, HEADING_ACCURACY=HEADING.ACCURACY, SHORT_PASSING=SHORT.PASSING, FREEKICK_ACCURACY=FK.ACCURACY, LONG_PASSING=LONG.PASSING, BALL_CONTROL=BALL.CONTROL, LONG_SHOTS=LONG.SHOTS, SPRINT_SPEED=SPRINT.SPEED, SHOT_POWER=SHOT.POWER, STANDING_TACKLE=STANDING.TACKLE, SLIDING_TACKLE=SLIDING.TACKLE, GK_DIVING=GK.DIVING,GK_HANDLING=GK.HANDLING,GK_KICKING=GK.KICKING,GK_POSITIONING=GK.POSITIONING,GK_REFLEXES=GK.REFLEXES, TOTAL_STATS=TOTAL.STATS, BASE_STATS=BASE.STATS, ATTACKING_WORKRATE=A.W, DEFENSIVE_WORKRATE=D.W, PACE=PAC, WEAK_FOOT=W.F, SKILL_MOVES_RATING=SM, INJURY_RATING=IR, SHOOTING_ATTRIBUTE=SHO, PASS_ACCURACY=PAS, LOAN_END=LOAN.DATE.END)

fifa21_org <- fifa21_rename %>%
  select(ID, FULL_NAME,NATIONALITY,AGE,HEIGHT,WEIGHT,PREFERRED_FOOT,JOINED, CLUB, CONTRACT, LOAN_END, VALUE, WAGE, RELEASE_CLAUSE,
         POSITIONS, BEST_POSITION, ATTACKING, CROSSING, FINISHING, HEADING_ACCURACY, SHORT_PASSING, VOLLEYS, SKILL, 
         DRIBBLING, CURVE, FREEKICK_ACCURACY, LONG_PASSING, PASS_ACCURACY, BALL_CONTROL, MOVEMENT, ACCELERATION, 
         SPRINT_SPEED, PACE, AGILITY, REACTIONS, BALANCE, POWER, SHOT_POWER,SHOOTING_ATTRIBUTE, JUMPING, STAMINA, STRENGTH, 
         LONG_SHOTS, MENTALITY, AGGRESSION, INTERCEPTIONS, POSITIONING, VISION, PENALTIES, COMPOSURE, 
         DEFENDING, MARKING, STANDING_TACKLE, SLIDING_TACKLE, GOALKEEPING, GK_DIVING, GK_HANDLING, 
         GK_KICKING, GK_POSITIONING, GK_REFLEXES,HITS, BASE_STATS, TOTAL_STATS, ATTACKING_WORKRATE, DEFENSIVE_WORKRATE, WEAK_FOOT, SKILL_MOVES_RATING, INJURY_RATING, PLAYER_URL, PHOTO_URL)
```
#### LONGNAME COLUMN
This column has already been renamed to Full name. The column contains spaces and non english letters. I had to change to english letters and replace spaces with underscores
|       Before     |        After                |                                 
  ---------------------:|:----------------------
  ![name](https://user-images.githubusercontent.com/118398053/228943592-65aa839b-4916-443e-901e-fb4c64e5d67a.png)|![name_cleaned](https://user-images.githubusercontent.com/118398053/228943686-360d1a99-5df6-4852-a866-e4686c54a731.png)

```R
fifa21_org <- fifa21_org |>
  mutate(FULL_NAME = str_remove_all(FULL_NAME, "\\n") |> stri_trans_general("Latin-ASCII"))
  
fifa21_org$FULL_NAME <- gsub(" ", "_", fifa21_org$FULL_NAME)
```
#### CLUB COLUMN
The club column had numbers attached to it and it had open spaces. I removed the numbers and replaced spaces with underscore.
|       Before     |            After                |                                 
  ---------------------:|:----------------------
  ![club](https://user-images.githubusercontent.com/118398053/228947897-9308f3ee-8076-445e-b6aa-331360daece5.png)|![club_cleaned](https://user-images.githubusercontent.com/118398053/228947963-8b2056e3-c400-46d4-ad9a-d2c91201ba57.png)

```R
fifa21_org <- fifa21_org |>
  mutate(CLUB = str_remove_all(CLUB, "\\n") |> stri_trans_general("Latin-ASCII"))
  
fifa21_org$CLUB <- gsub(" ", "_", fifa21_org$CLUB)
  
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FC_Heidenheim_1846")] <- "FC_Heidenheim_1846"
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FC_Kaiserslautern")] <- "FC_Kaiserslautern"
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FC_Koln")] <- "FC_Koln"
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FC_Magdeburg")] <- "FC_Magdeburg"
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FC_Nurnberg")] <- "FC_Nurnberg"
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FC_Saarbrucken")] <- "FC_Saarbrucken"
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FC_Union_Berlin")] <-"FC_Union_Berlin"
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FSV_Mainz_05")] <- "FSV_Mainz_05"
fifa21_org$CLUB[fifa21_org$CLUB %in% c("1._FC_Saarbrucken")] <- "FC_Saarbrucken"
```
#### HEIGHT AND WEIGHT COLUMN
The height column contianed observations in two different scales in the same column feet and inches, while the weight were in lbs and kg. To solve this i used excel to convert all to Cm and Kg before exporting the dataset to R-studio. After which i had to remove the cm and kg scales from the observations, change the data type to numeric andrename the column name.
|         Before     |        After                |                                 
  ---------------------:|:----------------------
  ![height_weight](https://user-images.githubusercontent.com/118398053/228952219-d533483b-3f29-46aa-85cb-8db0c6ac8e63.png)|![height_weight_cleaned](https://user-images.githubusercontent.com/118398053/228952299-73222011-0dcb-4097-851e-e78d7a2e59e3.png)

```R
fifa21_org$HEIGHT <- gsub("cm", "", fifa21_org$HEIGHT)
fifa21_org$HEIGHT <- gsub("m", "", fifa21_org$HEIGHT)
fifa21_org$WEIGHT <- gsub("kg", "", fifa21_org$WEIGHT)

fifa21_org$HEIGHT_CM <- as.numeric(fifa21_org$HEIGHT_CM)
fifa21_org$WEIGHT_KG <- as.numeric(fifa21_org$WEIGHT_KG)

fifa21_org <- fifa21_org %>% rename(WEIGHT_KG=WEIGHT, HEIGHT_CM=HEIGHT)
```
#### WAGES, VALUE & RELEASE_CLAUSE COLUMN




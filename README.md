# FIFA_2021_DATA_CLEANING_CHALLENGE

![FIFA21new](https://user-images.githubusercontent.com/118398053/230074879-1460cd6d-d312-4a9d-a38b-9dac450ffa2a.jpg)


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
These columns had currency sign and letter 'K & M' that needed to be removed before converting to numeric. I used the gsub function to remove the currency sign and created a function to convert the numbers to thousands and millions, because the K & M stood for thousand and million. After creating the function, i then applied it to each of the columns.
|         Before     |          After                |                                 
  ---------------------:|:----------------------
  ![wages](https://user-images.githubusercontent.com/118398053/230062606-f8ac98a4-f195-4425-8356-7d99642357f0.png)|![wages_clean](https://user-images.githubusercontent.com/118398053/230062689-7c8fa79a-86ed-43a3-acfe-4a129eee9b85.png)

```R
fifa21_org2$VALUE <- gsub("€", "", fifa21_org2$VALUE)
fifa21_org2$RELEASE_CLAUSE <- gsub("€", "", fifa21_org2$RELEASE_CLAUSE)
fifa21_org2$WAGE <- gsub("€", "", fifa21_org2$WAGE)
convert_value <- function(value) {
  value_without_suffix <- gsub("[KM]", "", value)
  value_as_number <- as.numeric(value_without_suffix)
  if (grepl("K", value)) {
    value_as_number <- value_as_number *1000
  } else if (grepl("M", value)) {
    value_as_number <- value_as_number *1000000 } 
  return(value_as_number)
}
fifa21_org2$VALUE <-sapply(fifa21_org2$VALUE, convert_value)
fifa21_org2$WAGE <-sapply(fifa21_org2$WAGE, convert_value)
fifa21_org2$RELEASE_CLAUSE <-sapply(fifa21_org2$RELEASE_CLAUSE, convert_value)
```
#### CONTRACT & LOAN.DATE.END COLUMN
The contract column had observations in different date formats. The word 'free' in the contract column means the player has no contract and there has no club. In order not to introduce N/A to the column players with 'free' in the contract column were removed and the contract column was split into 2, contract_start and contract_end for easy understanding. Also another column was created contract_type to describe the type of contract a player had. After splitting the column, contract_start was splitted into contract_start and contract_type, gsub was then used to remove irrelevant data from loan.date.end and contract_type. Loan_end was then merged with contract_end.
|           Before     |            After                |                                 
  ---------------------:|:----------------------
  ![Contract](https://user-images.githubusercontent.com/118398053/230066998-7d324d51-a8e6-4ac4-a8b1-a24f8b980b59.png)|![contract_cleaned](https://user-images.githubusercontent.com/118398053/230067098-2b1c3d47-13bb-41e3-9e31-f82a6b23707d.png)

```R
fifa21_org2 <- subset(fifa21_org2, CLUB != "No_Club")

fifa21_org22 <- separate(fifa21_org22, CONTRACT, into = c("CONTRACT_START", "CONTRACT_END"), sep = "~")
fifa21_org22 <- separate(fifa21_org22, CONTRACT_START, into = c("CONTRACT_START", "CONTRACT_TYPE"), sep = ",")
fifa21_org22$CONTRACT_TYPE <- gsub("2023", "", fifa21_org22$CONTRACT_TYPE)
fifa21_org22$CONTRACT_TYPE <- gsub("2022", "", fifa21_org22$CONTRACT_TYPE)
fifa21_org22$CONTRACT_TYPE <- gsub("2021", "", fifa21_org22$CONTRACT_TYPE)
fifa21_org22$CONTRACT_TYPE <- gsub("2020", "", fifa21_org22$CONTRACT_TYPE)
fifa21_org22$CONTRACT_TYPE <- gsub("On", "", fifa21_org22$CONTRACT_TYPE)
fifa21_org22$CONTRACT_START <- gsub("Aug 1", "", fifa21_org22$CONTRACT_START)
fifa21_org22$CONTRACT_START[fifa21_org22$CONTRACT_START %in% c("7")] <- "2021"
fifa21_org222$CONTRACT_TYPE[is.na(fifa21_org222$CONTRACT_TYPE)] <- "Permanent"
fifa21_org222$CONTRACT_START <- paste(fifa21_org222$LOAN_END, fifa21_org222$CONTRACT_START)
fifa21_org222$CONTRACT_END <- ifelse(is.na(fifa21_org222$CONTRACT_END), fifa21_org222$LOAN_END, paste(
                                fifa21_org222$CONTRACT_END, fifa21_org222$LOAN_END))
fifa21_cleaned1$CONTRACT_START1 <- as.Date(paste0(fifa21_cleaned1$CONTRACT_START1, "-01-01"))
fifa21_cleaned1$CONTRACT_END1 <- as.Date(paste0(fifa21_cleaned1$CONTRACT_END1, "-01-01"))
```
#### HITS, WF,SM & IR COLUMN
The HITS column had missing values, so the rows that had missing values were all given '0' as their value. The WF, SM & IR had special characters. Those were removed and all columns data type were changed to numeric.
|          Before     |         After                |                                 
  ---------------------:|:----------------------
  ![HITS](https://user-images.githubusercontent.com/118398053/230070478-1134f827-8e1d-429a-a9de-50bf26f0ee60.png)|![HITS_cleaned](https://user-images.githubusercontent.com/118398053/230070596-ea1d9c03-f9b5-421e-aa1d-efa1de189eff.png)

```R
fifa21_cleaned1$HITS <- as.numeric(fifa21_cleaned1$HITS)
fifa21_cleaned1$HITS[is.na(fifa21_cleaned1$HITS)] <- "0"
fifa21_cleaned1$WEAK_FOOT <- gsub("★", "", fifa21_cleaned1$WEAK_FOOT)
fifa21_cleaned1$SKILL_MOVES_RATING <- gsub("★", "", fifa21_cleaned1$SKILL_MOVES_RATING)
fifa21_cleaned1$INJURY_RATING <- gsub("★", "", fifa21_cleaned1$INJURY_RATING)
fifa21_cleaned1$WEAK_FOOT <- as.numeric(fifa21_cleaned1$WEAK_FOOT)
fifa21_cleaned1$SKILL_MOVES_RATING <- as.numeric(fifa21_cleaned1$SKILL_MOVES_RATING)
fifa21_cleaned1$INJURY_RATING <- as.numeric(fifa21_cleaned1$INJURY_RATING)
```
#### X.OVA, POT, BOV, DRI, PHY, PAC, SHO, & DEF COLUMN
In my own view all this columns are meant to be in percentages. Why because for example the dataset cant have 2 dribbling columns but in this data set it has 2. which means 1 of the columns shows the overall dribbling attribute of the player and not just his/her dribbling skill alone. Meaning Column DRI- shows the overall dribbling attribute of the player and column Dribbling- shows the dribbling skill of the player alone. I used the paste0 function to paste the % sign and used the rename function to rename the columns for better understanding.
|          Before     |           After                |                                 
  ---------------------:|:----------------------
![Percentage](https://user-images.githubusercontent.com/118398053/230073221-b3cd35b1-dd3a-4ec8-b5c8-94bde283d0ff.png)|![Percentage_cleaned](https://user-images.githubusercontent.com/118398053/230073327-413f02bf-ed23-4663-8125-c39243d446a7.png)

```R
fifa21_clean$X.OVA <- paste0(fifa21_clean$X.OVA, "%")
fifa21_clean$BOV <- paste0(fifa21_clean$BOV, "%") 
fifa21_clean$POT <- paste0(fifa21_clean$POT, "%")
fifa21_clean$DRI <- paste0(fifa21_clean$DRI, "%")
fifa21_clean$PHY <- paste0(fifa21_clean$PHY, "%")
fifa21_clean$DEF <- paste0(fifa21_clean$DEF, "%")
fifa21_clean$PACE <- paste0(fifa21_clean$PACE, "%")
fifa21_clean$SHOOTING_ATTRIBUTE <- paste0(fifa21_clean$SHOOTING_ATTRIBUTE, "%")
fifa21_clean <- fifa21_clean %>% rename(OVERALL_RATING=X.OVA, POTENTIAL_RATING=POT, 
                                        BEST_OVERALL_RATING=BOV, DRIBBLING_ATTRIBUTE=DRI, PHYSICAL_ATTRIBUTE=PHY,
                                        DEFENSIVE_ATTRIBUTE=DEF)
```
#### CONCLUSION
This indeed was a really messy dataset. It was indeed a challenge as i had to do alot of research to solve some of the problems i encountered. I want to say Thank you to the organizers of this chanllenge and we await for more challenges like this. The dataset is now cleaned and ready for further analysis.

Click on the link below to download the raw dataset file

Raw data- [fifa21 raw data v2.csv](https://github.com/RemigiusMokwe/FIFA_2021_DATA_CLEANING/files/11159147/fifa21.raw.data.v2.csv)


cleaned data- [fifa21_clean_remi.csv](https://github.com/RemigiusMokwe/FIFA_2021_DATA_CLEANING/files/11159170/fifa21_clean_remi.csv)


ADDING HERE TO PRACTICE
install.packages(c('RSQLite', 'dplyr', 'tidyr', 'ggplot2'))
library(RSQLite)
conn = dbConnect(SQLite(), dbname='/Users/NicholasMedina/Desktop/survey.sqlite') #import db
tables = dbListTables(conn)
tables
class(tables)
surveys = dbGetQuery(conn, 'SELECT * FROM surveys')
head(surveys)
summary(surveys)
surveys = dbGetQuery(conn, 'SELECT * FROM surveys
                     JOIN species ON surveys.species_id = species.species_id
                     JOIN plots ON surveys.plot_id = plots.plot_id;')
#join requires common column

surveys = read.csv('/Users/NicholasMedina/Desktop/ecology.csv')
#lists can hold multiple data types & col/vector lengths
#data frame is a list of vectors
str(surveys)

class(surveys$year) # $ gives a vector
head(surveys$year)

class(surveys['year']) #gives a data frame
head(surveys['year'])

class(surveys[,'year']) #gives a vector

class(surveys[,4])

class(surveys[['year']])

#max/min can be called on ordered factors

tabulation = table(surveys$taxa) #total count per factor level
tabulation
barplot(table(surveys$taxa))
surveys$taxa = ordered(surveys$taxa, levels = c('Rodent', 'Bird', 'Rabbit', 'Reptile'))
table(surveys$year, surveys$taxa)
with(surveys, table(year, taxa))

dbDisconnect(conn)
rm(conn)
order(surveys$weight) #ordered indices
sort(surveys$weight) #values in order


#What was the median weight of each rodent species b/w 1980 and 1990?

surveys$taxa == 'Rodent'
length(surveys$taxa == 'Rodent')
dim(surveys)
surveys[surveys$taxa == 'Rodent', 'taxa']
surveys[surveys$taxa == 'Rodent' & surveys$year %in% seq.int(1980, 1990), 'year']


library(dplyr)
output = select(surveys, year, taxa, weight)
head(output)
filter(surveys, taxa == 'Rodent')
#intermediate variables clutter workspace (take up RAM)
rodent_surveys = surveys %>%
  filter(taxa == 'Rodent') %>%
  select(year, taxa, weight)

#subset surveys to only Rodent surveys b/w 1980 and 1990
rodent_surveys = surveys %>%
  filter(taxa == 'Rodent' & year %in% seq.int(1980,1990)) %>%
  select(year, taxa, weight)
rodent_surveys
#all.equal to check if 2 outputs are the same

surveys %>%
  mutate(weight_kg = weight / 1000) %>%
  tail()

#split, apply, combine
#median weight of every species
surveys %>%
  filter(!is.na(weight),
         year %in% 1980:1990,
         taxa == 'Rodent') %>%
  group_by(species_id) %>%
  summarize(median(weight))

surveys_complete = surveys %>%
  filter(!is.na(weight),
         sex != '',
         species_id != '',
         !is.na(hindfoot_length),
         year %in% 1980:1990,
         taxa == 'Rodent')

common_species = surveys_complete %>%
  group_by(species_id) %>%
  tally() %>%
  filter(n >= 50) %>%
  select(species_id)

common_surveys = surveys_complete %>%
  filter(species_id %in% common_species$species_id)

write.csv(common_surveys, file = '~/Desktop/surveys_complete.csv', row.names = FALSE)

#ggplot2
library(ggplot2)
ggplot(data = common_surveys,
       aes(x = weight, y = hindfoot_length, color = species_id)) +
  geom_point()

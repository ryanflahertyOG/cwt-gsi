#Author: Ryan Flaherty
#Description: CWT Recovery data preperation. Adds catch-area.

library('tidyverse')
library('lubridate')

oregon_recoveries <- read.csv("recoveries_OR.TXT", stringsAsFactors = F) %>%
  select(-format_version) %>%
  filter(tag_status == 1) %>%
  mutate(sampling_site = as.character(sampling_site)) %>%
  mutate(catch_sample_id = as.character(catch_sample_id)) %>%
  mutate(catch_area = 
           if_else(recovery_location_name %in% c('ASTORIA AREA 3',
                                                 'BROOKINGS AREA 3',
                                                 'CHARLESTON AREA 3',
                                                 'DEPOE BAY AREA 3',
                                                 'GARIBALDI AREA 3',
                                                 'NEWPORT AREA 3',
                                                 'PACIFIC CITY AREA 3'),
         'Tillamook',
         ifelse(recovery_location_name %in% c('ASTORIA AREA 4',
                                              'CHARLESTON AREA 4',
                                              'DEPOE BAY AREA 4',
                                              'GARIBALDI AREA 4',
                                              'FLORENCE AREA 4',
                                              'NEWPORT AREA 4',
                                              'SIUSLAW BAY AREA 4',
                                              'WINCHESTER BAY AREA 4'
         ),
         'Newport',
         ifelse(recovery_location_name %in% c('ASTORIA AREA 5',
                                              'BANDON AREA 5',
                                              'BROOKINGS AREA 5',
                                              'CHARLESTON AREA 5',
                                              'COOS BAY AREA 5',
                                              'DEPOE BAY AREA 5',
                                              'FLORENCE AREA 5',
                                              'GARIBALDI AREA 5',
                                              'NEWPORT AREA 5',
                                              'PORT ORFORD AREA 5',
                                              'SIUSLAW BAY AREA 5',
                                              'WINCHESTER B AREA 5',
                                              'WINCHESTER BAY AREA 5'
         ),
         'Coos',
         ifelse(recovery_location_name %in% c('BANDON AREA 6',
                                              'BROOKINGS AREA 6',
                                              'CHARLESTON AREA 6',
                                              'COOS BAY AREA 6',
                                              'DEPOE BAY AREA 6',
                                              'FLORENCE AREA 6',
                                              'GARIBALDI AREA 6',
                                              'GOLD BEACH AREA 6',
                                              'NEWPORT AREA 6',
                                              'PORT ORFORD AREA 6',
                                              'SIUSLAW BAY AREA 6',
                                              'WINCHESTER BAY AREA 6'
         ),
         'Brookings',
         'OTHER'
         )))))

california_recoveries <- read.csv("recoveries_CA.TXT", stringsAsFactors = F) %>%
  select(-format_version) %>%
  filter(tag_status == 1) %>%
  mutate(recovery_id = as.character(recovery_id)) %>%
  mutate(catch_area = 
           if_else(recovery_location_name %in% c('BIG LAG.- SPAN. FLAT',
                                                 'BIG LAG.-CENTERV.BEA',
                                                 'BIG LAG.-HUMBLT.JET.',
                                                 'CA/OR BDR.- HMBT.JET',
                                                 'CA/OR BOR-BIG LAGOON',
                                                 'CA/OR BOR-CENTER.BEA',
                                                 'CA/OR BOR-FA.KLAM.RC',
                                                 'CENTERV.BE-SPAN.FLAT',
                                                 'FA.KLA.RC-BIG LAGOON',
                                                 'FA.KLA.RC-CENTERV.BE',
                                                 'FA.KLAM.RC-SPAN.FLAT'),
                   'KLAMATH',
                   ifelse(recovery_location_name %in% c('C.VIZCAINO-FORT ROSS',
                                                        'C.VIZCAINO-NAVARR.HD',
                                                        'NAVARRO HD-FORT ROSS',
                                                        'SPAN.FLAT-C.VIZCAINO',
                                                        'SPAN.FLAT-NAVARRO HD',
                                                        'SPAN.FLAT-PT.ARENA'
                   ),
                   'BRAGG',
                   ifelse(recovery_location_name %in% c('FORT ROSS-PIGEON PT',
                                                        'FORT ROSS-POINT SUR',
                                                        'NAVARRO HD-PIGEON PT',
                                                        'NAVARRO HD-POINT SUR',
                                                        'PT.ARENA-PIGEON PT.',
                                                        'PT.ARENA-PT.SUR',
                                                        'PT.ARENA-PT.REYES',
                                                        'PT.REYES-PT.SUR',
                                                        'PT.REYES-PT.SAN PEDR',
                                                        'PT.ARENA-PT.SAN PEDR',
                                                        'PT.REYES-PIGEON PT.',
                                                        'PT.SN.PEDRO-PIGN.PT.',
                                                        'SPAN.FLAT-PT.REYES',
                                                        'SPAN.FLAT-PIGEON PT.',
                                                        'SPANISH FLAT-FT ROSS'
                   ),
                   'SANFR',
                   ifelse(recovery_location_name %in% c('PIGEON PT.-POINT SUR',
                                                        'PIGEON PT-CA/MEX.BOR',
                                                        'POINT SUR-CA/MEX.BOR'
                   ),
                   'MONTERY',
                   'OTHER'
                   )))))

#produce recovery location tables. Useful for determining later on which recovery 
#locations should be assigned to catch areas.

recovery_locations_OR <- tibble(unique(oregon_recoveries$recovery_location_name)) %>%
  arrange()
recovery_locations_CA <- tibble(unique(california_recoveries$recovery_location_name)) %>%
  arrange()

#change data depending on which state you are interested in.
recoveries_byArea <- california_recoveries %>%
  group_by(recovery_location_name, catch_area) %>%
  summarise(number = n())

#return distinct tag codes. Change data for Oregon. 
distinct_tags <- oregon_recoveries %>%
  select(tag_code) %>%
  distinct()
  
# join oregon and california recoveries
all_recoveries <- bind_rows(oregon_recoveries, california_recoveries) 


















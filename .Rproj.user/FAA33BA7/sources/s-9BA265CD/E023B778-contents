#Author: Ryan Flaherty ryan1.flaherty@gmail.com
#Description: This script preps CWT release data. Use the CWT recovery data script
#first to obtain distinct tag codes. THose codes are then used to query the RMIS database.
#Finally use this script to assign each release to equivalent GSI stock group.

#load required libraries
library('tidyverse')

#read in raw data from RMIS query.
OR_releases <- read.csv('releases_OR.txt', stringsAsFactors = F)
CA_releases <- read.csv('releases_CA.txt', stringsAsFactors = F)

#Function to add most likely GSI stock assignment to each CWT release group.
add_stock <- function(cwt_set){
  # add 'stock' to the cwt data where stock is representative of how the CWT release would be identified through GSI.
  # cwt_set are cwt release data.
  cwt_set %>%
  mutate(stock = ifelse(release_location_rmis_region %in% c('CECA',
                                                            'SJOA',
                                                            'SAFA')
                        & stock_location_name %in% c('FEATHER R HATCHERY',
                                                     'FEATHER RIVER',
                                                     'AMERICAN RIVER',
                                                     'MOKELUMNE RIVER',
                                                     'MERCED RIVER',
                                                     'SAN JOAQUIN RIVER',
                                                     'BATTLE CREEK BELOW CNFH',
                                                     'HOLLOW TREE CREEK',
                                                     'TEHAMA-COLUSA FF',
                                                     'COLEMAN NFH',
                                                     'DRY CREEK',
                                                     'SAC R AB COLLINSVILLE',
                                                     'SAC R AB FEATHER',
                                                     'YUBA RIVER')
                        & run %in% c(3,7),
                        'CVF', 
                        ifelse(release_location_rmis_region %in% c('CECA',
                                                                   'SJOA',
                                                                   'SAFA')
                               & stock_location_name %in% c('FEATHER R HATCHERY',
                                                            'FEATHER RIVER',
                                                            'AMERICAN RIVER',
                                                            'MOKELUMNE RIVER',
                                                            'MERCED RIVER',
                                                            'SAN JOAQUIN RIVER',
                                                            'BATTLE CREEK BELOW CNFH',
                                                            'HOLLOW TREE CREEK',
                                                            'COLEMAN NFH',
                                                            'DRY CREEK',
                                                            'BUTTE CREEK')
                               & run == 1,
                               'CVS', 
                               ifelse(release_location_rmis_region %in% c('KLTR')
                                             & stock_location_name %in% c('KLAMATH RIVER',
                                                                          'BLUE CREEK',
                                                                          'IRON GATE HATCHERY',
                                                                          'BOGUS CREEK',
                                                                          'CAMP CREEK',
                                                                          'SCOTT RIVER',
                                                                          'RED CAP CREEK',
                                                                          'SHASTA RIVER',
                                                                          'TRINITY RIVER',
                                                                          'HORSE LINTO CREEK',
                                                                          'MILL CR.-TRINITY.R.',
                                                                          'SUPPLY CREEK')
                                             & run %in% c(1,3,7),
                                             'KLM',
                                      ifelse(release_location_rmis_region %in% c('NOCA')
                                                          & stock_location_name %in% c('EEL RIVER',
                                                                                       'HOLLOW TREE CREEK',
                                                                                       'LITTLE RIVER',
                                                                                       'MAD RIVER',
                                                                                       'MAIN STEM MAD RIVER',
                                                                                       'MATTOLE RIVER',
                                                                                       'OUTLET CREEK',
                                                                                       'PRAIRIE CREEK',
                                                                                       'REDWOOD CR(TRIB EEL)',
                                                                                       'MAIN STEM EEL RIVER'
                                                                                       )
                                            & run %in% c(3,7),
                                            'CAC',
                                            ifelse(release_location_rmis_region %in% c('CECR')
                                                   & stock_location_name %in% c('DESCHUTES R',
                                                                                'WARM SPRINGS R',
                                                                                'ROUND BUTTE HATCHERY'),
                                                  
                                             'DSC',
                                             ifelse(release_location_rmis_region %in% c('CECR', 'UPCR')
                                                    & stock_location_name %in% c('COLUMBIA R UPRIVER S',
                                                                                 'TANNER CR (BNVILLE)',
                                                                                 'WASHINGTON BRIGHTS',
                                                                                 'UMATILLA R',
                                                                                 'LTL WHITE SALMON-NFH',
                                                                                 'PRIEST RAPIDS   (36)',
                                                                                 'COLUMBIA R BRIGHTS',
                                                                                 'COLUMBIA R UPRIVER S',
                                                                                 'SPRING CR    29.0159',
                                                                                 'WIND R       29.0023',
                                                                                 'ABERNATHY CR 25.0297',
                                                                                 'KLICKITAT R  30.0002',
                                                                                 'METHOW R     48.0002',
                                                                                 'LTL WHITE SALMON-NFH',
                                                                                 'METHOW & OKANOGAN',
                                                                                 'WELLS HATCHERY',
                                                                                 'WELLS DAM       (47)',
                                                                                 'WENATCHEE R  45.0030',
                                                                                 'HANFORD REACH STOCK',
                                                                                 'WASHINGTON BRIGHTS')
                                                    & run %in% c(2,3,7,8),
                                              'UCSF', 
                                              ifelse(release_location_rmis_region %in% c('LOCR')
                                                     & release_location_rmis_basin %in% c('SAND', 'WILL', 'YOCL', 'COWL', 'GREL', 'LEWI', 'SAWA')
                                                     & stock_location_name %in% c('TANNER CR (BNVILLE)',
                                                                                  'MID WILLAMETTE R',
                                                                                  'BIG CR HATCHERY',
                                                                                  'KLASKANINE R',
                                                                                  'COWLITZ R    26.0002',
                                                                                  'GREEN R      26.0323',
                                                                                  'ABERNATHY CR 25.0297',
                                                                                  'ELOCHOMAN R  25.0236',
                                                                                  'GRAYS R      25.0093',
                                                                                  'KALAMA R     27.0002',
                                                                                  'LEWIS R      27.0168',
                                                                                  'LEWIS R -NF  27.0168',
                                                                                  'TOUTLE R     26.0227',
                                                                                  'BONNEVILLE+WASHOUGAL',
                                                                                  'COWLITZ MIXED STOCKS',
                                                                                  'WASHOUGAL R  28.0159',
                                                                                  'ELOCHOMAN + KALAMA R',
                                                                                  'ELOCHOMAN + TOUTLE R',
                                                                                  'LOWER COLUMBIA',
                                                                                  'GRAYS R +ELOCHOMAN R')
                                                     & run %in% c(3,7,8),
                                                     'LCF',
                                                     ifelse(release_location_rmis_region %in% c('LOCR')
                                                            & release_location_rmis_basin %in% c('SAND', 'WILL', 'YOCL', 'COWL', 'GREL', 'LEWI', 'SAWA')
                                                            & stock_location_name %in% c('TANNER CR (BNVILLE)',
                                                                                         'MID WILLAMETTE R',
                                                                                         'BIG CR HATCHERY',
                                                                                         'KLASKANINE R',
                                                                                         'COWLITZ R    26.0002',
                                                                                         'GREEN R      26.0323',
                                                                                         'ABERNATHY CR 25.0297',
                                                                                         'ELOCHOMAN R  25.0236',
                                                                                         'GRAYS R      25.0093',
                                                                                         'KALAMA R     27.0002',
                                                                                         'LEWIS R      27.0168',
                                                                                         'LEWIS R -NF  27.0168',
                                                                                         'BONNEVILLE+WASHOUGAL',
                                                                                         'COWLITZ MIXED STOCKS',
                                                                                         'WASHOUGAL R  28.0159',
                                                                                         'ELOCHOMAN + KALAMA R',
                                                                                         'ELOCHOMAN + TOUTLE R',
                                                                                         'LOWER COLUMBIA',
                                                                                         'GRAYS R +ELOCHOMAN R',
                                                                                         'SANDY HATCHERY (SANDY R)',
                                                                                         'CLACKAMAS R EARLY')
                                                            & run %in% c(1),
                                                            'LCS',
                                                     ifelse(release_location_rmis_region %in% c('NOOR')
                                                            & stock_location_name %in% c('FALL CR (ALSEA R)',
                                                                                         'ALSEA R AND TRIBS',
                                                                                         'TRASK R (TRASK HT)',
                                                                                         'YAQUINA R - PUBLIC',
                                                                                         'NESTUCCA R (CEDAR CR',
                                                                                         'TRASK R (TRASK HT)',
                                                                                         'SALMON R',
                                                                                         'SALMON R WILD')
                                                            & run %in% c(1,3,4),
                                                            'NOC',
                                                            ifelse(release_location_rmis_region %in% c('SOOR', 'NOOR')
                                                                   & release_location_rmis_basin %in% c('SIXE', 'COQU', 'COOS', 'SIUS', 'UMPQ')
                                                                   & stock_location_name %in% c('SIUSLAW R - PRIVATE',
                                                                                                'SIUSLAW R',
                                                                                                'COOS R',
                                                                                                'COQUILLE R',
                                                                                                'ELK R (ELK R HT)',
                                                                                                'UMPQUA R (ROCK CR HT)',
                                                                                                'GARDINER CR (UMPQUA)',
                                                                                                'COW CR (S UMPQUA R)',
                                                                                                'SILETZ R (SILETZ HT)',
                                                                                                'TRASK R (TRASK HT)'
                                                                                                )
                                                                   & run %in% c(1,3),
                                                                   'MOC',
                                                                   ifelse(release_location_rmis_region %in% c('SOOR')
                                                                          & release_location_rmis_basin %in% c('ROGU')
                                                                          & run %in% c(1,3),
                                                                          'ROG',
                                                                          ifelse(release_location_rmis_region %in% c('SOOR', 'NOCA')
                                                                                 & release_location_rmis_basin %in% c('CHET', 'SMIT')
                                                                                 & stock_location_name %in% c('CHETCO R',
                                                                                                              'HUNTER CR AND TRIBS',
                                                                                                              'LOBSTER CR PRIVATE',
                                                                                                              'PISTOL R AND TRIBS',
                                                                                                              'WINCHUCK R AND TRIBS',
                                                                                                              'ROWDY CREEK, SMITH R',
                                                                                                              'SMITH RIVER'
                                                                                 )
                                                                                 & run %in% c(1,3),
                                                                                 'NCSOC',
                                                                                        ifelse(release_location_rmis_region %in% c('LOCR')
                                                                                               & release_location_rmis_basin %in% c('WILL'),
                                                                                               'WILL', 
                                                                                               ifelse(release_location_rmis_region %in% c('SNAK'),
                                                                                                      'SNK', 'OTHER'
                                                                                        
                                                                                        



                                            )))))))))))))))
}

#Use add_stock function to add stock assignments.
OR_releases <- add_stock(OR_releases) 
CA_releases <- add_stock(CA_releases)

#bind oregon and california releases.
ALL_releases <- bind_rows(OR_releases, CA_releases) %>%
  distinct()

#join release data with recovery data using tag_code as the primary key.
CWT_RR <- left_join(all_recoveries, ALL_releases, by = c("tag_code" = "tag_code_or_release_id"))


#EDA stuff
OR_stocks <- OR_releases %>%
  filter(stock == 'OTHER') %>%
  group_by(release_location_state, 
           release_location_rmis_region, 
           release_location_rmis_basin,
           release_location_name,
           hatchery_location_name,
           stock_location_name,
           stock,
           run) %>%
  summarise(number = n())

CA_stocks <- CA_releases %>%
  filter(stock == 'OTHER') %>%
  group_by(release_location_state, 
           release_location_rmis_region, 
           release_location_rmis_basin,
           release_location_name,
           hatchery_location_name,
           stock_location_name,
           stock,
           run) %>%
  summarise(number = n())

CWT_stocks <- CWT_RR %>%
  filter(stock == 'OTHER') %>%
  group_by(release_location_state, 
           release_location_rmis_region, 
           release_location_rmis_basin,
           release_location_name,
           hatchery_location_name,
           stock_location_name,
           stock,
           run) %>%
  summarise(number = n())
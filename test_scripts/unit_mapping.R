# library(measurements)
# 
# # The LHS will be removed from the options and then be replaced with the RHS
# # if typed into a variable table.
# 
# unit_mapping <- c(
#   nsec = "ns",
#   usec = "us",
#   msec = "ms",
#   sec = "s",
#   mins = "min",
#   hour = "hr",
#   hours = "hr",
#   days = "day",
#   week = "wk",
#   mon = "month",
#   year = "yr",
#   dec = "decade",
#   cen = "century",
#   mil = "millenium",
#   
#   # Length
#   meter = "m",
#   metre = "m",
#   inch = "in",
#   foot = "ft",
#   feet = "ft",
#   yard = "yd",
#   mile = "mi",
#   AU = "au",
#   light_yr = "light_year",
#   
#   # Mass
#   pound = "lbs",
#   pounds = "lbs",
#   
#   # Volume
#   ul = "uL",
#   ml = "mL",
#   dl = "dL",
#   l = "L",
#   
#   # Energy
#   Cal = "cal",
#   
#   # Flow
#   LPM = "l_per_min",
#   LPH = "l_per_hour",
#   GPM = "gal_per_min",
#   GPH = "gal_per_hour"
#   
#   # Count (none)
# )
# 
# # Energy
# print(measurements::conv_unit_options$count)
# print(names(unit_mapping))
# 
# cleaned_duration <- setdiff(measurements::conv_unit_options$energy, 
#                             names(unit_mapping))
# print(cleaned_duration)
# 
# # Volume
# print(measurements::conv_unit_options$volume)
# print(names(unit_mapping))
# 
# cleaned_duration <- setdiff(measurements::conv_unit_options$volume, 
#                             names(unit_mapping))
# print(cleaned_duration)
# 
# # Mass
# print(measurements::conv_unit_options$mass)
# print(names(unit_mapping))
# 
# cleaned_duration <- setdiff(measurements::conv_unit_options$mass, 
#                             names(unit_mapping))
# print(cleaned_duration)
# 
# # Length
# print(measurements::conv_unit_options$length)
# print(names(unit_mapping))
# 
# cleaned_duration <- setdiff(measurements::conv_unit_options$length, 
#                             names(unit_mapping))
# print(cleaned_duration)
# 
# # Duration
# # print(measurements::conv_unit_options$duration)
# print(names(unit_mapping))
# 
# cleaned_duration <- setdiff(measurements::conv_unit_options$duration, 
#                             names(unit_mapping))
# print(cleaned_duration)

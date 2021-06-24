#######################
#   TIMUR MAGZHANOV   #
#     25/03/2021      #
#######################

# This code is a parser (scraper) that takes data of air tickets' prices
# Source: https://www.aviakassa.com/
# The database is written in mongodb that should be installed:
# IN CMD: pip install pymongo
# Let's go!

#######################

# LIBRARIES

import logging
import requests
import json
import time
import pymongo
from pymongo import MongoClient
# see https://docs.mongodb.com/manual/installation/ if something's wrong with database

#######################

# MAIN

# Set up a database in Mongo, this line is standart:
client = MongoClient(host='localhost', port=27017)

# Create a database Data_Avia_25_03 in Mongo:
db = client['Data_Avia_25_03']

# Set time format:
date = time.strftime("%d/%m/%Y")

# Set flights departure (from) and destination (to)
# Format: 'FROMTO'
# City codes can be found on site (use F12 -> All -> any file)
# Now Moscow - Saint Petersburg and Saint Petersburg - Moscow

locations = ['MOWLED', 'LEDMOW']

# Set classes to get (default: WE - econom, WB - business)
clases = ['WE', 'WB']

# Set dates to track
# Format: yearmonthday
dates = ['20210401', '20210402', '20210403']

# Take this line manually from the site
# Use F12 -> XHR -> search-result?... -> Copy -> Copy link adress
# You need what is between metadata= and &uuid=
# Paste it here:
metadata = 's%2FEDxyXHITeYEB1oFzrRQYZtSVn0TFUXczSb%2Fz7ScCDQmq4MZEjcOktfRC2%2B34k9dHmO2b8fpiw3uWIx5r5gOOYK26GqdQFtnGTOXA8ZASsKKz0unVd%2Fvzw1YtdhuFA8PwJTNEZX%2FUcmQiSshfWADfL%2Bnl0TJpWYliJiQwgVFPpWikqzymyWX5Hl3gt8iiYC83BcolgAAvk%2BhD4IzQfOwS0uDULMJiwL%2FECKyYiz19AI6pEAc9hTOzoDvJEQzwOGDmAHlc%2BKR5iuA%2B6LZiCxtZdtRNgllk9fcInVep0F50f0mgteqQmlq7D%2FlhoRPykYfXW8Yp3QKUZQXtyDxe9QI%2F8bTn6OIoiyYLBBt1tex0esoeGRmHo1c6r3zDYg6eDiGYqOb0a0RZCopK6zIvICflvCX4kLT4UeQd5IDmaAeJESeyWq0j94vxAQemhKQZYsyzs%2B9uZAJ%2FjHotIYG%2Bl8HPYrafyNLxh6%2B%2FosxcHRYTlXRTtGTmWOGz6kJYD65KgWPQvx1OazvCJ8fMFif8Xbq9T66U91hAVpbR1tc0cXc3pxKhvVph9d%2B4NOyHUrov%2BJSYvZszh8OXQjnRtjmfZm6%2FXivoMNEdups0vgxnKpbXrAHLj2UDw7zSJl6c8AHB%2BJNWmVItmFs7qlUM62MyCulVw4o6w5r0NbUlmlASa1HkQ%3D'
print(date)

# Define parser function:
def parser(location):
    for dep_date in dates:
        # For each location and date there will be created separate database:
        collection = db['%s-%s' % (location, dep_date)]
        flight_id = 0
        for clas in clases:
            # Load JSON data from the site (nothing to change):
            main_json_data = json.loads(requests.get('https://api4.aviakassa.com/v4/avia/search-result?sro=AKV40000O%s1000000%s%s&include_subtrips=1&lang=ru&metadata=%s&uuid=73e4fb35-a8ce-4bb8-adc7-9c0da9ce6ee9' % (clas, location, dep_date, metadata)).text)
            # Take flights' data:
            json_data = main_json_data['data']['flights']
            
            for item in json_data:
                if len(item['segments'])==1:
                    flight_id = flight_id + 1
                    # Get company's name:
                    name = item['provider']['supplier']['title']

                    # Get flight's number:
                    flight_num = "%s-%s"%(item['provider']['supplier']['code'],item['segments'][0]['flight_number'])

                    # Get departure time:
                    dep_time = item['segments'][0]['dep']['time']

                    # Get flight duration:
                    duration = item['duration']

                    # Get price:
                    price = item['price']['RUB']['amount']
            
                    # Update database (write new observation):
                    collection.update_one(
                        {"ID": flight_id},
                        {"$set":{
                        "Номер рейса": flight_num,
                        "Класс": clas,
                        "Время вылета": dep_time,
                        "Продолжительность полета": duration,
                        "%s" % (date): price
                        }
                        }, upsert = True)
    print('Success!')
for l in locations:
    parser(l)







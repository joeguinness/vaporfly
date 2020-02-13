
import requests
from bs4 import BeautifulSoup
import csv
import pandas as pd
import sys

# run as
# $ python scrape01.py gender_int max_place output_filename

# read geneder_int from command line
# (gender_int = 0 for both )
# (gender_int = 1 for men  )
# (gender_int = 2 for women)
gender_int = int( sys.argv[1] )

# read maximum place in any race
max_place = int( sys.argv[2] )

# output filename
output_filename = sys.argv[3]

# generic marathon guide browse url and makelinks url
browse_url = "http://www.marathonguide.com/results/browse.cfm"
makelinks_url = "http://www.marathonguide.com/results/makelinks.cfm"

# data and header objects to post to website
post_data = {
    "RaceRange" : "",
    "MIDD" : "",
    "SubmitButton" : "View"
}
headers = {
    "Referer" : "",
    "User-Agent" : ("Mozilla/5.0 (Windows NT 6.1; WOW64) "
                    "AppleWebKit/537.36 (KHTML, like Gecko) "
                    "Chrome/34.0.1847.116 Safari/537.36")
}

# all possible fields
all_fields = [
     'Last Name, First Name(Sex/Age)', 'Time', 
     'OverAllPlace', 'Sex Place', 'Sex Place/Div Place', 'DIV', 'Net Time', 
     'City, State, Country', 'State, Country', 'Country', 'City', 'City, Country',
     'AG Time*', 'BQ*']
race_results = pd.DataFrame(columns = all_fields)

# read in race codes
reader = csv.DictReader(open("race_codes.csv", "rb"))
race_codes = [ line for line in reader ] 

    
# loop over all races
for r in range(0, len(race_codes)):
    
    # midd and marathon url
    midd = race_codes[r]["midd"]
    mar_name = race_codes[r]["race"]
    mar_year = race_codes[r]["year"]
    mar_date = race_codes[r]["date"]
    mar_url = browse_url + "?MIDD=" + midd
    print(mar_name + " " + mar_year)
    
    # update the midd and referer
    post_data["MIDD"] = midd
    headers["Referer"] = mar_url

    # start up a session
    sesh = requests.Session()

    # request browse page and soup it
    browse_page = sesh.get( mar_url )
    browse_soup = BeautifulSoup( browse_page.content, "html5lib" )

    # extract the ranges for this race
    select_gender = browse_soup.find_all("select")[gender_int]
    options_gender = select_gender.find_all("option")
    all_race_ranges = [ options_gender[i]["value"] for i in range(1,len(options_gender)) ]
    
    # loop over these and only keep places less than max_place
    race_ranges = []
    for j in range(0,len(all_race_ranges)):
        rr_list = all_race_ranges[j].split(",")
        if int(rr_list[1]) < max_place:
            race_ranges.append( all_race_ranges[j] )

    # loop over the race ranges
    for j in range(0,len(race_ranges)):

        race_range = race_ranges[j]

        # update post data
        post_data["RaceRange"] = race_range

        # make post request and soup it
        page = sesh.post(makelinks_url, data=post_data, headers=headers)
        soup = BeautifulSoup(page.content, "html5lib")

        # results sit in the 9th table
        table = soup.find_all("table")[8]

        # all but first three tr's are results
        results_rows = table.find_all("tr")
        for row in range(0,len(results_rows)):
            names = results_rows[row].find_all("th")
            if len(names) > 1:
                field_names = [ f.get_text().encode("utf-8").strip("\n") for f in names ]
                first_row = row+1

        # loop over these rows
        for row in range(first_row,len(results_rows)):

            # extract the data fields for this row
            fields = results_rows[row].find_all("td")
            
            # insert NAs into all fields
            this_result = {}
            for field in all_fields:
                this_result[field] = "NA"
                
            # extract the data and add some additional race info
            for f in range(0,len(fields)):
                this_result[field_names[f]] = fields[f].get_text().encode("utf-8")

            this_result["marathon"] = mar_name 
            this_result["year"] = mar_year 
            this_result["date"] = mar_date
            this_result["midd"] = midd
            race_results = race_results.append( this_result, ignore_index = True )


# rename the columns of race_results
col_names = {
    'Last Name, First Name(Sex/Age)' : "name_age",
    'Time' : "time",
    'OverAllPlace' : "overall_place",
    'Sex Place/Div Place' : "sex_div_place",
    'Sex Place' : "sex_place",
    'DIV' : "div",
    'Net Time' : "net_time",
    'City, State, Country' : "city_state_country",
    'State, Country' : "state_country",
    'Country' : "country",
    'City' : "city",
    'City, Country' : "city_country",
    'AG Time*' : "ag_time",
    'BQ*' : "bq"
}
race_results = race_results.rename( columns = col_names )

# write to csv without row names
race_results.to_csv(output_filename, index = False)


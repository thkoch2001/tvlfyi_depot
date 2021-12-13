# The file '2010.census.txt' contains summary statistics from the 2010 United
# States census including household income. The data is in an unspecified
# format.

# Find the average of the column called:

#     'MEDIAN HOUSEHOLD INCOME'

# Ideally the solution should be a command line script, of the form:

#     $ ./solution [options] [file...]

# The solution may be written in any language, Python is preferred but not
# required.

# Google, stack overflow, etc. usage is allowed.

import requests
import csv

url = "https://assets.tryramp.com/interview/census/2010.census.txt"
column = 'MEDIAN HOUSEHOLD INCOME'
columns = [
    'CENSUS YEAR',
    'TRACT',
    'BLOCK GROUP',
    'FIPS ID',
    'TOTAL POPULATION',
    'POPULATION WHITE',
    'POPULATION BLACK',
    'POPULATION ASIAN',
    'POPULATION OTHER',
    'POPULATION AMERICAN INDIAN',
    'POPULATION PACIFIC ISLANDER',
    'POPULATION ONE RACE',
    'POPULATION MULTI RACE',
    'POPULATION 25 OLDER',
    'MEDIAN AGE',
    'MEDIAN HOUSEHOLD INCOME',
    'HIGH SCHOOL MALE',
    'HIGH SCHOOL MORE MALE',
    'COLLEGE 1 YR LESS MALE',
    'COLLEGE 1 YR MORE MALE',
    'ASSOCIATES DEGREE MALE',
    'BACHELORS DEGREE MALE',
    'MASTERS DEGREE MALE',
    'PROFESSIONAL DEGREE MALE',
    'DOCTORAL DEGREE MALE',
    'HIGH SCHOOL FEMALE',
    'HIGH SCHOOL MORE FEMALE',
    'COLLEGE 1 YR LESS FEMALE',
    'COLLEGE 1 YR MORE FEMALE',
    'ASSOCIATES DEGREE FEMALE',
    'BACHELORS DEGREE FEMALE',
    'MASTERS DEGREE FEMALE',
    'PROFESSIONAL DEGREE FEMALE',
    'DOCTORAL DEGREE FEMALE',
    'PERCENT 25 YR OVER HIGH SCHOOL MORE',
    'HOUSING UNITS',
    'OCCUPIED HOUSING UNITS',
    'OWNER OCCUPIED HOUSING',
    'RENTER OCCUPIED HOUSING',
    'PERCENT OWNER OCCUPIED',
    'PERCENT RENTER OCCUPIED',
    'MEDIAN HOUSE VALUE OWNER OCCUPIED',
    'MEDIAN YEAR BUILT',
    'VACANCY RATES',
]


def average(xs):
    return sum(xs) / len(xs)


def parse_body(body):
    return list(csv.DictReader(body.split('\n')[1:], delimiter='|', fieldnames=columns))


def main():
    res = requests.get(url)
    if res.status_code not in {200}:
        raise Exception("Unexpected status code: {}".format(res.status_code))
    return average([int(d.get(column))
                    for d in parse_body(res.text)
                    if int(d.get(column)) >= 0])

print(main())

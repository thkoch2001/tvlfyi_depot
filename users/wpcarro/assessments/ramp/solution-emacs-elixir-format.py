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

url = "https://assets.tryramp.com/interview/census/2010.census.txt"

def main():
    res = requests.get(url)
    if res.status not in {200}:
        raise Exception("Unexpected status code: {}".format(res.status_code))
    # download the content
    # parse row
    # select 'MEDIAN HOUSEHOLD INCOME' column
    pass

# This turns the CDC mortality data[0] into a format useful for my
# excess mortality spreadsheet. The US format is by far the worst one
# I have dealt with, as expected.
#
# This requires miller for transforming the CSV appropriately.
#
# Params:
#  state: abbreviation of the state to extract ('US' for whole country)
#  period: time period (either "2020" for current data, or anything else
#          for historical averages)
#
# Call as:
#  mlr --icsv --ojson cat weekly.csv | \
#    jq -rsf us_mortality.jq --arg state US --arg period 2020
#
# [0]: https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm

def filter_period(period):
  if period == "2020"
  then . | map(select(.["Time Period"] == 2020))
  else . | map(select(.["Time Period"] == "2015-2019"))
  end;

def collate_weeks(period):
  (. | map(.["Number of Deaths"]) | add) as $count
  | {
    count: (if period == "2020" then $count else $count / 5 end),
    week: .[0].Week,
  };

. | map(select(.Type == "Predicted (weighted)"))
  | map(select(.["State Abbreviation"] == $state))
  | filter_period($period)
  | group_by(.Week)
  | map(collate_weeks($period))
  | .[] | "week \(.week): \(.count)"

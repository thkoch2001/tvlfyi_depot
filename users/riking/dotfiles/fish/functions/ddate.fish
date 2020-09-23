function ddate --description 'current date in Discourse format'
    TZ=UTC date '+[date=%Y-%m-%d time=%H:%M:%S timezone=\"%Z\"]'
end

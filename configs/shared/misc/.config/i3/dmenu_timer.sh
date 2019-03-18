#!/usr/bin/env bash

# Select common timer intervals with dmenu and play an alarm sound when
# finished. Useful if you bind a KBD in a window manager such as i3. Pass the
# path to the alarm mp3 as the only argument.
#
# Usage: ./dmenu_timer.sh path/to/alarm.mp3

times=$(cat <<EOF
1 minute
2 minutes
3 minutes
4 minutes
5 minutes
10 minutes
15 minutes
20 minutes
30 minutes
45 minutes
1 hour
2 hours
EOF
)
selection=$(echo "$times" | dmenu)

case $selection in
  '1 minute')
    notify-send 'Timer' 'Set for 1 minute' && \
      sleep 1m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '2 minutes')
    notify-send 'Timer' 'Set for 2 minute' && \
      sleep 2m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '3 minutes')
    notify-send 'Timer' 'Set for 3 minutes' && \
      sleep 3m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '4 minutes')
    notify-send 'Timer' 'Set for 4 minutes' && \
      sleep 4m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '5 minutes')
    notify-send 'Timer' 'Set for 5 minutes' && \
      sleep 5m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '10 minutes')
    notify-send 'Timer' 'Set for 10 minutes' && \
      sleep 10m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '15 minutes')
    notify-send 'Timer' 'Set for 15 minutes' && \
      sleep 15m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '20 minutes')
    notify-send 'Timer' 'Set for 20 minutes' && \
      sleep 20m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '30 minutes')
    notify-send 'Timer' 'Set for 30 minutes' && \
      sleep 30m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '45 minutes')
    notify-send 'Timer' 'Set for 45 minutes' && \
      sleep 45m && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '1 hour')
    notify-send 'Timer' 'Set for 1 hour' && \
      sleep 1h && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  '2 hours')
    notify-send 'Timer' 'Set for 2 hours' && \
      sleep 2h && \
      notify-send 'Timer' 'Finished.' && \
      mpg123 $1 && \
      exit 0
    ;;
  *)
    notify-send 'Timer' 'No supported time selected. Exiting...' && exit 1
esac

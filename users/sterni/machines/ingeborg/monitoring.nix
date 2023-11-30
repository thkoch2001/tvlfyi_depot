{ pkgs, lib, config, ... }:

let
  ircChannel = "#sterni.lv";
  irccatPort =
    builtins.replaceStrings [ ":" ] [ "" ]
      config.services.depot.irccat.config.tcp.listen;

  mkIrcMessager =
    { name
    , msgExpr
    }:
    pkgs.writeShellScript name ''
      set -euo pipefail
      printf '%s %s\n' ${lib.escapeShellArg ircChannel} ${msgExpr} | \
        ${lib.getBin pkgs.netcat-openbsd}/bin/nc -N localhost ${irccatPort}
    '';

  netdataPort = 19999;
in

{
  imports = [
    ./irccat.nix
  ];

  config = {
    services.depot.irccat.config.irc.channels = [
      ircChannel
    ];

    # Since we have irccat we can wire up mdadm --monitor
    boot.swraid.mdadmConf = ''
      PROGRAM ${
        mkIrcMessager {
          name = "mdmonitor-to-irc";
          # prog EVENT MD_DEVICE COMPONENT_DEVICE
          msgExpr = ''"mdmonitor: $1($2''${3:+, $3})"'';
        }
      }
    '';

    # TODO(sterni): irc notifications (?)
    services = {
      smartd = {
        enable = true;
        autodetect = true;
        # Short self test every day 03:00
        # Long self test every tuesday 05:00
        defaults.autodetected = "-a -o on -s (S/../.././03|L/../../2/05)";
        extraOptions = [
          "-A"
          "/var/log/smartd/"
        ];
      };

      netdata = {
        enable = true;
        config = {
          logs = {
            access = "syslog";
            error = "syslog";
            debug = "syslog";
            health = "syslog";
            collector = "syslog";
          };
          web = {
            "default port" = toString netdataPort;
            "bind to" = "localhost:${toString netdataPort}";
          };
          health = {
            "script to execute on alarm" = pkgs.writeShellScript "simple-alarm-notify" ''
              set -euo pipefail

              # This humongous list is copied over from netdata's alarm-notify.sh
              roles="''${1}"               # the roles that should be notified for this event
              args_host="''${2}"           # the host generated this event
              unique_id="''${3}"           # the unique id of this event
              alarm_id="''${4}"            # the unique id of the alarm that generated this event
              event_id="''${5}"            # the incremental id of the event, for this alarm id
              when="''${6}"                # the timestamp this event occurred
              name="''${7}"                # the name of the alarm, as given in netdata health.d entries
              chart="''${8}"               # the name of the chart (type.id)
              status="''${9}"              # the current status : REMOVED, UNINITIALIZED, UNDEFINED, CLEAR, WARNING, CRITICAL
              old_status="''${10}"         # the previous status: REMOVED, UNINITIALIZED, UNDEFINED, CLEAR, WARNING, CRITICAL
              value="''${11}"              # the current value of the alarm
              old_value="''${12}"          # the previous value of the alarm
              src="''${13}"                # the line number and file the alarm has been configured
              duration="''${14}"           # the duration in seconds of the previous alarm state
              non_clear_duration="''${15}" # the total duration in seconds this is/was non-clear
              units="''${16}"              # the units of the value
              info="''${17}"               # a short description of the alarm
              value_string="''${18}"       # friendly value (with units)
              # shellcheck disable=SC2034
              # variable is unused, but https://github.com/netdata/netdata/pull/5164#discussion_r255572947
              old_value_string="''${19}"   # friendly old value (with units), previously named "old_value_string"
              calc_expression="''${20}"    # contains the expression that was evaluated to trigger the alarm
              calc_param_values="''${21}"  # the values of the parameters in the expression, at the time of the evaluation
              total_warnings="''${22}"     # Total number of alarms in WARNING state
              total_critical="''${23}"     # Total number of alarms in CRITICAL state
              total_warn_alarms="''${24}"  # List of alarms in warning state
              total_crit_alarms="''${25}"  # List of alarms in critical state
              classification="''${26}"     # The class field from .conf files
              edit_command_line="''${27}"  # The command to edit the alarm, with the line number
              child_machine_guid="''${28}" # the machine_guid of the child
              transition_id="''${29}"      # the transition_id of the alert
              summary="''${30}"            # the summary text field of the alert

              # Verify that they haven't extended the arg list
              ARG_COUNT_EXPECTED=30

              if [[ "$#" != "$ARG_COUNT_EXPECTED" ]]; then
                echo "$0: WARNING: unexpected number of arguments: $#. Did netdata add more?" >&2
              fi

              MSG="netdata: $status ''${name//_/ } ($chart): ''${summary//_/ } = $value_string"

              echo "$0: INFO: sending message: $MSG" >&2
              ${
                mkIrcMessager {
                  name = "trivial-send-to-irc";
                  msgExpr = "\"$1\"";
                }
              } "$MSG"
            '';
          };
        };
      };
    };
  };
}

{ pkgs }:

let

  bin' = pkg: pkgs.${pkg} + "/bin/" + pkg;

  bash' = "bash"; # or compatible

  rg = pkgs.ripgrep + "/bin/rg";
  notify-desktop = bin' "notify-desktop";

  x-notify-on-low-battery = pkgs.writeScriptBin "x-notify-on-low-battery" ''
    #!${bin' bash'}

    test -f "$1" || exit

    devices=$(cat "$1")

    for bat_path in "$devices"
    do
      ${bin' "refresh-power-stats"} "$bat_path"

      bat_summary="$(${bin' "power-stats"} "$bat_path")"

      state="$( echo "$bat_summary" \
              | ${rg} -i -o "(?:state\s*:\s*)discharging" \
              | xargs | ${rg} -i -o discharging \
              | head -c -1 \
              | tr '[:upper:]' '[:lower:]')"

      if [ "$state" == "discharging" ]
      then
        percentage="$( echo "$bat_summary" \
                     | ${rg} -i -o "percentage\s*:\s*[0-9]+%" \
                     | xargs | ${rg} --trim -o "[0-9]+%" \
                     | head -c -2)"

        if [[ "$percentage" -lt 18 && $percentage -gt 10 ]]
        then
          ${notify-desktop} -u normal "Battery Low"
        fi

        if [[ "$percentage" -lt 11 ]]
        then
          ${notify-desktop} -u critical "Battery Critically Low"
        fi
      fi
    done
  '';

in x-notify-on-low-battery

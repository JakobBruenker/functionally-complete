#/bin/bash
set -e
APP_NAME=functionally-complete
APP_PATH=$(cabal list-bin $APP_NAME)
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

sudo -v

keep_sudo_alive() {
  while true; do
    sudo -v
    sleep 60
  done
}

keep_sudo_alive &
KEEP_SUDO_PID=$!

cabal clean
cabal build

sudo setcap 'cap_net_bind_service=+ep' $APP_PATH
sudo bash -c "sed 's|{{APP_NAME}}|$APP_NAME|g; s|{{APP_PATH}}|$APP_PATH|g; s|{{WORKING_DIR}}|$SCRIPT_DIR|g' service > /etc/systemd/system/$APP_NAME.service"
sudo systemctl enable $APP_NAME.service
sudo systemctl restart $APP_NAME.service

kill $KEEP_SUDO_PID

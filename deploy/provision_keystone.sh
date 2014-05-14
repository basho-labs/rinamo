#!/usr/bin/env bash

set -e

export DEBIAN_FRONTEND=noninteractive
export HOME=/home/vagrant

# libpam
if ! dpkg --get-selections | grep -q "libpam0g-dev"; then
  apt-get update -qq && apt-get install -y libpam0g-dev
  apt-get autoremove -y
fi

if [ ! -f "${HOME}/.ssh/config" ]; then
  cat <<EOF >> .ssh/config
Host github.com
  StrictHostKeyChecking no
EOF
fi

# KeyStone
if ! dpkg --get-selections | grep -q "keystone"; then
  apt-get update -qq && apt-get install -y keystone jq
  apt-get autoremove -y
fi

# Post Keystone Config
sed -i.bak 's/#admin_token=ADMIN/admin_token=rinamo_admin_token/' /etc/keystone/keystone.conf
service keystone restart

echo
echo "--"
echo
echo "Oh, cool. Keystone is available to develop against: 10.0.0.3:35357"
echo

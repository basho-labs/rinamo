#!/usr/bin/env bash

set -e

# libpam
if ! dpkg --get-selections | grep -q "libpam0g-dev"; then
  apt-get update -qq && apt-get install -y libpam0g-dev
fi

if [ ! -f "${HOME}/.ssh/config" ]; then
  cat <<EOF >> .ssh/config
Host github.com
  StrictHostKeyChecking no
EOF
fi

# Riak
if [ ! -d riak ]; then
  su --preserve-environment -c "git clone -b rs-rinamo-vagrant git@github.com:basho/riak.git" vagrant
  pushd riak
  su -c "make locked-all rel" vagrant
  pushd rel/riak
  su -c "sed -i.bak 's/anti_entropy = active/anti_entropy = passive/' etc/riak.conf" vagrant
  su -c "sed -i.bak 's/log.console.level = info/log.console.level = debug/' etc/riak.conf" vagrant
  su -c "sed -i.bak 's/listener.http.internal = 127.0.0.1:8098/listener.http.internal = 0.0.0.0:8098/' etc/riak.conf" vagrant
  su -c "echo 'rinamo.network.bind = 0.0.0.0:8000' >> etc/riak.conf" vagrant

  popd
  popd
fi

# Start Riak
pushd riak/rel/riak
ulimit -n 32768
su -c "./bin/riak start" vagrant
su -c "./bin/riak ping" vagrant

echo
echo "--"
echo
echo "Oh, cool. Rinamo is available to develop against: 10.0.0.2:8000"
echo

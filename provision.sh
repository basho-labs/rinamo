#!/usr/bin/env bash

# Riak
if [ ! -d riak ]; then
  pushd riak
  su -c 'make locked-all rel' vagrant
  pushd rel/riak
  echo 'anti_entropy = passive' >> etc/riak.conf
  echo 'log.console.level = debug' >> etc/riak.conf
  echo 'rinamo.network.bind = 0.0.0.0:8000' >> etc/riak.conf
  echo 'listener.http.internal = 0.0.0.0:8098' >> etc/riak.conf

  popd
  popd
fi
chown -R vagrant:vagrant riak

# Start Riak
pushd riak/rel/riak
ulimit -n 32768
su -c './bin/riak start' vagrant
su -c './bin/riak ping' vagrant

#!/usr/bin/env bash

# i took these from https://github.com/sstephenson/ruby-build/blob/master/bin/ruby-build
compute_md5() {
  if type md5 &>/dev/null; then
    md5 -q
  elif type openssl &>/dev/null; then
    local output="$(openssl md5)"
    echo "${output##* }"
  elif type md5sum &>/dev/null; then
    local output="$(md5sum -b)"
    echo "${output% *}"
  else
    return 1
  fi
}

verify_checksum() {
  # If there's no MD5 support, return success
  [ -n "$HAS_MD5_SUPPORT" ] || return 0

  # If the specified filename doesn't exist, return success
  local filename="$1"
  [ -e "$filename" ] || return 0

  # If there's no expected checksum, return success
  local expected_checksum=`echo "$2" | tr [A-Z] [a-z]`
  [ -n "$expected_checksum" ] || return 0

  # If the computed checksum is empty, return failure
  local computed_checksum=`echo "$(compute_md5 < "$filename")" | tr [A-Z] [a-z]`
  [ -n "$computed_checksum" ] || return 1

  if [ "$expected_checksum" != "$computed_checksum" ]; then
    { echo
      echo "checksum mismatch: ${filename} (file is corrupt)"
      echo "expected $expected_checksum, got $computed_checksum"
      echo
    } >&4
    return 1
  fi
}

download_unless_exist() {
  local filename=`basename $1`
  local url="$1"
  if [[ -e "$filename" ]]; then
      return 0
  fi
  `wget "$url"`
}

# Base System
sudo apt-get update
sudo apt-get install -y build-essential libncurses5-dev openssl libssl-dev git curl libpam0g-dev expect openjdk-7-jdk autoconf vim libjansi-java libjansi-native-java libhawtjni-runtime-java

# Scala
download_unless_exist "http://www.scala-lang.org/files/archive/scala-2.10.3.deb"
verify_checksum "scala-2.10.3.deb" 59f6ff95433df2105ed27dd77bb29a9e || exit
if [ ! -e /usr/bin/scala ]; then
  dpkg -i scala-2.10.3.deb
fi

# Maven
download_unless_exist "http://mirror.cc.columbia.edu/pub/software/apache/maven/maven-3/3.2.1/binaries/apache-maven-3.2.1-bin.tar.gz"
verify_checksum "apache-maven-3.2.1-bin.tar.gz" aaef971206104e04e21a3b580d9634fe || exit
if [ ! -e bin/apache-maven-3.2.1/bin/mvn ]; then
  mkdir -p bin
  tar -xzf apache-maven-3.2.1-bin.tar.gz -C bin
  echo 'export PATH=$PATH:$HOME/bin/apache-maven-3.2.1/bin' > .bash_profile
fi

# Erlang
download_unless_exist "http://www.erlang.org/download/otp_src_R16B02.tar.gz"
verify_checksum "otp_src_R16B02.tar.gz" 5bd028771290eacbc075ca65a63749e6 || exit
if [ ! -d otp_src_R16B02 ]; then
  tar -xzf otp_src_R16B02.tar.gz
  chown -R vagrant:vagrant otp_src_R16B02
  pushd otp_src_R16B02
  su -c 'CFLAGS="-DOPENSSL_NO_EC=1" ./configure --disable-hipe --enable-kernel-poll --without-odbc --enable-threads --enable-smp-support' vagrant
  su -c 'make' vagrant
  sudo make install
  popd
fi

# Riak
if [ ! -d riak ]; then
  su -c 'git clone -b rs-rinamo-vagrant http://github.com/basho/riak.git' vagrant
  pushd riak
  su -c 'make rel' vagrant
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
ulimit -n 4096
./bin/riak start
./bin/riak ping

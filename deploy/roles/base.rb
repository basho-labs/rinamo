name "base"
description "Base role."
run_list(
  "recipe[apt]",
  "recipe[ntp]",
  "recipe[openssh]",
  "recipe[htop]",
  "recipe[git]",
  "recipe[build-essential]",
  "recipe[java]",
  "recipe[scala]",
  "recipe[maven]",
  "recipe[erlang::source]"
)
default_attributes(
  "openssh" => {
    "server" => {
      "password_authentication" => "no"
    }
  },
  "java" => {
    "install_flavor" => "oracle",
    "jdk_version" => 7,
    "oracle" => {
      "accept_oracle_download_terms" => true
    }
  },
  "erlang" => {
    "source" => {
      "version" => "R16B02-basho4",
      "url" => "http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho4.tar.gz",
      "checksum" => "539263be01b584cac2f26f422cce576e12e1d8fd0aff2bc9ad876a33f1803d31",
      "build_flags" => "--disable-hipe --enable-smp-support --without-odbc --enable-m64-build"
    }
  }
)

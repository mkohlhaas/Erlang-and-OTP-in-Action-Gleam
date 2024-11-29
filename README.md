![Erlang and OTP in Action](https://images.manning.com/360/480/resize/book/f/d21069b-7a05-4be7-bcd8-7d38aff03e34/logan.png "Erlang and OTP in Action")

This repository contains the source code for the book [Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action), by Martin Logan, Eric Merritt, and Richard Carlsson, Manning Publications, 2010.

For installing Erlang use [Kerl](https://github.com/kerl/kerl) with following environment variables.
See also [Adopting Erlang](https://adoptingerlang.org/).

```shell
export KERL_DOC_TARGETS="man html pdf chunks"
export KERL_INSTALL_MANPAGES=yes
export KERL_BUILD_DOCS=yes
export KERL_BUILD_BACKEND=tarball
```

Then using Kerl you can build and install Erlang:

```shell
kerl list releases
kerl build 26.0 26.0
kerl install 26.0 ~/Kerl/26.0
. ~/Kerl/26.0/activate
kerl_deactivate # after using Erlang
```

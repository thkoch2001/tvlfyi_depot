#!/bin/sh
#
# Install dependencies required to build and test Git inside container
#

case "$jobname" in
Linux32)
	linux32 --32bit i386 sh -c '
		apt update >/dev/null &&
		apt install -y build-essential libcurl4-openssl-dev \
			libssl-dev libexpat-dev gettext python >/dev/null
	'
	;;
linux-musl)
	apk add --update build-base curl-dev openssl-dev expat-dev gettext \
		pcre2-dev python3 musl-libintl perl-utils ncurses >/dev/null
	;;
esac

FROM ubuntu:20.04
ARG VERSION=2.2

RUN echo 'tzdata tzdata/Areas select Europe' | debconf-set-selections \
 && echo 'tzdata tzdata/Zones/Europe select Paris' | debconf-set-selections
RUN apt update \
 && apt autoremove -y \
 && DEBIAN_FRONTEND=noninteractive apt install -y lsb-release wget \
      software-properties-common gnupg curl bison build-essential cmake doxygen \
      flex g++ git libffi-dev libncurses5-dev libsqlite3-dev make mcpp sqlite3 zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /tmp/souffle-src \
 && cd /tmp/souffle-src \
 && git clone https://github.com/souffle-lang/souffle.git \
 && cd souffle \
 && git checkout $VERSION \
 && cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
 && cmake --build build -j \
 && cmake --build build --target install \
 && cd /tmp \
 && rm -rf /tmp/souffle-src

VOLUME [/code]
WORKDIR /app
CMD ["souffle" "--help"]

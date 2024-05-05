FROM primordus/souffle-ubuntu:2.2

SHELL [ "/bin/bash", "-c" ]

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
# Set the PATH variable to include a custom directory
ENV PATH="${PATH}:/root/.local/bin"

# install packages
RUN apt-get update \
    && apt-get install -y build-essential curl libffi-dev  libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 locales \
    && echo "source /root/.ghcup/env" >> ~/.bashrc \
    # install ghcup, ghc-9.6.2 and cabal-3.10.1.0
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.6.2 BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0 \
    BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 BOOTSTRAP_HASKELL_ADJUST_BASHRC=P sh \
    && source /root/.ghcup/env \
    && cabal install hpack \
    && cabal install hspec-discover \
    && sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
    && locale-gen \
    && apt-get autoremove -y \
    && apt-get purge -y --auto-remove \
    && rm -rf /var/lib/apt/lists/*

VOLUME /code
WORKDIR /app/build

RUN echo -e '#!/bin/bash\nsource /root/.ghcup/env\nexec "$@"\n' > /app/build/entrypoint.sh \
    && chmod u+x /app/build/entrypoint.sh

# The entrypoint script sources ghcup setup script so we can easily call cabal etc.
ENTRYPOINT [ "/app/build/entrypoint.sh" ]

COPY . .

RUN source /root/.ghcup/env \
    && cabal update \
    && make configure \
    && make build

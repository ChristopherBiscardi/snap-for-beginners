FROM biscarch/ghc-7.8.3

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install zlib1g-dev libssl-dev -y

ENV LANG en_US.utf8

RUN cabal update

# Install Dependencies into sandbox. Each command is cached by Docker
# so we don't have to reinstall everything unless we make changes to 
# our .cabal file.
ADD ./site.cabal /opt/server/site.cabal
RUN cd /opt/server && cabal install --only-dependencies -j4

# Add Application Code
ADD ./src /opt/server/src
# Install Application
RUN cd /opt/server && cabal install

# Add production assets and run application
ADD ./log /opt/server/log
ADD ./static /opt/server/static
WORKDIR /opt/server
CMD ["/opt/server/dist/build/site/site"]

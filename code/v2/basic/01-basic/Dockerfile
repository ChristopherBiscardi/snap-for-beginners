FROM biscarch/ghc-7.8.3

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install zlib1g-dev libssl-dev -y

ENV LANG en_US.utf8

RUN cabal update
RUN cabal install snap

# Add Application Code
ADD ./Site.hs /opt/server/Site.hs

# run application 
CMD ["ghci", "/opt/server/Site.hs"]

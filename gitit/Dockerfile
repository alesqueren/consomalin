FROM haskell:7

# install gitit
RUN cabal update && \
    cabal install pandoc -fhighlighting --force-reinstalls && \
    cabal install gitit --force-reinstalls

# install git
RUN apt-get update && \
    apt-get install -y git && \
    git config --global user.email "gitit@gitit.com" && \
    git config --global user.name "gitit"

WORKDIR /app
CMD gitit -p 80

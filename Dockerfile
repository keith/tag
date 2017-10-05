FROM haskell:8.0.2

RUN apt-get -y update && apt-get -y install libpcre3-dev

COPY . /code/tag
WORKDIR /code/tag
RUN stack build

FROM ubuntu:16.04

WORKDIR /registry

HEALTHCHECK --interval=5m --timeout=10s \
  CMD curl -f http://localhost:3000/ || exit 1

# Install necessary libraries
RUN apt-get update && apt-get -qq -y install libmemcached-dev ca-certificates netbase wget gdebi-core curl

# Add built exectutable binary
ADD .stack-work/install/x86_64-linux/lts-13.12/8.6.4/bin/registry-server /registry/registry-server

# Add templates
ADD templates /registry/templates

# Add configs
ADD config/application.yml /registry/config/application.yml
ADD config/build-info.yml /registry/config/build-info.yml

CMD ["./registry-server"]

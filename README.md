# Pitch Deck Server

Server is a [Scala](https://www.scala-lang.org) project managed by [sbt.](https://www.scala-sbt.org) It serves as a backend for [Pitch Deck Client.](https://github.com/jokka/pitch-deck-client)

## How to run

If you happen to have [JDK 17](https://aws.amazon.com/corretto/) and [sbt](https://www.scala-sbt.org) installed, just run the following command.

```shell
sbt run
```

If you don’t, just run it using [Docker.](https://www.docker.com)

```shell
docker run -it --rm \
  --entrypoint /bin/bash \
  --workdir /root/app \
  --name pitch-deck-server \
  -p 9000:9000 \
  -v $(pwd):/root/app \
  sbtscala/scala-sbt:graalvm-ce-21.3.0-java17_1.7.2_2.13.9 \
  sbt run
```

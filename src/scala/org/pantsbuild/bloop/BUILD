scala_library(
  name = 'bloop-install',
  sources = ['Cycles.scala', 'Graph.scala', 'Tarjans.scala'],
  dependencies=[
    '3rdparty/jvm/ch/epfl/scala:bloop-config',
    '3rdparty/jvm/io/get-coursier:coursier',
    '3rdparty/jvm/com/lihaoyi:pprint',
    '3rdparty/jvm/com/lihaoyi:ujson',
  ],
)

jvm_binary(
  main = 'org.pantsbuild.bloop.Bloop',
  sources = ['Bloop.scala'],
  dependencies = [
    ':bloop-install'
  ]
)

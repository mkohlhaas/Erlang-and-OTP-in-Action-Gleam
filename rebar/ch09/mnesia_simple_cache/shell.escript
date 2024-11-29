#!/usr/bin/env escript

main(_) ->
  application:start(sasl),
  mnesia:start(),
  application:start(resource_discovery),
  application:start(simple_cache).

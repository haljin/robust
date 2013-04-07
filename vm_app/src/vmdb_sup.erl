-module(vmdb_sup).
%-behavior(supervisor).

-export([start/2, start/3, stop/0]).


start(Stock, Coins)->
    ok.

start(dets, StockDets, CoinDets)->
    ok.

stop()->
    ok.

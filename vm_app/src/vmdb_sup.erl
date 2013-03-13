-module(vmdb_sup).
%-behavior(supervisor).

-exports([start/2, start/3, stop/0]).


start(Stock, Coins)->
    ok.

start(dets, StockDets, CoinDets)->
    ok.

stop()->
    ok.

%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 十一月 2017 17:44
%%%-------------------------------------------------------------------
-module(pg_txn_echo_server).
-author("simon").

%% API
-export([echo/3]).

echo(SessionID, _Env, Input) ->
  lager:error("================================~p", [Input]),
  Header = ["Content-Type: text/plain; charset=utf-8\r\n\r\n"],
%%  Content = "echo返回：",
  mod_esi:deliver(SessionID, [Header, Input]).

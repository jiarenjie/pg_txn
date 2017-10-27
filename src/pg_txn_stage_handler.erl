%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十月 2017 18:53
%%%-------------------------------------------------------------------
-module(pg_txn_stage_handler).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  validate_format/1
  , create_req_model/2
  , validate_biz/2
  , save_req_model/2
]).
%%-------------------------------------------------------------------
-define(LARGER_STACKTRACE_1(X),
  lager:error("Error =~p,stacktrace=~s", [X, lager:pr_stacktrace(erlang:get_stacktrace())])).

%%-------------------------------------------------------------------
validate_format(PV) when is_list(PV) ->
  try
    pg_mcht_protocol:validate_format(PV)
  catch
    throw:{validate_format_fail, RespCd, RespMsg} ->
      throw({validate_format_fail, RespCd, RespMsg, {proplist, PV}})
  end.


%%-------------------------------------------------------------------
create_req_model(MIn, PV) when is_atom(MIn), is_list(PV) ->
  try
    pg_protocol:out_2_in(MIn, PV)
  catch
    _:X ->
      ?LARGER_STACKTRACE_1(X),
      lager:error("convert to mcht req [~p] error.PV = ~p", [MIn, PV]),
      throw({create_mcht_req_model_stop, <<"99">>, <<"请求协议转换错误"/utf8>>, {proplist, PV}})
  end.

%%-------------------------------------------------------------------
validate_biz(MIn, Protocol) when is_atom(MIn), is_tuple(Protocol) ->
  try
    pg_mcht_protocol:validate_biz(MIn, Protocol)
  catch
    throw:{validate_fail, RespCd, RespMsg} ->
      lager:error("validate req model fail! RespCd = ~p,RespMsg = ~ts,Model =~p",
        [RespCd, RespMsg, Protocol]),
      throw({validate_req_model_stop, RespCd, RespMsg, {model, MIn, Protocol}});
    _:X ->
      lager:error("Error = ~p,stack = ~s", [X, lager:pr_stacktrace(erlang:get_stacktrace())]),
      lager:error("validate req model error. Model = ~p", [Protocol]),
      throw({validate_req_model_stop, <<"99">>, <<"请求交易验证失败"/utf8>>, {model, MIn, Protocol}})
  end.

%%-------------------------------------------------------------------
save_req_model(MIn, Protocol) when is_atom(MIn), is_tuple(Protocol) ->
  try
    {ok, Repo} = pg_mcht_protocol:save(MIn, Protocol),
    {Protocol, Repo}
  catch
    _ :X ->
      ?LARGER_STACKTRACE_1(X),
      lager:error("save req model error. Model = ~p", [Protocol]),
      throw({save_req_model_stop, <<"99">>, <<"保存交易请求错误"/utf8>>, {model, MIn, Protocol}})
  end.

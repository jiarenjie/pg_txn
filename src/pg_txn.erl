-module(pg_txn).
-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([
  stage_handle_mcht_req/2
]).

-export([
  handle/2
]).

-define(APP, ?MODULE).
%%====================================================================
%% API functions
%%====================================================================
-spec stage_handle_mcht_req(PV, Options) -> MchtTxnLog when
  PV :: proplists:proplist(),
  Options :: list(),
  MchtTxnLog :: pg_model:pg_model().

stage_handle_mcht_req(PV, Options) when is_list(PV), is_list(Options) ->
  MIn = proplists:get_value(model_in, Options),
  MRepo = proplists:get_value(model_repo, Options),

  MchtReq = pg_protocol:out_2_in(MIn, PV),
  {ok, Repo} = pg_mcht_protocol:save(MIn, MchtReq),
  Repo.


%%-----------------------------------------------------------------
handle(M, Params) when is_atom(M) ->
  {ok, TxnConfig} = application:get_env(?APP, M),
  Stages = proplists:get_value(stages, TxnConfig),
  xfutils:cond_lager(?MODULE, debug, error, "Stages = ~p", [Stages]),

  F =
    fun({stage, {Stage, Options}}, Acc) ->
      AccNew = ?MODULE:Stage(Acc, Options),
      AccNew
    end,
  Result = lists:foldl(F, Params, Stages),
  Result.

%%====================================================================
%% Internal functions
%%====================================================================

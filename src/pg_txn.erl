-module(pg_txn).
-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([
  stage_handle_mcht_req/2
]).

-export([
  handle/2
  , repo_module/1
]).

-define(APP, ?MODULE).
%%-------------------------------------------------------------------
-define(LARGER_STACKTRACE_1(X),
  lager:error("Error =~p,stacktrace=~s", [X, lager:pr_stacktrace(erlang:get_stacktrace())])).
%%====================================================================
%% API functions
%%====================================================================
-spec stage_handle_mcht_req(PV, Options) -> MchtTxnLog when
  PV :: proplists:proplist(),
  Options :: list(),
  MchtTxnLog :: pg_model:pg_model().

stage_handle_mcht_req(PV, Options) when is_list(PV), is_list(Options) ->
  %% validate wire format
  pg_txn_stage_handler:validate_format(PV),

  %% create model
  MIn = proplists:get_value(model_in, Options),
  MRepo = proplists:get_value(model_repo, Options),
  MchtReq = pg_txn_stage_handler:create_req_model(MIn, PV),

  %% validate mcht req model
  pg_txn_stage_handler:validate_biz(MIn, MchtReq),

  %% save mcht req model
  {P, Repo} = pg_txn_stage_handler:save_req_model(MIn, MchtReq),
  {P, Repo}.


%%-----------------------------------------------------------------
repo_module(mchants) ->
  {ok, Module} = application:get_env(?APP, mcht_repo_name),
  Module;
repo_module(mcht_txn_log) ->
  {ok, Module} = application:get_env(?APP, mcht_txn_log_repo_name),
  Module;
repo_module(up_txn_log) ->
  {ok, Module} = application:get_env(?APP, up_txn_log_repo_name),
  Module.
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

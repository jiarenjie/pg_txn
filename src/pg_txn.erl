-module(pg_txn).
-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([
  stage_handle_mcht_req/2
]).

-export([
  handle/2
  , repo_module/1
  , txn_options/1
  , stage_options/2
]).

-export([
  txn_options_test_1/0
  , stage_options_test_1/0
  , stage_gen_up_req/2
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
  _MRepo = proplists:get_value(model_repo, Options),

  MchtReq = pg_txn_stage_handler:create_req_model(MIn, PV),

  %% validate mcht req model
  pg_txn_stage_handler:validate_biz(MIn, MchtReq),

  %% save mcht req model
  {P, Repo} = pg_txn_stage_handler:save_req_model(MIn, MchtReq),
  {P, Repo}.


%%-----------------------------------------------------------------
stage_gen_up_req({PMchtReq, RepoMchtTxnLog}, Options)
  when is_tuple(PMchtReq), is_tuple(RepoMchtTxnLog), is_list(Options) ->
  ?debugFmt("======================================================~n", []),
  ?debugFmt("Options = ~p", [Options]),

  %% convert to up req
  MOut = proplists:get_value(model_out, Options),
  ?debugFmt("Mout = ~p,PMchtReq = ~p", [MOut, PMchtReq]),
  PUpReq = pg_convert:convert(MOut, PMchtReq),
  ?debugFmt("PUpReq = ~p", [PUpReq]),

  %% sign
  Sig = pg_up_protocol:sign(MOut, PUpReq),
  PUpReqWithSig = pg_model:set(MOut, PUpReq, signature, Sig),
  ?debugFmt("PUpReqWithSig = ~p", [PUpReqWithSig]),

  %% convert to up_txn_log
  RepoUpReq = pg_convert:convert(MOut, PUpReq, save_req),
  ?debugFmt("RepoUpReq = ~p", [RepoUpReq]),

  %% save up_txn_log
  ok = pg_repo:save(RepoUpReq),


  {PUpReq, RepoUpReq}.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
repo_module(mchants = TableName) ->
  {ok, Module} = application:get_env(?APP, mcht_repo_name),
  TableName = pg_model:name(Module),
  Module;
repo_module(mcht_txn_log = TableName) ->
  {ok, Module} = application:get_env(?APP, mcht_txn_log_repo_name),
  TableName = pg_model:name(Module),
  Module;
repo_module(up_txn_log = TableName) ->
  {ok, Module} = application:get_env(?APP, up_txn_log_repo_name),
  TableName = pg_model:name(Module),
  Module.
%%-----------------------------------------------------------------
txn_options(MTxn) when is_atom(MTxn) ->
  {ok, TxnConfig} = application:get_env(?APP, MTxn),
  proplists:delete(stages, TxnConfig).

txn_options_test_1() ->
  Options = txn_options(mcht_txn_req_collect),
  ?assertEqual(body, proplists:get_value(resp_mode, Options)),
  ok.

%%-----------------------------------------------------------------
stage_options(MTxn, Stage) ->
  {ok, TxnConfig} = application:get_env(?APP, MTxn),
  Stages = proplists:get_value(stages, TxnConfig),

  F = fun
        ({stage, {StageName, Options}}, {StageName, AccIn}) ->
          {StageName, Options};
        (_, Acc) ->
          Acc
      end,
  {Stage, StageOptions} = lists:foldl(F, {Stage, undefined}, Stages),
  StageOptions.

stage_options_test_1() ->
  ?assertEqual([{model_in, pg_mcht_protocol_req_collect}, {model_repo, pg_txn_t_repo_mcht_txn_log_pt}],
    stage_options(mcht_txn_req_collect, stage_handle_mcht_req)),
  ok.

%%-----------------------------------------------------------------
handle(M, Params) when is_atom(M) ->
  {ok, TxnConfig} = application:get_env(?APP, M),
  Stages = proplists:get_value(stages, TxnConfig),
  xfutils:cond_lager(?MODULE, debug, error, "Stages = ~p", [Stages]),

  F =
    fun({stage, {Stage, StageOptions}}, LastParams) ->
      AccNew = ?MODULE:Stage(LastParams, StageOptions),
      AccNew
    end,

  try
    Result = lists:foldl(F, Params, Stages),
    Result
  catch
    throw:{_, RespCd, RespMsg, FailResult} = X ->
      try
        render_fail_result(M, TxnConfig, RespCd, RespMsg, FailResult)
      catch
        _:X ->
          ?LARGER_STACKTRACE_1(X),
          PVRespCdMsg = [{resp_code, RespCd}, {resp_msg, RespMsg}],
          xfutils:proplist_to_iolist(PVRespCdMsg)
      end;
    _:X ->
      ?LARGER_STACKTRACE_1(X)
  end.

%%====================================================================
%% Internal functions
%%====================================================================
render_fail_result(M, TxnConfig, RespCd, RespMsg, Result) ->
  RespMode = proplists:get_value(resp_mode, TxnConfig),
  do_render_fail_result(RespMode, M, TxnConfig, RespCd, RespMsg, Result).

do_render_fail_result(body, MTxn, TxnConfig, RespCd, RespMsg, {proplist, PV})
  when is_list(PV) ->
  %% before model create successfully
  %% return original PV , plus respcd/respmsg
  PVReturn = PV ++ [{<<"respCode">>, RespCd}, {<<"respMsg">>, RespMsg}],
  xfutils:proplist_to_iolist(PVReturn);
do_render_fail_result(body, MTxn, TxnConfig, RespCd, RespMsg, {model, MIn, Protocol})
  when is_atom(MTxn), is_atom(MIn), is_tuple(Protocol) ->
  PVRespCdMsg = [{resp_code, RespCd}, {resp_msg, RespMsg}],

  MTo = proplists:get_value(resp_protocol_model, TxnConfig),
  RespProtocol = pg_convert:convert(MTo, Protocol, fail_resp),
  %% validate it
%%  ok = pg_mcht_protocol:validate_biz(MTo, RespProtocol),

  RespProtocolWithResp = pg_model:set(MTo, RespProtocol, PVRespCdMsg),
  {SignString, Sign} = pg_mcht_protocol:sign(MTo, RespProtocolWithResp),
  RespProtocolWithSig = pg_model:set(MTo, RespProtocolWithResp, signature, Sign),
  PostFields = [signature | MTo:sign_fields()],
  In2OutMap = pg_mcht_protocol:in_2_out_map(),
  pg_model:to(MTo, RespProtocolWithSig, {poststring, PostFields, In2OutMap}).




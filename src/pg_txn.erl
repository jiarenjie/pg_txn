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
  , stage_send_up_req/2
  , stage_handle_up_resp/2
  , stage_return_mcht_resp/2
]).
-define(APP, ?MODULE).
%%-------------------------------------------------------------------
-define(LARGER_STACKTRACE_1(X),
  lager:error("Error =~p,stacktrace=~ts", [X, iolist_to_binary(lager:pr_stacktrace(erlang:get_stacktrace()))])).
%%====================================================================
%% API functions
%%====================================================================
%% stage 1
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
%% stage 2
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
%% stage 3
stage_send_up_req({PUpReq, RepoUpReq}, Options)
  when is_tuple(PUpReq), is_tuple(RepoUpReq), is_list(Options) ->
  %% convert PUpReq to post
  MIn = proplists:get_value(model_in, Options),
  PostBody = pg_up_protocol:in_2_out(MIn, PUpReq, post),

  %% send to unionpay
  PostUrl = up_config:get_config(up_back_url),
  ?debugFmt("===========================================", []),
  ?debugFmt("PostBody = ~ts~nPostUrl = ~p", [PostBody, PostUrl]),

  %% receive response
  try
    {ok, {Status, Headers, Body}} = httpc:request(post,
      {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", iolist_to_binary(PostBody)}, %% Request
      [],                 %% HTTPOptions
      [{body_format, binary}]                %% Options
    ),
    ?debugFmt("http Statue = ~p~nHeaders  = ~p~nBody=~ts~n", [Status, Headers, Body]),
    {Status, Headers, Body}
  catch
    _:X ->
      ?LARGER_STACKTRACE_1(X),
      lager:error("send up req to unionpay error,PostBody = ~ts", [PostBody]),
      throw({send_up_req_stop, <<"99">>, <<"上游通道接受错误">>, {model, MIn, PUpReq}})
  end.

%%-----------------------------------------------------------------
%% stage 4
stage_handle_up_resp({Status, Headers, Body}, Options) when is_binary(Body) ->
  ?debugFmt("Enter stage_handle_up_resp ====================", []),
  PV = xfutils:parse_post_body(Body),
  MIn = proplists:get_value(model_in, Options),
  PUpResp = pg_protocol:out_2_in(MIn, PV),
  ?debugFmt("PUpResp = ~p", [PUpResp]),
  [UpIndexKey, UpRespCd, UpRespMsg] = pg_up_protocol:get(MIn, PUpResp,
    [up_index_key, respCode, respMsg]),

  %% update up_txn_log
  ?debugFmt("UpIndexKey = ~p,UpRespCd = ~p,UpRespMsg = ~p", [UpIndexKey, UpRespCd, UpRespMsg]),
  MRepoUp = pg_txn:repo_module(up_txn_log),
  {ok, RepoUpNew} = pg_repo:update(MRepoUp, {up_index_key, UpIndexKey},
    [{up_respCode, UpRespCd}, {up_respMsg, UpRespMsg}, {txn_status, xfutils:up_resp_code_2_txn_status(UpRespCd)}]),

  %% update_mcht_txn_log
  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  MOut = proplists:get_value(model_out, Options),
  VL = pg_convert:convert(MOut, RepoUpNew),
  {ok, RepoMchtNew} = pg_repo:update(MRepoMcht, VL),
  ?debugFmt("VL = ~p~nRepoMchtNew = ~p", [VL, RepoMchtNew]),
  %%  MchtIndexKey = pg_model:get(MRepoUp, RepoUpNew, mcht_index_key),
  %%  {ok, RepoMchtNew} = pg_repo:update_pk(MRepoMcht, MchtIndexKey,
  %%    [{resp_code, UpRespCd}, {resp_msg, UpRespMsg}, {txn_status, xfutils:up_resp_code_2_txn_status(UpRespCd)}]),

  ?debugFmt("MRepoUp = ~ts~nMRepoMcht = ~ts", [pg_model:pr(MRepoUp, RepoUpNew), pg_model:pr(MRepoMcht, RepoMchtNew)]),
  {RepoUpNew, RepoMchtNew}.

%%-----------------------------------------------------------------
%% stage 5
stage_return_mcht_resp({RepoUpResp, RepoMchtNew}, Options) when is_tuple(RepoUpResp) ->
  MOut = proplists:get_value(model_out, Options),
  %% update mcht_txn_log
  PMchtResp = pg_convert:convert(MOut, RepoMchtNew, normal_resp),
  ?debugFmt("PMchtResp = ~p", [PMchtResp]),

  {SignString, Sig} = pg_mcht_protocol:sign(MOut, PMchtResp),
  lager:debug("Return mcht resp ,signstring = ~ts,sig=~p", [SignString, Sig]),
  ?debugFmt("Return mcht resp ,signstring = ~ts,sig=~p", [SignString, Sig]),

  PMchtRespWithSig = pg_model:set(MOut, PMchtResp, signature, Sig),
  ?debugFmt("PMchtRespWithSig = ~ts", [pg_model:pr(MOut, PMchtRespWithSig)]),

  %% resp body
  pg_mcht_protocol:in_2_out(MOut, PMchtRespWithSig, post).
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




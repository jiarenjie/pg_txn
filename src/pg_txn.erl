-module(pg_txn).
-include_lib("eunit/include/eunit.hrl").
-include_lib("xfutils/include/xfutils.hrl").

%% API exports
-export([
  stage_handle_mcht_req/2
]).

-export([
  handle/2
  , repo_module/1
  , txn_options/1
  , stage_options/2
  , mcht_sign/2
]).

-export([
  txn_options_test_1/0
  , stage_options_test_1/0
  , stage_gen_up_req/2
  , stage_send_up_req/2
  , stage_handle_up_resp/2
  , stage_return_mcht_resp/2
  , stage_handle_up_info/2
  , stage_return_mcht_info/2
  , stage_gen_up_query/2
  , stage_handle_up_resp_query/2
  , stage_handle_mcht_req_query/2
  %%对账文件处理
  , stage_gen_up_reconcile/2
  , stage_handle_up_resp_reconcile/2]).
-define(APP, ?MODULE).
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

  %% convert to up req
  MOut = proplists:get_value(model_out, Options),

  %% sign
  PUpReqWithSig = convert_sign(MOut, PMchtReq),

  lager:debug("PUpReqWithSig = ~p", [PUpReqWithSig]),

  %% convert to up_txn_log
  %% save up_txn_log
  ok = save_up_req(MOut, PUpReqWithSig),

  PUpReqWithSig.
%%-----------------------------------------------------------------
%% stage 3
stage_send_up_req(PUpReq, Options)
  when is_tuple(PUpReq), is_list(Options) ->
  %% convert PUpReq to post
  MIn = proplists:get_value(model_in, Options),
  PostBody = pg_up_protocol:in_2_out(MIn, PUpReq, post),

  %% send to unionpay
  UpUrlCfg = proplists:get_value(post_url, Options),
  PostUrl = up_config:get_config(UpUrlCfg),
  lager:debug("PostBody = ~ts~nPostUrl = ~p", [PostBody, PostUrl]),

  %% receive response
  try
%%    {200, Header, Body} = do_post(PostUrl, PostBody),
    {200, Header, Body} = xfutils:post(PostUrl, PostBody),


    %% add query request
%%    UpIndexKey = pg_up_protocol:get(MIn, PUpReq, up_index_key),
    QueryFlag = proplists:get_value(query_action, Options,on),
    case QueryFlag of
        off -> ok;
        _ -> issue_query_redo(MIn, PUpReq)

    end,
    {200, Header, Body}
  catch
    _:X ->
      ?LARGER_STACKTRACE_1(X),
      throw({send_up_req_stop, <<"99">>, <<"上游通道接受错误"/utf8>>, {model, MIn, PUpReq}})
  end.

%%-----------------------------------------------------------------
%% stage 4
stage_handle_up_resp({_Status, _Headers, Body}, Options) when is_binary(Body) ->
  PV = xfutils:parse_post_body(Body),
  MIn = proplists:get_value(model_in, Options),
  PUpResp = pg_protocol:out_2_in(MIn, PV),
  lager:debug("PUpResp = ~ts", [pg_model:pr(MIn, PUpResp)]),

  %% update up_txn_log
  MRepoUp = pg_txn:repo_module(up_txn_log),
  RepoUpNew = update_txn_log(MIn, {MIn, PUpResp}, MRepoUp),


  %% update_mcht_txn_log
  MOut = proplists:get_value(model_out, Options),
  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  RepoMchtNew = update_txn_log(MOut, {MRepoUp, RepoUpNew}, MRepoMcht),

  {RepoUpNew, RepoMchtNew}.

%%-----------------------------------------------------------------
%% stage 5
stage_return_mcht_resp({_RepoUpResp, RepoMchtNew}, Options) when is_tuple(RepoMchtNew) ->
  %% in_2_out
  MOut = proplists:get_value(model_out, Options),

  PMchtRespWithSig = convert_sign(MOut, RepoMchtNew, normal_resp),
  lager:debug("Resp msg verify result = ~p", [pg_mcht_protocol:verify(MOut, PMchtRespWithSig)]),

  %% resp body
  pg_mcht_protocol:in_2_out(MOut, PMchtRespWithSig, post).
%%-----------------------------------------------------------------
%% stage 6
stage_handle_up_info(PV, Options) when is_list(PV), is_list(Options) ->

  %% out_2_in
  MIn = proplists:get_value(model_in, Options),
  UpInfoCollect = pg_up_protocol:out_2_in(MIn, PV),
  lager:debug("UpInfoCollect = ~ts", [pg_model:pr(MIn, UpInfoCollect)]),

  %% maybe validate it

  %% update up_txn_log
  %% update status
  MRepoUp = pg_txn:repo_module(up_txn_log),
  RepoUpNew = convert_update_fetch(MIn, UpInfoCollect, MRepoUp),

  %% update quota
  update_quota(MRepoUp, RepoUpNew),

  %% update_mcht_txn_log
  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  MOut = proplists:get_value(model_out, Options),
  RepoMchtNew = convert_update_fetch(MOut, RepoUpNew, MRepoMcht),

  lager:debug("MRepoUp = ~ts~nMRepoMcht = ~ts",
    [pg_model:pr(MRepoUp, RepoUpNew), pg_model:pr(MRepoMcht, RepoMchtNew)]),
  {RepoUpNew, RepoMchtNew}.


%%-----------------------------------------------------------------
%% stage 7
stage_return_mcht_info({_RepoUp, RepoMcht}, Options) ->
%% post  to mcht back_url
  MOut = proplists:get_value(model_out, Options),

  PMchtInfoWithSig = convert_sign(MOut, RepoMcht, normal_resp),


  lager:debug("Info msg verify result = ~p", [pg_mcht_protocol:verify(MOut, PMchtInfoWithSig)]),

  %% resp body
  InfoBody = pg_mcht_protocol:in_2_out(MOut, PMchtInfoWithSig, post),
  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  InfoUrl = pg_model:get(MRepoMcht, RepoMcht, back_url),

  lager:debug("InfoUrl = ~ts,InfoBody = ~ts", [InfoUrl, list_to_binary(InfoBody)]),
  pg_redoer:add_notify(InfoUrl, list_to_binary(InfoBody)),

  Result = pg_model:get(MRepoMcht, RepoMcht, txn_status),

  [atom_to_binary(Result, utf8)].

%%-----------------------------------------------------------------
%% stage 8
stage_gen_up_query(UpIndexKey, Options) when is_list(Options), is_tuple(UpIndexKey) ->
  MRepoUp = pg_txn:repo_module(up_txn_log),
  MOut = proplists:get_value(model_out, Options),
  {ok, [RepoUp]} = pg_repo:fetch_index(MRepoUp, {up_index_key, UpIndexKey}),

  convert_sign(MOut, RepoUp).

%%-----------------------------------------------------------------
%% stage 9 , handle_up_resp_query
stage_handle_up_resp_query({_Status, _Headers, Body}, Options) when is_binary(Body) ->
  PV = xfutils:parse_post_body(Body),
  MIn = proplists:get_value(model_in, Options),
  PUpResp = pg_protocol:out_2_in(MIn, PV),
  lager:debug("PUpResp = ~ts", [pg_model:pr(MIn, PUpResp)]),

  %% if resp_query's respCode != 00
  %% exit directly
  %% otherwise , update up_txn_log/mcht_txn_log according to origRespCode/origRespMsg
  %% update @ 2017-11-26
  %% all resp code is accepted

%%  RespCode = pg_model:get(MIn, PUpResp, respCode),
%%  case RespCode of
%%    <<"00">> ->
%%      ok;
%%    _ ->
%%      lager:error("UpQuery return's not success! respCode = ~p,UpRespQuery = ~ts",
%%        [RespCode, pg_model:pr(MIn, PUpResp)]),
%%      throw({up_resp_return_fail, RespCode, PUpResp})
%%  end,

  %% update up_txn_log
  MRepoUp = pg_txn:repo_module(up_txn_log),

  RepoUpNew = convert_update_fetch(MIn, PUpResp, MRepoUp),

  %% update quota
  update_quota(MRepoUp, RepoUpNew),

  %% update_mcht_txn_log
  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  MOut = proplists:get_value(model_out, Options),

  RepoMchtNew = convert_update_fetch(MOut, RepoUpNew, MRepoMcht),

  {RepoUpNew, RepoMchtNew}.
%%-----------------------------------------------------------------
%% stage 10, handle_mcht_req_query
stage_handle_mcht_req_query(PV, Options) when is_list(PV), is_list(Options) ->
  %% validate wire format
  pg_txn_stage_handler:validate_format(PV),

  %% create model
  MIn = proplists:get_value(model_in, Options),

  PMchtReqQuery = pg_txn_stage_handler:create_req_model(MIn, PV),

  %% validate mcht req model
  pg_txn_stage_handler:validate_biz(MIn, PMchtReqQuery),

  %% orig txn exists,otherwise exception should be throw
  %% fetch orig
  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  OrigMchtIndexKey = pg_mcht_protocol:get(MIn, PMchtReqQuery, mcht_index_key),
  {ok, [OrigMchtTxn]} = pg_repo:fetch(MRepoMcht, OrigMchtIndexKey),

  {{}, OrigMchtTxn}.
%%  下载银联对账文件
stage_gen_up_reconcile({MMDD,MerId},Options) when
  is_binary(MMDD),is_binary(MerId),is_list(Options) ->

  TxnTime = list_to_binary(xfutils:now(txn)),
  CertId = up_config:get_mer_prop(MerId, certId),
  List = [
    {merId,MerId}
    ,{settleDate,MMDD}
    ,{txnTime,TxnTime}
    ,{certId,CertId}
  ]
  ,
  ProtocolRepo = pg_model:new(pg_up_protocol_req_reconcile,List),
  up_sign(pg_up_protocol_req_reconcile,ProtocolRepo).

stage_handle_up_resp_reconcile({_Status, _Headers, Body}, Options) when is_binary(Body)->
  PV = xfutils:parse_post_body(Body),
  MIn = proplists:get_value(model_in, Options),
  PUpResp = pg_protocol:out_2_in(MIn, PV),
  lager:debug("PUpResp = ~ts", [pg_model:pr(MIn, PUpResp)]),
  RespCode = pg_model:get(MIn,PUpResp,respCode),
  case RespCode of
    <<"98">> ->
      % file not exist
      MerId = pg_model:get(MIn,PUpResp,merId),
      SettleDate = pg_model:get(MIn,PUpResp,settleDate),
      lager:info("Reconcile file not exist! MerId = ~ts,Date = ~ts", [MerId, SettleDate]),
      {error,"file is not exist!!"}
      ;
    <<"00">> ->
      FileName = pg_model:get(MIn,PUpResp,fileName),
      FileContent = pg_model:get(MIn,PUpResp,fileContent),
      Bin = base64:decode(FileContent),
      Ret = xfutils:inflate(Bin),
      lager:debug("Ready to write reconcile file = ~ts", [FileName]),
      {ok,{FileName,Ret}}
  end.

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
        ({stage, {StageName, Options}}, {StageName, _AccIn}) ->
          {StageName, Options};
        (_, Acc) ->
          Acc
      end,
  {Stage, StageOptions} = lists:foldl(F, {Stage, undefined}, Stages),
  StageOptions.

stage_options_test_1() ->
  ?assertEqual([{model_in, pg_mcht_protocol_req_collect}],
    stage_options(mcht_txn_req_collect, stage_handle_mcht_req)),
  ok.

%%-----------------------------------------------------------------
handle(M, Params) when is_atom(M) ->
  {ok, TxnConfig} = application:get_env(?APP, M),
  Stages = proplists:get_value(stages, TxnConfig),
  xfutils:cond_lager(?MODULE, debug, error, "Stages = ~p", [Stages]),

  F =
    fun({stage, {Stage, StageOptions}}, LastParams) ->
      lager:debug("Enter stage=~p,Params = ~p,Options=~p", [Stage, LastParams, StageOptions]),
      AccNew = ?MODULE:Stage(LastParams, StageOptions),
      AccNew
    end,

  try
    Result = lists:foldl(F, Params, Stages),
    Result
  catch
    throw:{_, RespCd, RespMsg, FailResult} = X ->
      try
        lager:error("before render_fail_result, M = ~p,TxnConfig = ~p,RespCd = ~p,RespMsg = ~ts,FailResult = ~p",
          [M, TxnConfig, RespCd, RespMsg, FailResult]),
        render_fail_result(M, TxnConfig, RespCd, RespMsg, FailResult)
      catch
        _:X ->
          ?LARGER_STACKTRACE_1(X),
          lager:error("error on render_fail_result,return resp_cd = ~p,rsp_msg = ~ts",
            [RespCd, RespMsg]),
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

do_render_fail_result(body, _MTxn, _TxnConfig, RespCd, RespMsg, {proplist, PV})
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
  {_SignString, Sign} = pg_mcht_protocol:sign(MTo, RespProtocolWithResp),
  RespProtocolWithSig = pg_model:set(MTo, RespProtocolWithResp, signature, Sign),
  PostFields = [signature | MTo:sign_fields()],
  In2OutMap = pg_mcht_protocol:in_2_out_map(),
  pg_model:to(MTo, RespProtocolWithSig, {poststring, PostFields, In2OutMap}).


%%-------------------------------------------------------------
convert_sign(MFrom, P) when is_atom(MFrom), is_tuple(P) ->
  convert_sign(MFrom, P, default).
convert_sign(MFrom, P, ConvertTag) when is_atom(MFrom), is_tuple(P), is_atom(ConvertTag) ->
  MP = MFrom,
  PNew = pg_convert:convert(MFrom, P, ConvertTag),
  lager:debug("MFrom = ~p,P = ~p,ConvertTag = ~p,PNew = ~ts", [MFrom, P, ConvertTag, pg_model:pr(MFrom, PNew)]),
  #{channel_type := ChannelType} = apply(MP, options, []),
  PNewWithSig = case ChannelType of
                  mcht ->
                    mcht_sign(MP, PNew);
                  up ->
                    up_sign(MP, PNew)
                end,
  PNewWithSig.

%%-------------------------------------------------------------
up_sign(M, P) when is_atom(M), is_tuple(P) ->
  {SignString, Sig} = pg_up_protocol:sign(M, P),
  lager:debug("SignString=~ts,Sig = ~ts", [SignString, Sig]),
  pg_model:set(M, P, signature, Sig).

mcht_sign(M, P) when is_atom(M), is_tuple(P) ->
  {SignString, Sig} = pg_mcht_protocol:sign(M, P),
  lager:debug("SignString=~ts,Sig = ~ts", [SignString, Sig]),
  pg_model:set(M, P, signature, Sig).
%%-----------------------------------------------------------------
save_up_req(M, P) when is_atom(M), is_tuple(P) ->
  Repo = pg_convert:convert(M, P, save_req),
  lager:debug("Repo = ~ts", [pg_model:pr(pg_txn:repo_module(up_txn_log), Repo)]),
  ok = pg_repo:save(Repo).
%%-----------------------------------------------------------------
do_post(PostUrl, PostBody) when is_binary(PostUrl) ->

  {ok, {{_, StatusCode, _}, Headers, Body}} = httpc:request(post,
    {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", iolist_to_binary(PostBody)}, %% Request
    [],                 %% HTTPOptions
    [{body_format, binary}]                %% Options
  ),
  lager:debug("http StatusCode = ~p~nHeaders  = ~p~nBody=~ts~n", [StatusCode, Headers, Body]),
  try
    200 = StatusCode,
    {200, Headers, Body}

  catch
    _:X ->
      ?LARGER_STACKTRACE_1(X),
      lager:error("send up req to unionpay url ~p error,PostBody = ~ts", [PostUrl, PostBody])
  end.
%%-----------------------------------------------------------------
update_txn_log(MFrom, {MP, P}, MRepo) when is_atom(MFrom), is_atom(MP), is_tuple(P), is_atom(MRepo) ->
  VL = pg_convert:convert(MFrom, P),
  {ok, RepoNew} = pg_repo:update(MRepo, VL),
  lager:debug("After update_txn_log,MP=~p,P=~ts",
    [MFrom, pg_model:pr(MP, P)]),
  lager:debug("MRepo=~p,RepoNew=~ts", [MRepo, pg_model:pr(MRepo, RepoNew)]),
  RepoNew.
%%-----------------------------------------------------------------
convert_update_fetch(MIn, PIn, MRepoUpdate) when is_atom(MIn), is_atom(MRepoUpdate), is_tuple(PIn) ->
  %% convert
  VL = pg_convert:convert(MIn, PIn),
  %% update
  {ok, RepoNew} = pg_repo:update(MRepoUpdate, VL),
  lager:debug("VL = ~p,PIn = ~p,MIn = ~p", [VL, PIn, MIn]),
  lager:debug("VL = ~p,PIn = ~ts", [VL, pg_model:pr(MIn, PIn)]),
  lager:debug("RepoNew = ~ts", [pg_model:pr(MRepoUpdate, RepoNew)]),
  %% fetch
  RepoFull = fetch_orig(MRepoUpdate, RepoNew),
  lager:debug("RepoFull = ~ts", [pg_model:pr(MRepoUpdate, RepoFull)]),
  RepoFull.

fetch_orig(MRepo, Repo) when is_atom(MRepo), is_tuple(Repo) ->
  lager:debug("MRepo=~p,Repo=~ts", [MRepo, pg_model:pr(MRepo, Repo)]),
  PK = pg_repo:pk(MRepo),
  Indexes = pg_repo:indexes(MRepo),
  lager:debug("PK=~p,Indexes=~p", [PK, Indexes]),

  %% check does PK or Indexes exist
  %% if exist , the fetch orig

  F = fun
        (Key, {undefined, undefined} = Acc) ->
          case pg_model:get(MRepo, Repo, Key) of
            undefined ->
              %% not in VL, then next
              Acc;
            Value ->
              {Key, Value}
          end;
        (_Key, {KeyFound, ValueFound}) ->
          {KeyFound, ValueFound}
      end,

  {Key, Value} = lists:foldl(F, {undefined, undefined}, [PK | Indexes]),
  lager:debug("Key=~p,Value=~p", [Key, Value]),

  [RepoOrig] = case Key of
                 PK ->
                   pg_repo:read(MRepo, Value);
                 Key ->
                   pg_repo:read_index(MRepo, Key, Value)
               end,
  case RepoOrig of
    [] ->
      %% not found
      throw({orig_txn_not_found, MRepo, Repo});
    _ ->
      ok
  end,
  RepoOrig.
%%-----------------------------------------------------------------
update_quota(MRepoUp, RepoUp) when is_atom(MRepoUp), is_tuple(RepoUp) ->
  [UpTxnStatus, MchtIndexKey, TxnAmt, TxnType] = pg_model:get(MRepoUp, RepoUp,
    [txn_status, mcht_index_key, up_txnAmt, txn_type]),
  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  MchtTxnStatus = pg_repo:fetch_by(MRepoMcht, MchtIndexKey, txn_status),
  true = (not_found =/= MchtTxnStatus),
  {MchtId, TxnDate, _} = MchtIndexKey,
  lager:debug("RepoUp = ~ts", [pg_model:pr(MRepoUp, RepoUp)]),
  lager:debug("MchtId = ~p,TxnType=~p,TxnDate = ~p,TxnAmt = ~p", [MchtId, TxnType, TxnDate, TxnAmt]),
  do_update_quota({MchtId, TxnType, TxnDate, TxnAmt}, UpTxnStatus, MchtTxnStatus).

do_update_quota({MchtId, TxnType, TxnDate, TxnAmt}, success, waiting) ->
  pg_quota:update(MchtId, TxnType, TxnDate, TxnAmt);
do_update_quota(_, _, _) ->
  ok.

%%-----------------------------------------------------------------
issue_query_redo(M, P) ->
  TxnType = maps:get(txn_type, M:options()),
  lager:debug("TxnType = ~p", [TxnType]),
  do_issue_query_redo(TxnType, M, P).

do_issue_query_redo(query, _, _) ->
  %% no need add redoers
  ok;
do_issue_query_redo(_, M, P) ->
  UpIndexKey = pg_up_protocol:get(M, P, up_index_key),

  ActionFun = fun
                (QueryParam) ->
                  pg_txn:handle(up_txn_query, QueryParam)
              end,
  ResultHandleFun = fun
                      (Result) ->
                        %% query result = success/fail, no need query any more
                        %% query result = waiting, continue
                        case Result of
                          [<<"success">>] ->
                            ok;
                          [<<"fail">>] ->
                            ok;
                          [<<"waiting">>] ->
                            continue;
                          _ ->
                            continue
                        end
                    end,

  pg_redoer:add_query(UpIndexKey, ActionFun, ResultHandleFun),
  ok.

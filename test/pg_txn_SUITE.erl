%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十月 2017 22:02
%%%-------------------------------------------------------------------
-module(pg_txn_SUITE).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([]).

-define(M_Repo, pg_txn_t_repo_mcht_txn_log_pt).
-define(APP, pg_txn).

setup() ->
  lager:start(),
  pg_test_utils:setup(mnesia),
  application:start(pg_mcht_enc),
  application:start(pg_mcht_protocol),
  application:start(pg_up_protocol),
  application:start(pg_convert),
  application:start(up_config),

  pg_repo:drop(?M_Repo),
  pg_repo:init(?M_Repo),

  env_init(),

  db_init(),

  ok.


%%------------------------------------------------------------
db_init() ->
  RepoContents = [
    {pg_txn:repo_module(mchants),
      [
        [
          {id, 1}
          , {payment_method, [gw_collect1]}
        ],
        [
          {id, 2}
          , {payment_method, [gw_wap]}
        ],
        [
          {id, 3}
          , {payment_method, [gw_netbank]}
        ],
        [
          {id, 4}
          , {payment_method, [gw_netbank_only]}
        ]

      ]
    },
    {pg_txn:repo_module(mcht_txn_log),
      [

      ]
    },
    {pg_txn:repo_module(up_txn_log),
      [

      ]
    }
  ],
  pg_test_utils:db_init(RepoContents),
  ok.

%%------------------------------------------------------------
env_init() ->
  Cfgs = [
    {pg_mcht_protocol,
      [
%%        {debug, true},
        {mcht_repo_name, pg_txn_t_repo_mchants_pt}
        , {mcht_txn_log_repo_name, pg_txn_t_repo_mcht_txn_log_pt}
        , {up_txn_log_repo_name, pg_txn_t_repo_up_txn_log_pt}
      ]
    },
    {
      pg_convert,
      [
%%        {debug, true}
      ]

    },
    {pg_up_protocol,
      [
        {mchants_repo_name, pg_txn_t_repo_mchants_pt}
        , {up_repo_name, pg_txn_t_repo_up_txn_log_pt}
        , {debug, true}

      ]
    },
    {pg_txn,
      [
        {debug, true}
        , {mcht_repo_name, pg_txn_t_repo_mchants_pt}
        , {mcht_txn_log_repo_name, pg_txn_t_repo_mcht_txn_log_pt}
        , {up_txn_log_repo_name, pg_txn_t_repo_up_txn_log_pt}
        ,
        {mcht_txn_req_collect,
          [
            {stages,
              [
                {stage,
                  {stage_handle_mcht_req,
                    [
                      {model_in, pg_mcht_protocol_req_collect}
                      , {model_repo, pg_txn_t_repo_mcht_txn_log_pt}
                    ]
                  }
                },
                {stage,
                  {stage_gen_up_req,
                    [
                      {model_in, pg_mcht_protocol_req_collect}
                      , {model_out, pg_up_protocol_req_collect}
                    ]

                  }
                },
                {stage,
                  {stage_send_up_req,
                    [
                      {model_in, pg_up_protocol_req_collect}
                    ]
                  }
                },
                {stage,
                  {stage_handle_up_resp,
                    [
                      {model_in, pg_up_protocol_resp_collect}
                      , {model_out, pg_mcht_protocol_resp_collect}
                    ]
                  }
                },
                {stage,
                  {stage_return_mcht_resp,
                    [
                      {model_in, pg_up_protocol_resp_collect}
                      , {model_out, pg_mcht_protocol_resp_collect}
                    ]
                  }
                }
              ]
            },
            {resp_mode, body},
            {resp_protocol_model, pg_mcht_protocol_resp_collect},
            {fail_template, x},
            {succ_template, x}

          ]
        },
        {mcht_txn_req_batch_collect,
          [
            {stages,
              [
                {stage,
                  {stage_handle_mcht_req,
                    [
                      {model_in, pg_mcht_protocol_req_batch_collect}
                      , {model_repo, pg_txn_t_repo_mcht_txn_log_pt}
                    ]
                  }
                },
                {stage,
                  {stage_gen_up_req,
                    [
                      {model_in, pg_mcht_protocol_req_batch_collect}
                      , {model_out, pg_up_protocol_req_batch_collect}
                    ]

                  }
                },
                {stage,
                  {stage_send_up_req,
                    [
                      {model_in, pg_up_protocol_req_batch_collect}
                    ]
                  }
                },
                {stage,
                  {stage_handle_up_resp,
                    [
                      {model_in, pg_up_protocol_resp_batch_collect}
                      , {model_out, pg_mcht_protocol_resp_batch_collect}
                    ]
                  }
                },
                {stage,
                  {stage_return_mcht_resp,
                    [
                      {model_in, pg_up_protocol_resp_batch_collect}
                      , {model_out, pg_mcht_protocol_resp_batch_collect}
                    ]
                  }
                }
              ]
            },
            {resp_mode, body},
            {resp_protocol_model, pg_mcht_protocol_resp_batch_collect},
            {fail_template, x},
            {succ_template, x}

          ]
        }

      ]
    },
    {lager,
      [
        {log_root, "/tmp/logs/"},
        {handlers,
          [
            {lager_console_backend,
              [debug,
                {lager_default_formatter,
                  [date, " ", time
                    , " [", severity, "]"
                    , {module, [
                    module,
                    {function, [":", function], ""},
                    {line, [":", line], ""}], ""},
                    {pid, ["@", pid], ""},
                    message
                    , "\n"
                  ]
                }
              ]
            },
            {lager_file_backend, [{file, "error.log"}, {level, error}, {date, "$D23"}, {count, 60}]},
            {lager_file_backend, [{file, "console.log"}, {level, debug}, {date, "$D23"}, {count, 60}]}
          ]}
      ]}
  ],

  pg_test_utils:env_init(Cfgs).

%%------------------------------------------------------------

my_test_() ->
  {
    setup,
    fun setup/0,
    {
      inorder,
      [
        fun mchants_test_1/0
        , fun pg_txn:txn_options_test_1/0
        , fun pg_txn:stage_options_test_1/0
        , fun mcht_txn_req_collect_test_1/0

        , fun create_req_model_test_1/0
        , fun mcht_txn_req_collect_test_2/0

        , fun fail_render_test_1/0

        %% batch collect
        , fun batch_collect_test_1/0

      ]
    }
  }.

%%--------------------------------------------------------------------
qs(collect) ->
  [
    {<<"tranAmt">>, <<"50">>}
    , {<<"orderDesc">>, <<"测试交易"/utf8>>}
    , {<<"merchId">>, <<"00001">>}
    , {<<"tranId">>, <<"20171021095817473460847">>}
    , {<<"bankCardNo">>, <<"6216261000000000018">>}
    , {<<"tranDate">>, <<"20171021">>}
    , {<<"tranTime">>, <<"095817">>}
    , {<<"certifType">>, <<"01">>}
    , {<<"certifId">>, <<"341126197709218366">>}
    , {<<"certifName">>, <<"全渠道"/utf8>>}
    , {<<"phoneNo">>, <<"13552535506">>}
    , {<<"trustBackUrl">>, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>}
  ];
qs(batch_collect) ->
  [
    {<<"tranAmt">>, <<"130000">>}
    , {<<"orderDesc">>, <<"测试交易"/utf8>>}
    , {<<"merchId">>, <<"00001">>}
    , {<<"tranId">>, <<"20171111155907130382068">>}
    , {<<"tranDate">>, <<"20171111">>}
    , {<<"tranTime">>, <<"155907">>}
    , {<<"trustBackUrl">>, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>}
    , {<<"tranCount">>, <<"3">>}
    , {<<"batchNo">>, <<"0009">>}
    , {<<"fileContent">>, <<"eJx9kM1KAzEQx++FvkqZ/2STTY7iyUtPPXjVbcQeurukK1iZF5BCT3oqQg++gCeliE/Tj9dwtkrxA00CEzK/mV8Y26MeCQzpEiPS7VRpGNPJUIqrlGJZTI+rYZTmujwaN3JWFINpHdvYr5SYNNU4pv5Yzkc3+0QRUzO6GNSfF21TX1ZlVLiuJs2kSKO66XbIcyvcS43JIbBOPr5AEMdgdmBiIs4pU042r8v1y60oYZXQYwyBgZArAw45TgXekbOOtEK2y+f122zzdL97XP3w+Zz3PvvLp3mwDYaNbB/mm8Xiu8+wVV8AOLOA06Fl3uXInHP/+8yfPn33IYOX3Xy2Xt198QGkGyFQGzO0QzK+bQEof/C9A0jokSU=">>}
    , {<<"reqReserved">>, <<"eeee">>}
  ].

pk(collect) ->
  {<<"00001">>, <<"20171021">>, <<"20171021095817473460847">>};
pk(batch_collect) ->
  {<<"00001">>, <<"20171111">>, <<"20171111155907130382068">>}.

%%--------------------------------------------------------------------
sign(M, TxnName, PV) ->
  PV = qs(TxnName),
  Model = pg_protocol:out_2_in(M, PV),
  {SignString, Sig} = pg_mcht_protocol:sign(M, Model),
  {SignString, Sig}.


sig(collect) ->
  {_SignString, Sig} = sign(pg_mcht_protocol_req_collect, collect, qs(collect)),
  Sig;
sig(batch_collect) ->
  {_SignString, Sig} = sign(pg_mcht_protocol_req_batch_collect, batch_collect, qs(batch_collect)),
  Sig.

%%--------------------------------------------------------------------
sign_string(collect) ->
  {SignString, _Sig} = sign(pg_mcht_protocol_req_collect, collect, qs(collect)),
  SignString;
sign_string(batch_collect) ->
  {SignString, _Sig} = sign(pg_mcht_protocol_req_batch_collect, batch_collect, qs(batch_collect)),
  SignString.

%%--------------------------------------------------------------------
req_collect_pv() ->
  PV = qs(collect) ++ [{<<"signature">>, sig(collect)}],
  PV.
req_batch_collect_pv() ->
  PV = qs(batch_collect) ++ [{<<"signature">>, sig(batch_collect)}],
  PV.
%%--------------------------------------------------------------------
mchants_test_1() ->
  MMchants = pg_txn:repo_module(mchants),
  ?assertEqual([[gw_collect1]], pg_repo:fetch_by(MMchants, 1, [payment_method])),
  ?assertEqual([[gw_collect1]], pg_repo:fetch_by(MMchants, <<"001">>, [payment_method])),
  ok.
%%--------------------------------------------------------------------
mcht_txn_req_collect_test_1() ->
  TxnType = mcht_txn_req_collect,


  %% stage 1,handle_mcht_req
%%  PV = qs(collect) ++ [{<<"signature">>, sig(collect)}],
  PV = req_collect_pv(),
%%  {_, Result} = pg_txn:handle(mcht_txn_req_collect, PV),
  {PMchtReq, RepoMcht} = stage_action(TxnType, stage_handle_mcht_req, PV),
%%  Exp = {mcht_txn_log, {<<"00001">>, <<"20171021">>,
%%    <<"20171021095817473460847">>},
%%    collect, <<"00001">>, <<"20171021">>,
%%    <<"095817">>, <<"20171021095817473460847">>, 50,
%%    <<"测试交易"/utf8>>,
%%    undefined, undefined,
%%    <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>,
%%    undefined, undefined, undefined, undefined,
%%    undefined, undefined, undefined, undefined,
%%    undefined, waiting, <<"6216261000000000018">>,
%%    <<"01">>, <<"341126197709218366">>,
%%    <<"全渠道"/utf8>>,
%%    <<"13552535506">>},
  ?assertEqual(
    [
      {<<"00001">>, <<"20171021">>, <<"20171021095817473460847">>}
      , collect, <<"6216261000000000018">>,
      <<"01">>, <<"341126197709218366">>,
      <<"全渠道"/utf8>>,
      <<"13552535506">>
    ],
    pg_model:get(pg_txn:repo_module(mcht_txn_log), RepoMcht,
      [mcht_index_key, txn_type, bank_card_no, id_type, id_no, id_name, mobile])),

  %% stage 2,gen_up_req
  {PUpReq, RepoUp} = stage_action(TxnType, stage_gen_up_req, {PMchtReq, RepoMcht}),
  ?assertEqual([pk(collect), <<"6216261000000000018">>, <<"341126197709218366">>, <<"全渠道"/utf8>>, <<"13552535506">>],
    pg_model:get(pg_txn_t_repo_up_txn_log_pt, RepoUp, [mcht_index_key, up_accNo, up_idNo, up_idName, up_mobile])),
  ?debugFmt("PUpReq = ~p~nRepoUp = ~p", [PUpReq, RepoUp]),

  %% stage 3,send_up_req
  {Status, Headers, Body} = stage_action(TxnType, stage_send_up_req, {PUpReq, RepoUp}),
  ?assertEqual({"HTTP/1.1", 200, "OK"}, Status),
  ?assertEqual("UPJAS", proplists:get_value("server", Headers)),
  ?assertEqual(<<"UTF-8">>, proplists:get_value(<<"encoding">>, xfutils:parse_post_body(Body))),

  %% stage 4,handle_up_resp
  {RepoUpNew, RepoMchtNew} = stage_action(TxnType, stage_handle_up_resp, {Status, Headers, Body}),
  ?debugFmt("RepoUpNew = ~ts~nRepoMchtNew = ~ts", [pg_model:pr(pg_txn:repo_module(up_txn_log), RepoUpNew),
    pg_model:pr(pg_txn:repo_module(mcht_txn_log), RepoMchtNew)]),
  ?assertEqual(50, pg_model:get(pg_txn:repo_module(up_txn_log), RepoUpNew, up_txnAmt)),

  %% stage 5,send_mcht_resp
  ReturnBody = stage_action(TxnType, stage_return_mcht_resp, {RepoUpNew, RepoMchtNew}),
  ?debugFmt("ReturnBody = ~ts", [ReturnBody]),
  QueryId = pg_model:get(pg_txn:repo_module(up_txn_log), RepoUpNew, up_orderId),
  MatchResult = binary:match(iolist_to_binary(ReturnBody), <<"queryId=", QueryId/binary>>),
  ?assertNotEqual(nomatch, MatchResult),


  %% error
  PV1 = proplists:delete(<<"merchId">>, PV) ++ [{<<"merchId">>, <<"100">>}],
  ?assertThrow({validate_req_model_stop, <<"31">>, <<"商户号不存在"/utf8>>, {model, _, _}},
    stage_action(mcht_txn_req_collect, stage_handle_mcht_req, PV1)),

  ?assertThrow({validate_format_fail, <<"99">>, _, {proplist, _}},
    stage_action(mcht_txn_req_collect, stage_handle_mcht_req,
      proplists:delete(<<"merchId">>, PV) ++ [{<<"merchId">>, <<"d">>}])),

  ?assertThrow({validate_req_model_stop, <<"12">>, _, {model, _, _}},
    stage_action(mcht_txn_req_collect, stage_handle_mcht_req,
      proplists:delete(<<"signature">>, PV) ++ [{<<"signature">>, <<"AA">>}])),
  ok.
%%--------------------------------------------------------------------
mcht_txn_req_collect_test_2() ->
  TxnType = mcht_txn_req_collect,

  %% full link test
  db_init(),

  PV = req_collect_pv(),
  ResultBody = pg_txn:handle(TxnType, PV),
  MatchResult = binary:match(iolist_to_binary(ResultBody), <<"tranId=20171021095817473460847">>),
  ?assertNotEqual(nomatch, MatchResult),
  ok.

%%--------------------------------------------------------------------
create_req_model_test_1() ->
  db_init(),

  PVTxnAmtSmall = update_qs(pg_mcht_protocol_req_collect, qs(collect), [{<<"tranAmt">>, <<"1">>}]),
  MIn = pg_mcht_protocol_req_collect,

  ?assertEqual(1, pg_model:get(MIn, pg_txn_stage_handler:create_req_model(MIn, PVTxnAmtSmall), txn_amt)),

  ok.
%%--------------------------------------------------------------------
fail_render_test_1() ->
  db_init(),

  PV = qs(collect) ++ [{<<"signature">>, sig(collect)}],

  %% format error
  Result = pg_txn:handle(mcht_txn_req_collect,
    proplists:delete(<<"merchId">>, PV) ++ [{<<"merchId">>, <<"AA">>}]),
  MatchResult = binary:match(iolist_to_binary(Result), <<"respCode=99">>),
  ?assertNotEqual(nomatch, MatchResult),

  %% validate req fail
  PVTxnAmtSmall = update_qs(pg_mcht_protocol_req_collect, qs(collect), [{<<"tranAmt">>, <<"1">>}]),
  ?assertThrow({validate_req_model_stop, <<"33">>, _, {model, pg_mcht_protocol_req_collect, _}},
    stage_action(mcht_txn_req_collect, stage_handle_mcht_req, PVTxnAmtSmall)),
  Result1 = pg_txn:handle(mcht_txn_req_collect, PVTxnAmtSmall),
  MatchResult1 = binary:match(iolist_to_binary(Result1), <<"respCode=33">>),
  ?assertNotEqual(nomatch, MatchResult1),
  MatchResult11 = binary:match(iolist_to_binary(Result1), <<"respMsg=交易金额太小"/utf8>>),
  ?assertNotEqual(nomatch, MatchResult11),
  ok.

%%--------------------------------------------------------------------
stage_action(M, Stage, Params) ->
  Options = pg_txn:stage_options(M, Stage),
  pg_txn:Stage(Params, Options).

%%--------------------------------------------------------------------
update_qs(M, QS, UpdateKV) when is_list(QS), is_list(UpdateKV) ->
  F = fun
        ({Key, Value}, Acc) ->
          proplists:delete(Key, Acc) ++ [{Key, Value}]
      end,
  UpdateQS = lists:foldl(F, QS, UpdateKV),

  Protocol = pg_protocol:out_2_in(M, UpdateQS),
  {_, Sig} = pg_mcht_protocol:sign(M, Protocol),
  PWithSig = pg_model:set(M, Protocol, signature, Sig),
  OutFields = [signature | M:sign_fields()],
  pg_model:to(M, PWithSig, {proplists, OutFields, pg_mcht_protocol:in_2_out_map()}).

%%--------------------------------------------------------------------
batch_collect_test_1() ->
  db_init(),

  M = pg_mcht_protocol_req_batch_collect,
  TxnType = mcht_txn_req_batch_collect,
  PV = req_batch_collect_pv(),
  %% stage 1
  {PMchtReq, RepoMcht} = stage_action(TxnType, stage_handle_mcht_req, PV),
  ?assertEqual(
    [
      pk(batch_collect)
      , batch_collect, 9, 130000, 3
      , <<"eJx9kM1KAzEQx++FvkqZ/2STTY7iyUtPPXjVbcQeurukK1iZF5BCT3oqQg++gCeliE/Tj9dwtkrxA00CEzK/mV8Y26MeCQzpEiPS7VRpGNPJUIqrlGJZTI+rYZTmujwaN3JWFINpHdvYr5SYNNU4pv5Yzkc3+0QRUzO6GNSfF21TX1ZlVLiuJs2kSKO66XbIcyvcS43JIbBOPr5AEMdgdmBiIs4pU042r8v1y60oYZXQYwyBgZArAw45TgXekbOOtEK2y+f122zzdL97XP3w+Zz3PvvLp3mwDYaNbB/mm8Xiu8+wVV8AOLOA06Fl3uXInHP/+8yfPn33IYOX3Xy2Xt198QGkGyFQGzO0QzK+bQEof/C9A0jokSU=">>
    ],
    pg_model:get(pg_txn:repo_module(mcht_txn_log), RepoMcht,
      [mcht_index_key, txn_type, batch_no, txn_amt, txn_count, file_content])),
  ?assertEqual(<<"000012017111120171111155907130382068155907130000测试交易http://localhost:8888/pg/simu_mcht_back_succ_info39eJx9kM1KAzEQx++FvkqZ/2STTY7iyUtPPXjVbcQeurukK1iZF5BCT3oqQg++gCeliE/Tj9dwtkrxA00CEzK/mV8Y26MeCQzpEiPS7VRpGNPJUIqrlGJZTI+rYZTmujwaN3JWFINpHdvYr5SYNNU4pv5Yzkc3+0QRUzO6GNSfF21TX1ZlVLiuJs2kSKO66XbIcyvcS43JIbBOPr5AEMdgdmBiIs4pU042r8v1y60oYZXQYwyBgZArAw45TgXekbOOtEK2y+f122zzdL97XP3w+Zz3PvvLp3mwDYaNbB/mm8Xiu8+wVV8AOLOA06Fl3uXInHP/+8yfPn33IYOX3Xy2Xt198QGkGyFQGzO0QzK+bQEof/C9A0jokSU=eeee"/utf8>>,
    pg_mcht_protocol:sign_string(M, PMchtReq)),


  ok.

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
  pg_test_utils:lager_init(),

  pg_test_utils:setup(mnesia),
  application:start(pg_mcht_enc),
  application:start(pg_mcht_protocol),
  application:start(pg_up_protocol),
  application:start(pg_convert),
  application:start(up_config),
  application:start(inets),
  pg_test_utils:http_echo_server_init(),
  application:start(pg_quota),
  application:start(pg_redoer),

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
          , {quota, [{txn, 10000}, {daily, 20000}, {monthly, 30000}]}
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
    },
    {pg_quota:repo_module(mcht_txn_acc),
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
%%        , {debug, true}

      ]
    },
    {pg_quota,
      [
        {mcht_repo_name, pg_txn_t_repo_mchants_pt}
        , {mcht_txn_acc_repo_name, pg_txn_t_repo_mcht_txn_acc_pt}

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
                      , {post_url, up_back_url}
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
            {resp_protocol_model, pg_mcht_protocol_resp_collect}

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
                      , {post_url, up_back_url}
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
            {resp_protocol_model, pg_mcht_protocol_resp_batch_collect}

          ]
        },
        {up_txn_info_collect,
          [
            {stages,
              [
                {stage,
                  {stage_handle_up_info,
                    [
                      {model_in, pg_up_protocol_info_collect}
                      , {model_out, pg_txn_t_repo_up_txn_log_pt}
                    ]
                  }
                },
                {stage,
                  {stage_return_mcht_info,
                    [
                      {model_out, pg_mcht_protocol_info_collect}

                    ]
                  }}

              ]
            },
            {resp_mode, body},
            {resp_protocol_model, pg_mcht_protocol_resp_batch_collect}
          ]

        },
        {up_txn_query,
          [
            {stages,
              [
                {stage,
                  {stage_gen_up_query,
                    [
                      {model_out, pg_up_protocol_req_query}
                    ]
                  }
                },
                {stage,
                  {stage_send_up_req,
                    [
                      {model_in, pg_up_protocol_req_query}
                      , {post_url, up_query_url}
                    ]
                  }
                },
                {stage,
                  {stage_handle_up_resp_query,
                    [
                      {model_in, pg_up_protocol_resp_query}
                      , {model_out, pg_txn_t_repo_up_txn_log_pt}
                    ]
                  }
                },
                {stage,
                  {stage_return_mcht_info,
                    [
                      {model_out, pg_mcht_protocol_info_collect}

                    ]
                  }}

              ]
            },
            {resp_mode, body},
            {resp_protocol_model, pg_mcht_protocol_resp_batch_collect}
          ]

        },
        {mcht_txn_query,
          [
            {stages,
              [
                {stage,
                  {stage_handle_mcht_req_query,
                    [
                      {model_in, pg_mcht_protocol_req_query}
                    ]
                  }
                },
                {stage,
                  {stage_return_mcht_resp,
                    [
                      {model_in, pg_txn_t_repo_mcht_txn_log_pt}
                      , {model_out, pg_mcht_protocol_resp_query}

                    ]
                  }}

              ]
            },
            {resp_mode, body},
            {resp_protocol_model, pg_mcht_protocol_resp_batch_collect}
          ]

        },
        {up_reconcile,
          [
            {stages,
              [
                {stage,
                  {stage_gen_up_reconcile,
                    [
                      {model_out, pg_up_protocol_req_reconcile}
                    ]
                  }
                },
                {stage,
                  {stage_send_up_req,
                    [
                      {model_in, pg_up_protocol_req_reconcile}
                      , {post_url, up_file_url}
                      , {query_action, off}
                    ]
                  }
                },
                {stage,
                  {stage_handle_up_resp_reconcile,
                    [
                      {model_in, pg_up_protocol_resp_reconcile}
                    ]
                  }
                }
              ]
            }
          ]

        }

      ]
    }
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
        {timeout, 240, fun mchants_test_1/0}
        , {timeout, 240, fun pg_txn:txn_options_test_1/0}
        , {timeout, 240, fun pg_txn:stage_options_test_1/0}
        , {timeout, 240, fun mcht_txn_req_collect_test_1/0}

        , {timeout, 240, fun create_req_model_test_1/0}
        , {timeout, 240, fun mcht_txn_req_collect_test_2/0}

        , {timeout, 240, fun fail_render_test_1/0}

        %% batch collect
        , {timeout, 240, fun batch_collect_test_1/0}

        , {timeout, 240, fun echo_server_test_1/0}

        , {timeout, 240, fun info_collect_test_1/0}

        , {timeout, 240, fun up_reconcile_test_1/0}

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
    , {<<"trustBackUrl">>, <<"http://localhost:9999/esi/pg_test_utils_echo_server:echo">>}
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
  ];
qs(up_info_collect) ->
  [
    {<<"accNo">>, <<"y2diank/+z3sOUUEw68T4BgK2pgdKG1SuPkJi5Or1b/npHxwdor7gQpy3Cwejo9hJmhk8zwoS0reNkaT9x4QHzc9A7auFouODHVbvFfnhYXXLU5f6WIMZ4tlNTGxsXYOfSZgRr7Kc8Rw/nj8fR131tmSITUOENmCzhZ6F88voboLOlHz4eTf0cS/1yv9NDa7X2QWVluJR4hwiG+C0ND2WyjeLd7sAnDWGRwDPy2Ukw0QKO8//ZvIlwjlfqZPKWYNZYwLdll5DsoioPwChJpOCHN6rntK57kTDUr9LyCqTllztfH/i/YJR98TMOm4KL+c3ro9n6UmhWDasT8R3W7GeA==">>}
    , {<<"accessType">>, <<"0">>}
    , {<<"bizType">>, <<"000501">>}
    , {<<"certId">>, <<"68759585097">>}
    , {<<"currencyCode">>, <<"156">>}
    , {<<"encoding">>, <<"UTF-8">>}
    , {<<"merId">>, <<"777290058110097">>}
    , {<<"orderId">>, <<"20171115104441561205906">>}
    , {<<"payCardType">>, <<"01">>}
    , {<<"queryId">>, <<"201711151044411659858">>}
    , {<<"respCode">>, <<"00">>}
    , {<<"respMsg">>, <<"Success!">>}
    , {<<"settleAmt">>, <<"50">>}
    , {<<"settleCurrencyCode">>, <<"156">>}
    , {<<"settleDate">>, <<"1115">>}
    , {<<"signMethod">>, <<"01">>}
    , {<<"traceNo">>, <<"165985">>}
    , {<<"traceTime">>, <<"1115104441">>}
    , {<<"txnAmt">>, <<"50">>}
    , {<<"txnSubType">>, <<"00">>}
    , {<<"txnTime">>, <<"20171115104441">>}
    , {<<"txnType">>, <<"11">>}
    , {<<"version">>, <<"5.0.0">>}
    , {<<"signature">>, <<"SB7flyvLufIjuYHus4wb1+BqMuKoEsEYph3qGRvpjqQku5rT5o//T/hbC2j59gPA+wlZx/rti24M1A7WW1RnuH9DGk70VxTguOF07F7PFxKlflYa93zBKvY2hGvyizHlpqYMmYwoFng9PxGhlj8Cdv8spziax4pfYzVmV+V4NN90zDEqJv8eQfqUtOsCSEJCf4GJKvj8SIvFaO+QRcrlPh2RFKUVFhdmAPMMNE/9v4wJ6UNoUT8/04htkc8MjRE0HOJu5AqFIfsAEVke1aUc5IuEjcPTdUXespkExJfHPsC2SbTJp2QysHGNsWVBgnKAYw9/tmFfNPMC9o87MP9qQA==">>}
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
  PV = req_collect_pv(),
%%  {_, Result} = pg_txn:handle(mcht_txn_req_collect, PV),
  {PMchtReq, RepoMcht} = stage_action(TxnType, stage_handle_mcht_req, PV),
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
  PUpReq = stage_action(TxnType, stage_gen_up_req, {PMchtReq, RepoMcht}),
  ?debugFmt("PUpReq = ~p", [PUpReq]),

  %% stage 3,send_up_req
  {StatusCode, Headers, Body} = stage_action(TxnType, stage_send_up_req, PUpReq),
  ?assertEqual(200, StatusCode),
  ?assertEqual("UPJAS", proplists:get_value("server", Headers)),
  ?assertEqual(<<"UTF-8">>, proplists:get_value(<<"encoding">>, xfutils:parse_post_body(Body))),

  %% stage 4,handle_up_resp
  {RepoUpNew, RepoMchtNew} = stage_action(TxnType, stage_handle_up_resp, {StatusCode, Headers, Body}),
  ?debugFmt("RepoUpNew = ~ts~nRepoMchtNew = ~ts", [pg_model:pr(pg_txn:repo_module(up_txn_log), RepoUpNew),
    pg_model:pr(pg_txn:repo_module(mcht_txn_log), RepoMchtNew)]),
  ?assertEqual([50, waiting, collect], pg_model:get(pg_txn:repo_module(up_txn_log), RepoUpNew,
    [up_txnAmt, txn_status, txn_type])),

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
  %%-----------------------------------------------------------------------------------
  %% query
  %% stage 8, send query
  TxnTypeQuery = up_txn_query,
  UpIndexKey = pg_model:get(pg_txn:repo_module(up_txn_log), RepoUpNew, up_index_key),
  UpReqQuery = stage_action(TxnTypeQuery, stage_gen_up_query, UpIndexKey),
  ?debugFmt("UpIndexKey = ~p,UpReqQuery = ~ts", [UpIndexKey, pg_model:pr(pg_up_protocol_req_query, UpReqQuery)]),
  ?assertEqual([UpIndexKey], pg_up_protocol:get(pg_up_protocol_req_query, UpReqQuery, [up_index_key])),

  %% stage 3,send_up_req
  {StatusCodeQuery, HeaderQuery, BodyQuery} = stage_action(TxnTypeQuery, stage_send_up_req, UpReqQuery),
  ?debugFmt("Query resp ==> Status = ~p,Header=~p,Body=~ts", [StatusCodeQuery, HeaderQuery, BodyQuery]),
%%  ?assertEqual({"HTTP/1.1", 200, "OK"}, StatusQuery),
  ?assertEqual(200, StatusCodeQuery),
  ?assertEqual("UPJAS", proplists:get_value("server", HeaderQuery)),
  ?assertEqual(<<"UTF-8">>, proplists:get_value(<<"encoding">>, xfutils:parse_post_body(BodyQuery))),

  %% stage 9,handle_up_resp_query
  {RepoUpQueryNew, RepoMchtQueryNew} = stage_action(TxnTypeQuery, stage_handle_up_resp_query, {StatusCodeQuery, HeaderQuery, BodyQuery}),
  ?assertEqual([<<"00">>, success], pg_model:get(pg_txn:repo_module(up_txn_log), RepoUpQueryNew, [up_respCode, txn_status])),
  ?assertEqual([<<"00">>, success], pg_model:get(pg_txn:repo_module(mcht_txn_log), RepoMchtQueryNew, [resp_code, txn_status])),

  %% stage 7,return_mcht_info
  Return = stage_action(TxnTypeQuery, stage_return_mcht_info, {RepoUpQueryNew, RepoMchtQueryNew}),
  ?assertEqual([<<"success">>], Return),

  %%-----------------------------------------------------------------------------------
  %% mcht query
  TxnTypeMchtQuery = mcht_txn_query,
  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  PVMchtQuery = lists:zip([<<"merchId">>, <<"tranDate">>, <<"tranTime">>, <<"tranId">>],
    pg_model:get(MRepoMcht, RepoMchtQueryNew, [mcht_id, txn_date, txn_time, txn_seq])),
  P = pg_mcht_protocol:out_2_in(pg_mcht_protocol_req_query, PVMchtQuery),
  PSig = pg_txn:mcht_sign(pg_mcht_protocol_req_query, P),
  ?debugFmt("PSig = ~p", [PSig]),
  PVMchtQueryWithSig = PVMchtQuery ++ [{<<"signature">>, pg_model:get(pg_mcht_protocol_req_query, PSig, signature)}],
  {{}, OrigMchtTxn} = stage_action(TxnTypeMchtQuery, stage_handle_mcht_req_query, PVMchtQueryWithSig),
  ?assertEqual([success], pg_model:get(MRepoMcht, OrigMchtTxn, [txn_status])),

  ReturnBodyQuery = stage_action(TxnTypeMchtQuery, stage_return_mcht_resp, {{}, OrigMchtTxn}),
  ?debugFmt("ReturnBodyQuery = ~ts", [ReturnBodyQuery]),
  ?assertNotEqual(nomatch, binary:match(iolist_to_binary(ReturnBodyQuery), <<"origRespCode=", "00">>)),
  SuccessMsg = <<"成功%5B0000000%5D"/utf8>>,
  ?assertNotEqual(nomatch, binary:match(iolist_to_binary(ReturnBodyQuery), <<"origRespMsg=", SuccessMsg/binary>>)),
  ?assertNotEqual(nomatch, binary:match(iolist_to_binary(ReturnBodyQuery), <<"respCode=", "00">>)),
  ?assertNotEqual(nomatch, binary:match(iolist_to_binary(ReturnBodyQuery), <<"respMsg=", "success">>)),

  %% via pg_txn:handle
  ?assertEqual(ReturnBodyQuery, pg_txn:handle(TxnTypeMchtQuery, PVMchtQueryWithSig)),

  %%-----------------------------------------------------------------------------------
  %% quota check
  ?debugFmt("dirty all keys = ~p", [mnesia:dirty_all_keys(mcht_txn_acc)]),
  ?assertEqual(50, pg_repo:fetch_by(pg_txn_t_repo_mcht_txn_acc_pt, {1, collect, <<"20171021">>}, acc)),

  ok.
%%--------------------------------------------------------------------
mcht_txn_req_collect_test_2() ->
  TxnType = mcht_txn_req_collect,

  %% full link test
  db_init(),

  PV = req_collect_pv(),
  ?debugFmt("=======================~n", []),
  ?debugFmt("PV = ~p", [PV]),
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
  MatchResult11 = binary:match(iolist_to_binary(Result1), <<"respMsg=交易金额超限"/utf8>>),
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
  ?assertEqual(<<"9eJx9kM1KAzEQx++FvkqZ/2STTY7iyUtPPXjVbcQeurukK1iZF5BCT3oqQg++gCeliE/Tj9dwtkrxA00CEzK/mV8Y26MeCQzpEiPS7VRpGNPJUIqrlGJZTI+rYZTmujwaN3JWFINpHdvYr5SYNNU4pv5Yzkc3+0QRUzO6GNSfF21TX1ZlVLiuJs2kSKO66XbIcyvcS43JIbBOPr5AEMdgdmBiIs4pU042r8v1y60oYZXQYwyBgZArAw45TgXekbOOtEK2y+f122zzdL97XP3w+Zz3PvvLp3mwDYaNbB/mm8Xiu8+wVV8AOLOA06Fl3uXInHP/+8yfPn33IYOX3Xy2Xt198QGkGyFQGzO0QzK+bQEof/C9A0jokSU=00001测试交易eeee13000032017111120171111155907130382068155907http://localhost:8888/pg/simu_mcht_back_succ_info"/utf8>>,
    pg_mcht_protocol:sign_string(M, PMchtReq)),


  ok.
%%--------------------------------------------------------------------
echo_server_test_1() ->
  {StatusCode, _Header, Body} = xfutils:post("http://localhost:9999/esi/pg_test_utils_echo_server:echo", <<"a=b&c=d">>),
  ?assertEqual(200, StatusCode),
  ?assertEqual(<<"a=b&c=d">>, Body),
  ok.
%%--------------------------------------------------------------------
info_collect_test_1() ->
  db_init(),

  MRepoUp = pg_txn:repo_module(up_txn_log),
  ok = pg_repo:save(MRepoUp,
    pg_model:new(MRepoUp,
      [
        {mcht_index_key, {<<"00001">>, <<"20171115">>, <<"111">>}}
        , {up_index_key, {<<"777290058110097">>, <<"20171115104441">>, <<"20171115104441561205906">>}}
        , {up_orderId, <<"20171115104441561205906">>}
        , {txn_type, collect}
        , {up_txnAmt, 50}
      ])),

  MRepoMcht = pg_txn:repo_module(mcht_txn_log),
  ok = pg_repo:save(MRepoMcht,
    pg_model:new(MRepoMcht,
      [
        {mcht_index_key, {<<"00001">>, <<"20171115">>, <<"111">>}}
        , {back_url, <<"http://localhost:9999/esi/pg_test_utils_echo_server:echo">>}
        , {txn_amt, <<"50">>}
        , {order_desc, <<"测试交易"/utf8>>}
        , {mcht_id, <<"00001">>}
        , {txn_seq, <<"20171021095817473460847">>}
        , {bank_card_no, <<"6216261000000000018">>}
        , {txn_date, <<"20171021">>}
        , {txn_time, <<"095817">>}
        , {id_type, <<"01">>}
        , {id_no, <<"341126197709218366">>}
        , {id_name, <<"全渠道"/utf8>>}
        , {mobile, <<"13552535506">>}
        , {txn_type, collect}
      ])),

  TxnType = up_txn_info_collect,
  PV = qs(up_info_collect),

  {RepoUp, RepoMcht} = stage_action(TxnType, stage_handle_up_info, PV),
  ?assertEqual([{<<"00001">>, <<"20171115">>, <<"111">>}, <<"20171115104441561205906">>],
    pg_model:get(MRepoMcht, RepoMcht, [mcht_index_key, query_id])),

  stage_action(TxnType, stage_return_mcht_info, {RepoUp, RepoMcht}),


  %%-------------------------------
  pg_txn:handle(up_txn_info_collect, PV),

  ok.

up_reconcile_test_1()->
  TxnType = up_reconcile,
  PV = {<<"1127">>,<<"898319849000018">>},

  UpReconcileRepo = stage_action(TxnType, stage_gen_up_reconcile, PV),
  ?debugFmt("UpReconcileRepo = ~p", [UpReconcileRepo]),

  UpReqQuery = stage_action(TxnType,stage_send_up_req,UpReconcileRepo),
  {_Status, _Headers, Body} = UpReqQuery,
  ?debugFmt("Body = ~ts", [Body]),
  PostBody = xfutils:parse_post_body(Body),
  MIn = pg_up_protocol_resp_reconcile,
  PUpResp = pg_protocol:out_2_in(MIn, PostBody),
  RespCode = pg_model:get(MIn,PUpResp,respCode),
  ?assertEqual(<<"00">>,RespCode),
  FileName = pg_model:get(MIn,PUpResp,fileName),
  ?assertEqual(<<"898319849000018_20171127.zip">>,FileName),
  FileContent = pg_model:get(MIn,PUpResp,fileContent),
  Bin = base64:decode(FileContent),
  Ret = <<80,75,3,4,20,0,0,0,8,0,162,10,124,75,142,204,1,120,132,0,0,0,97,2,0,0,29,0,28,0,73,78,78,49,55,49,49,50,55,56,56,90,77,95,56,57,56,51,49,57,56,52,57,48,48,48,48,49,56,85,84,9,0,3,128,73,28,90,128,73,28,90,117,120,11,0,1,4,16,12,0,0,4,232,3,0,0,189,145,65,10,194,80,12,68,247,130,119,200,218,213,76,154,159,159,172,237,13,188,255,93,252,191,40,109,109,11,22,196,183,74,96,50,195,144,71,113,177,128,106,130,210,0,96,153,57,244,89,61,168,33,164,86,18,134,34,174,116,245,219,27,128,209,47,94,20,136,37,32,168,178,79,139,80,176,206,134,200,18,12,120,89,105,182,140,115,136,154,220,23,155,247,252,189,147,95,18,25,3,51,122,185,169,178,44,74,79,124,225,177,146,127,52,32,218,7,144,103,93,142,183,46,61,122,194,95,185,94,158,80,75,3,4,20,0,0,0,8,0,205,10,124,75,80,42,165,44,64,1,0,0,8,9,0,0,30,0,28,0,82,68,50,48,49,48,49,55,49,49,50,55,48,49,95,56,57,56,51,49,57,56,52,57,48,48,48,48,49,56,85,84,9,0,3,209,73,28,90,209,73,28,90,117,120,11,0,1,4,16,12,0,0,4,232,3,0,0,237,84,77,75,195,64,16,189,47,236,127,200,47,144,205,26,237,38,127,198,95,39,22,61,197,166,45,33,13,173,6,242,105,49,141,109,2,210,131,80,133,162,52,212,155,36,40,102,41,66,18,147,218,160,36,61,248,110,111,247,237,188,153,97,118,24,230,143,224,187,114,228,189,93,157,155,23,198,71,119,100,221,40,103,170,226,5,202,34,152,139,171,180,14,2,8,232,173,20,93,191,8,132,39,135,44,79,56,30,37,96,201,174,94,198,147,179,16,61,223,23,76,71,123,21,61,8,236,89,247,84,137,100,89,224,8,194,152,71,108,197,228,105,64,115,166,223,11,24,177,45,150,197,173,77,142,90,220,209,5,235,89,181,198,237,138,1,25,198,138,58,86,58,36,161,133,67,64,155,50,122,112,199,105,41,61,19,109,251,61,23,129,30,171,235,94,88,106,209,159,220,105,219,147,8,108,117,77,107,187,12,123,33,4,59,231,174,46,179,92,138,171,221,127,19,108,68,16,156,236,9,32,8,166,126,116,27,247,39,5,213,103,134,7,29,32,148,229,71,57,142,185,237,250,60,199,199,123,213,135,249,64,47,232,65,77,125,128,96,216,118,167,190,157,53,254,255,15,117,35,217,75,203,129,49,44,222,114,53,204,129,20,151,186,255,240,250,247,60,249,3,131,82,255,26,220,245,71,39,110,174,246,166,39,239,11,77,111,162,79,80,75,1,2,30,3,20,0,0,0,8,0,162,10,124,75,142,204,1,120,132,0,0,0,97,2,0,0,29,0,24,0,0,0,0,0,1,0,0,0,182,129,0,0,0,0,73,78,78,49,55,49,49,50,55,56,56,90,77,95,56,57,56,51,49,57,56,52,57,48,48,48,48,49,56,85,84,5,0,3,128,73,28,90,117,120,11,0,1,4,16,12,0,0,4,232,3,0,0,80,75,1,2,30,3,20,0,0,0,8,0,205,10,124,75,80,42,165,44,64,1,0,0,8,9,0,0,30,0,24,0,0,0,0,0,1,0,0,0,182,129,219,0,0,0,82,68,50,48,49,48,49,55,49,49,50,55,48,49,95,56,57,56,51,49,57,56,52,57,48,48,48,48,49,56,85,84,5,0,3,209,73,28,90,117,120,11,0,1,4,16,12,0,0,4,232,3,0,0,80,75,5,6,0,0,0,0,2,0,2,0,199,0,0,0,115,2,0,0,0,0>>,
  Ret2 = xfutils:inflate(Bin),
  ?assertEqual(Ret,Ret2),
  timer:sleep(1000),
  {ok,Ret3} = pg_txn:handle(TxnType,PV),
  ?assertEqual(Ret,Ret3)
.

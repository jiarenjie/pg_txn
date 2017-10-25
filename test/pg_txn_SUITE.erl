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

setup() ->
  lager:start(),
  pg_test_utils:setup(mnesia),
  application:start(pg_mcht_enc),

  pg_repo:drop(?M_Repo),
  pg_repo:init(?M_Repo),

  env_init(),

  ok.

env_init() ->
  Cfgs = [
    {pg_mcht_protocol,
      [
        {debug, true},
        {mcht_repo_name, pg_txn_t_repo_mcht_txn_log_pt}
      ]
    },
    {pg_txn,
      [
        {debug, true},
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
                }
              ]
            }
          ]
        }

      ]
    }
  ],

  pg_test_utils:env_init(Cfgs).

my_test_() ->
  {
    setup,
    fun setup/0,
    {
      inorder,
      [
        fun mcht_txn_req_collect_test_1/0

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
  ].

pk(collect) ->
  {<<"00001">>, <<"20171021">>, <<"20171021095817473460847">>}.

sig(collect) ->
  M = pg_mcht_protocol_req_collect,
  PV = qs(collect),
  Model = pg_protocol:out_2_in(M, PV),
  {SignString, Sig} = pg_mcht_protocol:sign(M, Model),
  {SignString, Sig}.
%%--------------------------------------------------------------------
mcht_txn_req_collect_test_1() ->
  PV = qs(collect) ++ [{<<"signature">>, sig(collect)}],
  Result = pg_txn:handle(mcht_txn_req_collect, PV),
  Exp = {mcht_txn_log, {<<"00001">>, <<"20171021">>,
    <<"20171021095817473460847">>},
    collect, <<"00001">>, <<"20171021">>,
    <<"095817">>, <<"20171021095817473460847">>, 50,
    <<"测试交易"/utf8>>,
    undefined, undefined,
    <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>,
    undefined, undefined, undefined, undefined,
    undefined, undefined, undefined, undefined,
    undefined, waiting, <<"6216261000000000018">>,
    <<"01">>, <<"341126197709218366">>,
    <<"全渠道"/utf8>>,
    <<"13552535506">>},
  ?assertEqual(Exp, Result),
  ok.
%%--------------------------------------------------------------------

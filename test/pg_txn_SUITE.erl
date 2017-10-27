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
          , {payment_method, [gw_collect]}
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
        {debug, true},
        {mcht_repo_name, pg_txn_t_repo_mchants_pt}
        , {mcht_txn_log_repo_name, pg_txn_t_repo_mcht_txn_log_pt}
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
                }
              ]
            },
            {resp_mode, body},
            {fail_template, x},
            {succ_template, x}

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
        fun mchants_test_1/0
        , fun pg_txn:txn_options_test_1/0
        , fun pg_txn:stage_options_test_1/0
        , fun mcht_txn_req_collect_test_1/0

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
  {_SignString, Sig} = pg_mcht_protocol:sign(M, Model),
  Sig.

sign_string(collect) ->
  M = pg_mcht_protocol_req_collect,
  PV = qs(collect),
  Model = pg_protocol:out_2_in(M, PV),
  {SignString, _Sig} = pg_mcht_protocol:sign(M, Model),
  SignString.
%%--------------------------------------------------------------------
mchants_test_1() ->
  MMchants = pg_txn:repo_module(mchants),
  ?assertEqual([[gw_collect]], pg_repo:fetch_by(MMchants, 1, [payment_method])),
  ?assertEqual([[gw_collect]], pg_repo:fetch_by(MMchants, <<"001">>, [payment_method])),
%%  ?debugFmt("Mchants 1 = ~p", [pg_repo:read(MMchants, 1)]),
%%  ?debugFmt("Mchants 001 = ~p", [pg_repo:read(MMchants, <<"001">>)]),
%%  ?debugFmt("Mchants 2 = ~p", [pg_repo:read(MMchants, 2)]),
  ok.
%%--------------------------------------------------------------------
mcht_txn_req_collect_test_1() ->
%%  ?debugFmt("pg_mcht_protocol's env = ~p", [application:get_all_env(pg_mcht_protocol)]),

  PV = qs(collect) ++ [{<<"signature">>, sig(collect)}],
  {_, Result} = pg_txn:handle(mcht_txn_req_collect, PV),
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


  %% error
  PV1 = proplists:delete(<<"merchId">>, PV) ++ [{<<"merchId">>, <<"100">>}],
  ?assertThrow({validate_req_model_stop, <<"31">>, <<"商户号不存在"/utf8>>, _},
    stage_action(mcht_txn_req_collect, stage_handle_mcht_req, PV1)),

  ok.
%%--------------------------------------------------------------------


stage_action(M, Stage, Params) ->
  Options = pg_txn:stage_options(M, Stage),
  pg_txn:Stage(Params, Options).

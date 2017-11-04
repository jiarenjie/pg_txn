%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2016 8:17 PM
%%%-------------------------------------------------------------------
-module(pg_txn_t_repo_mcht_txn_log_pt).
-compile({parse_trans, exprecs}).
-behavior(pg_repo).
-author("simon").

%%-define(BH, behaviour_repo).
%% API
%% callbacks
-export([
  table_config/0
]).

-export([
  pr_formatter/1
]).
-compile(export_all).
%%-------------------------------------------------------------
-define(TBL, mcht_txn_log).


-type txn_type() :: pay |refund|gws_up_query.
-type status() :: success |waiting |fail.
-type txn_amt() :: non_neg_integer().

-export_type([txn_type/0, status/0, txn_amt/0]).

-record(?TBL, {
  mcht_index_key
  , txn_type :: txn_type()
  , mcht_id
  , txn_date
  , txn_time
  , txn_seq
  , txn_amt :: txn_amt()
  , order_desc
  , gateway_id
  , bank_id
  , back_url
  , front_url

  , query_id
  , settle_date
  , quota
  , resp_code
  , resp_msg

  , orig_mcht_txn_date
  , orig_mcht_txn_seq
  , orig_query_id

  , txn_status = waiting :: status()
  , bank_card_no
  , id_type
  , id_no
  , id_name
  , mobile


}).
-type ?TBL() :: #?TBL{}.
-export_type([?TBL/0]).

-export_records([?TBL]).
%%-------------------------------------------------------------
%% call backs
table_config() ->
  #{
    table_indexes => [txn_date]
    , data_init => []
    , pk_is_sequence => false
    , pk_key_name => mcht_index_key
    , pk_type => tuple

    , unique_index_name => mcht_index_key
    , query_option =>
  #{
    mcht_id => integer_equal
  }

  }.

pr_formatter(Field)
  when (Field =:= id_no)
  or (Field =:= id_name)
  or (Field =:= mobile)
  or (Field =:= order_desc)
  or (Field =:= resp_msg)
  ->
  string;
pr_formatter(_) ->
  ok.

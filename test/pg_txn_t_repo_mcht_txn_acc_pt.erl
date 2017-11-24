%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2016 8:17 PM
%%%-------------------------------------------------------------------
-module(pg_txn_t_repo_mcht_txn_acc_pt).
-compile({parse_trans, exprecs}).
-behavior(pg_repo).
-author("simon").

%% API
%% callbacks
-export([
  %% table define related
  table_config/0
]).

-compile(export_all).
%%-------------------------------------------------------------
-define(TBL, mcht_txn_acc).

-type month_binary() :: <<_:48>>.
-type date_binary() :: <<_:64>>.
-type month_date_binary() :: month_binary() | date_binary().

-record(?TBL, {
  acc_index
  , mcht_id :: non_neg_integer()
  , txn_type :: pg_mcht_protocol:txn_type()
  , month_date :: month_date_binary()
  , acc :: non_neg_integer()
}).
-type ?TBL() :: #?TBL{}.
-export_type([?TBL/0]).

-export_records([?TBL]).
%%-------------------------------------------------------------
%% call backs
table_config() ->
  #{
    table_indexes => []
    , data_init => []
    , pk_is_sequence => false
    , pk_key_name => acc_index
    , pk_type => tuple

    , unique_index_name => undefined
    , query_option =>
  #{
  }

  }.


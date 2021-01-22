%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0,
         put_account/1, get_account/1,
         put_transaction/1, get_transaction/1, get_all_transactions/0, get_all_transactions/1, get_transactions_from/1,  
         unique_tx_id/0,
         atomically/1]).

%% id-table for atomic id increment
-record(table_id, {table_name :: mnesia:table(), last_id :: non_neg_integer()}).

%% destroy tables in case they already existed
destroy_tables() ->
    mnesia:del_table_copy(transaction, node()),
    mnesia:del_table_copy(account, node()),
    mnesia:del_table_copy(table_id, node()).

% unfortunately, delete_table doesn't always work such that create_table doesn't fail, so don't check return value
create_tables() ->
    mnesia:create_table(transaction, [{attributes, [id, timestamp, from_acc_nr, to_acc_nr, amount]}]),
    mnesia:create_table(account, [{attributes, [account_number, amount]}]),
    mnesia:create_table(table_id, [{record_name, table_id}, {attributes, record_info(fields, table_id)}]).

init_database() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    destroy_tables(),
    create_tables(),
    ok = mnesia:wait_for_tables([ transaction, account, table_id], 1000),
    ok = create_test_accounts(),
    ok.

create_test_accounts() ->
    Account1 = #account{account_number = 42, amount = 100 },
    Account2 = #account{account_number = 43, amount = 100 },
    Account3 = #account{account_number = 44, amount = 100 },
    database:put_account(Account1),
    database:put_account(Account2),
    database:put_account(Account3),

    % get with pattern matching variants
    {ok, #account{account_number = 42, amount = 100 }} = database:get_account(42),
    {ok, Account2} = database:get_account(43),
    {ok, Account3} = database:get_account(44),
    ok.

write(Table, Tuple) ->
    Fun = fun() -> ok = mnesia:write(Table, erlang:insert_element(1, Tuple, Table), write) end,
    {atomic, ok} = mnesia:transaction(Fun),
    ok.

-spec read_one(mnesia:table(), unique_id(), fun((tuple()) -> Obj)) -> {ok, Obj} | {error, not_found | more_than_one}.
read_one(Table, Id, Deserialize) ->
    Fun = fun() -> mnesia:read(Table, Id) end,
    {atomic, Res} = mnesia:transaction(Fun),
    case Res of
        [Tuple] -> {ok, Deserialize(erlang:delete_element(1, Tuple))};
        []  -> {error, not_found};
        [_ | _] -> {error, more_than_one}
    end.

-spec read_all(mnesia:table(), fun((tuple()) -> Obj)) -> list(Obj).
read_all(Table, Deserialize) ->
    Fun = fun() -> mnesia:select(Table,[{'_',[],['$_']}]) end,
    {atomic, Res} = mnesia:transaction(Fun),
    lists:map(fun (Tuple) -> Deserialize(erlang:delete_element(1, Tuple)) end, Res).

-spec put_account(#account{}) -> ok.
put_account(#account{account_number = AccountNumber, amount = Amount}) ->
    write(account, {AccountNumber, Amount}).

deserialize_account({AccountNumber,  Amount}) ->
    #account{account_number = AccountNumber, amount = Amount}.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) ->
    read_one(account, AccountNumber, fun deserialize_account/1).

-spec put_transaction(#transaction{}) -> ok.
put_transaction(#transaction{id = Id, timestamp = Timestamp, from_acc_nr = FromAccNr, to_acc_nr = ToAccNr, amount = Amount}) ->
    write(transaction, {Id, Timestamp, FromAccNr, ToAccNr, Amount}).

deserialize_transaction({Id, Timestamp, FromAccNr, ToAccNr, Amount}) ->
    #transaction{id = Id, timestamp = Timestamp, from_acc_nr = FromAccNr, to_acc_nr = ToAccNr, amount = Amount}.

-spec get_transaction(unique_id()) -> {ok, #transaction{}} | {error, any()}.
get_transaction(Id) ->
    read_one(transaction, Id, fun deserialize_transaction/1).

-spec get_all_transactions() -> list(#transaction{}).
get_all_transactions() -> read_all(transaction, fun deserialize_transaction/1).

-spec get_all_transactions(account_number()) -> list(#transaction{}).
get_all_transactions(AccountNr) ->
    Fun = fun() ->
            mnesia:select(transaction,
                           [{'$1',
                            [{'orelse',
                                {'==', {element, #transaction.from_acc_nr, '$1'}, AccountNr},
                                {'==', {element, #transaction.to_acc_nr, '$1'}, AccountNr}}],
                            ['$_']}]) 
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    lists:map(fun (Tuple) -> deserialize_transaction(erlang:delete_element(1, Tuple)) end, Res).


-spec unique_tx_id() -> unique_id().
unique_tx_id() -> mnesia:dirty_update_counter(table_id, transaction, 1).

-spec atomically(fun(() -> Ret)) -> Ret.
atomically(Fun) ->
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

-spec get_transactions_from(unique_id()) -> list(#transaction{}).
get_transactions_from(Id) ->
    Fun = fun() ->
            mnesia:select(transaction,
                          [{'$1',
                            [{'>=', {element, #transaction.id, '$1'}, Id}],
                            ['$_']}])
                      end,
    {atomic, Res} = mnesia:transaction(Fun),
    lists:map(fun (Tuple) -> deserialize_transaction(erlang:delete_element(1, Tuple)) end, Res).


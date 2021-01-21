%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0,
         put_account/1, get_account/1, get_all_accounts/0,
         put_person/1, get_person/1, get_all_persons/0, 
         put_transaction/1, get_transaction/1, get_all_transactions/0, get_all_transactions/1, 
         unique_account_number/0,unique_tx_id/0, unique_person_id/0,
         atomically/1]).

%% id-table for atomic id increment
-record(table_id, {table_name :: mnesia:table(), last_id :: non_neg_integer()}).

%% destroy tables in case they already existed
destroy_tables() ->
    mnesia:delete_table(person),
    mnesia:delete_table(transaction),
    mnesia:delete_table(account),
    mnesia:delete_table(table_id).

create_tables() ->
    {atomic, ok} = mnesia:create_table(person, [{attributes, [id,firstname,surname]}]),
    {atomic, ok} = mnesia:create_table(transaction, [{attributes, [id, timestamp, from_acc_nr, to_acc_nr, amount]}]),
    {atomic, ok} = mnesia:create_table(account, [{attributes, [account_number, person_id, amount]}]),
    {atomic, ok} = mnesia:create_table(table_id, [{record_name, table_id}, {attributes, record_info(fields, table_id)}]).

init_database() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:transaction(fun destroy_tables/0),
    mnesia:transaction(fun create_tables/0),
    mnesia:wait_for_tables([person, transaction, account, table_id], 1000),
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
put_account(#account{account_number = AccountNumber, person_id = PersonId, amount = Amount}) ->
    write(account, {AccountNumber, PersonId, Amount}).

deserialize_account({AccountNumber, PersonId, Amount}) ->
    #account{account_number = AccountNumber, person_id = PersonId, amount = Amount}.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) ->
    read_one(account, AccountNumber, fun deserialize_account/1).

-spec get_all_accounts() -> list(#account{}).
get_all_accounts() -> read_all(account, fun deserialize_account/1).

-spec put_person(#person{}) -> ok.
put_person(#person{id = Id, firstname = Firstname, surname = Surname}) ->
    write(person, {Id, Firstname, Surname}).

deserialize_person({Id, Firstname, Surname}) ->
    #person{id = Id, firstname = Firstname, surname = Surname}.

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) ->
    read_one(person, Id, fun deserialize_person/1).

-spec get_all_persons() -> list(#person{}).
get_all_persons() -> read_all(person, fun deserialize_person/1).

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

-spec unique_account_number() -> unique_id().
unique_account_number() -> mnesia:dirty_update_counter(table_id, account, 1).

-spec unique_person_id() -> unique_id().
unique_person_id() -> mnesia:dirty_update_counter(table_id, person, 1).

-spec unique_tx_id() -> unique_id().
unique_tx_id() -> mnesia:dirty_update_counter(table_id, transaction, 1).

-spec atomically(fun(() -> Ret)) -> Ret.
atomically(Fun) ->
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

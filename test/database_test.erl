-module(database_test).
-include_lib("eunit/include/eunit.hrl").
-include("data.hrl").

setup() ->
    database:init_database().

cleanup(_) -> ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [ fun put_account/1, fun put_transaction/1]
     }}.


put_account(_) ->
    fun() ->
            Account = #account{account_number = 42, amount = 100 },
            database:put_account(Account),
            {ok, Account} = database:get_account(42)

    end.

put_transaction(_) ->
    fun() ->
            Transaction = #transaction{id = 17, timestamp = {1610,547469,326863}, from_acc_nr = 17, to_acc_nr = 32, amount = 100 },
            database:put_transaction(Transaction),
            ?assertEqual(database:get_transaction(17), {ok, Transaction}),
            ?assertEqual(database:get_all_transactions(), [Transaction]),
            ?assertEqual(database:get_all_transactions(17), [Transaction]),
            ?assertEqual(database:get_all_transactions(16), [])
    end.






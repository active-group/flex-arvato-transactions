-module(transactiontest).
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
      [ fun get_transactions/1]
     }}.


get_transactions(_) ->
  fun() ->
    % 2 transaktionen anlegen (AAA)
    database:init_database(),
    TestedAccountId = 1017,


    %überweisung
    Transaction1 = #transaction{id = 1, timestamp = {1610,547469,326863}, from_acc_nr = TestedAccountId, to_acc_nr = 1032, amount = 100 },
    database:put_transaction(Transaction1),

    %Abbuchung
    Transaction2 = #transaction{id = 2, timestamp = {1611,547469,326863}, from_acc_nr = 1033, to_acc_nr = TestedAccountId, amount = 50 },
    database:put_transaction(Transaction2),


    %testseupt verifizieren
    ?assertEqual({ok, Transaction1},database:get_transaction(1)),
    ?assertEqual({ok, Transaction2},database:get_transaction(2)),
    ?assertEqual([Transaction1,Transaction2],database:get_all_transactions()),
    ?assertEqual([Transaction1,Transaction2], database:get_all_transactions(TestedAccountId)),
    ?assertEqual([],database:get_all_transactions(1016)),


    %Liste holen, assertion formulieren
    Result =  business_logic:get_transactions(TestedAccountId),
    ?assertEqual([Transaction1,Transaction2], Result)

  end.










-module(transactiontest).
-include_lib("eunit/include/eunit.hrl").
-include("data.hrl").
-include("transactions_events.hrl").

setup() ->
    database:init_database().

cleanup(_) -> ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [ fun get_transactions/1, fun put_testAccounts/1,
        fun transactions_server_test/1 ]
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


put_testAccounts(_) ->
    fun() ->
            Account1 = #account{account_number = 1, amount = 100 },
            Account2 = #account{account_number = 2, amount = 100 },
            Account3 = #account{account_number = 3, amount = 100 },
            database:put_account(Account1),
            database:put_account(Account2),
            database:put_account(Account3),

            {ok, Account1} = database:get_account(1),
            {ok, Account2} = database:get_account(2),
            {ok, Account3} = database:get_account(3)
    end.

transactions_server_test(_) ->
  fun () ->
    erlbank_transactions_server:start(),
    business_logic:make_account(1, 700),
    business_logic:make_account(2, 400),
    business_logic:transfer(1, 2, 200),
    business_logic:transfer(2, 1, 300),
    gen_server:cast(transaction_service,
      #transaction_event_subscription{ 
            from_transaction_id = 0, 
            subscriber_pid = self()}),
    receive
      {_, Transaction1} -> 
        #transaction_event{ transaction_id = 1, 
                       from_acc_nr = 1, to_acc_nr = 2, 
                       amount = 200,
                       from_account_resulting_balance = 500,
                       to_account_resulting_balance = 600} = Transaction1
    end,
    receive
      {_, Transaction2} -> 
         #transaction_event{ transaction_id = 2, 
                       from_acc_nr = 2, to_acc_nr = 1, 
                       amount = 300,
                       from_account_resulting_balance = 300,
                       to_account_resulting_balance = 800} = Transaction2
    end,
    Event = #transaction_event{transaction_id = 1,
        timestamp = erlang:timestamp(),
        from_acc_nr = 1,
        to_acc_nr = 2,
        amount = 55,
        from_account_resulting_balance = 75,
        to_account_resulting_balance = 65},        
    gen_server:cast(transaction_service, Event),
    receive
      {_, REvent} -> Event = REvent
    end,
    erlbank_transactions_server:stop()
  end.
    










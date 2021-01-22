-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(account,
    {account_number :: account_number(),
     amount :: money()}).
-record(transaction, 
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_acc_nr :: account_number(), 
     to_acc_nr :: account_number(), 
     amount :: money(),
     from_account_resulting_balance :: money(),
     to_account_resulting_balance :: money()
    }).

-record(transaction_service_state, {
  subscriber_pids :: list(pid())
}).

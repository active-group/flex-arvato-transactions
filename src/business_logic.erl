%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-include("transactions_events.hrl").
-export([ transfer/3, sort_tx/1, get_transactions/1, get_transactions_from/1]).


%% Opens an account, that is creates a new account containing a new person 
%% Writes them into database.


-spec get_transactions(unique_id()) -> list(#transaction{}).
get_transactions(Id) ->
     database:get_all_transactions(Id).

-spec get_transactions_from(unique_id()) -> list(#transaction{}).
get_transactions_from(FromId) ->
     database:get_transactions_from(FromId).

%% Takes a sender & receiver account number and an amount and transfers 
%% that amount from sender to receiver.
%% Crashes if accounts do not exist.
%% Returns {ok, tid}, where tid is the id of the stored transaction
%% or {error, insufficient_funds} when the sender does not have enough money
%% in his account.

-spec transfer(account_number(), account_number(), money()) -> 
     {error, sender_account_not_found | receiver_account_not_found | insufficient_funds}
   | {ok, unique_id()}.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->

    Transaction = fun() -> MaybeAccSender = database:get_account(SenderAccountNumber), %
                           MaybeAccReceiver = database:get_account(ReceiverAccountNumber),
                           case {MaybeAccSender, MaybeAccReceiver} of
                               {{error, not_found}, _} -> {error, sender_account_not_found};
                               {_, {error, not_found}} -> {error, receiver_account_not_found};

                               {{ok, AccSender}, {ok, AccReceiver}} ->
                                   AccSenderAmount = AccSender#account.amount,
                                   AccReceiverAmount = AccReceiver#account.amount,

                                   if
                                       AccSenderAmount - Amount >= 0 ->
                                           NewAccSender = AccSender#account{amount = (AccSenderAmount - Amount)},
                                           NewAccReceiver = AccReceiver#account{amount = (AccReceiverAmount + Amount)},
                                           TxId = database:unique_tx_id(),
                                           Tx = #transaction{id = TxId,
                                                             timestamp = erlang:timestamp(),
                                                             from_acc_nr = SenderAccountNumber,
                                                             to_acc_nr = ReceiverAccountNumber,
                                                             amount = Amount,
                                                             from_account_resulting_balance = NewAccSender#account.amount,
                                                             to_account_resulting_balance = NewAccReceiver#account.amount                                                         },
                                           database:put_transaction(Tx),
                                           database:put_account(NewAccSender),
                                           database:put_account(NewAccReceiver),
                                           {ok, Tx};
                                       true ->
                                           {error, insufficient_funds}
                                   end
                           end
                  end,

    {_, Tx} = database:atomically(Transaction),
    % Schicke "transaction created" an transaction server
    TransactionEvent = #transaction_event{
      transaction_id = Tx#transaction.id,
      amount = Tx#transaction.amount,
      from_acc_nr = Tx#transaction.from_acc_nr,
      to_acc_nr = Tx#transaction.to_acc_nr,
      timestamp = Tx#transaction.timestamp,
      from_account_resulting_balance = Tx#transaction.from_account_resulting_balance,
      to_account_resulting_balance = Tx#transaction.to_account_resulting_balance
    },
    gen_server:cast(transaction_service, TransactionEvent).

%% Takes a list of transactions and returns them sorted by their id (asc)

sort_tx(Txs) ->
    lists:sort(fun(Tx1, Tx2) -> Tx2#transaction.id < Tx1#transaction.id end, Txs).

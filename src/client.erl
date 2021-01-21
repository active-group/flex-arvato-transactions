%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([ transfer/3]).




%% transfers a given amount from the first account to the second account, identified
%% by their account number. Prints the transaction-id when successful, else the error
%% to stdout.
-spec transfer(account_number(), account_number(), money()) -> ok.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->
    case business_logic:transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) of
        {ok, TxId} ->
            io:format("Transaction successful, id: ~p~n", [TxId]);
        {error, Err} ->
            io:format("An error occured: ~p~n", [Err])
        end.



%% takes an transaction record and prints it to stdout.
print_tx(Tx) ->

    Amount = Tx#transaction.amount,
    Id = Tx#transaction.id,
    From = Tx#transaction.from_acc_nr,
    To = Tx#transaction.to_acc_nr,
    io:format("#~p\t ~p\t ~s \t -> ~s ~n", [Id, Amount, From, To]).

%% takes a list of transactions records and prints them to stdout
print_txs(Txs) ->
    lists:map(fun print_tx/1, Txs).




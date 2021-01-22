% Service name: transaction_service

% Duplicates from data.hrl:
% -type unique_id() :: pos_integer().
% -type account_number() :: integer().
% -type money() :: number().

-record(transaction_event, {
  transaction_id :: unique_id(),  % fortlaufend
  timestamp :: erlang:timestamp(),  % der transaction
  from_acc_nr :: account_number(),
  to_acc_nr :: account_number(),
  amount :: money(),
  from_account_resulting_balance :: money(),
  to_account_resulting_balance :: money()
}).

% subscription event
-record(transaction_event_subscription, {
  from_transaction_id :: unique_id(), % Wenn 0 oder leer => alle
  subscriber_pid :: pid()
}).

%%% evtl. flexibler
%%-record(account_transaction_event,
%%{
%%  id :: unique_id(),
%%  timestamp :: erlang:timestamp(),
%%  acc_nr :: account_number(),
%%  amount :: money(),  % can be positive or negative
%%  account_resulting_balance :: money()
%%  }).

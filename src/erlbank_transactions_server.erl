-module(erlbank_transactions_server).
-include("data.hrl").
-include("transactions_events.hrl").

-behaviour(gen_server).
% gen_server ist Teil des OTP-Frameworks, das bei Erlang dabei ist
% Open Telephone Platform
% Hier eine Implementierung des Interface gen_server
% Schnittstelle für Callbacks
-export([
  % gen_server
  init/1, handle_cast/2, handle_call/3, start/0 
]).

start() ->
  {ok, Pid} = gen_server:start(?MODULE, unused, []),
                           %  ^^^^^^^^^^^^ wird zum  Argument von init
  % macht einen neuen Prozess, ruft dort init auf, startet Schleife, die Nachrichten empfängt
  register(transaction_service, Pid),
  global:register_name(transaction_service, Pid).

% InitialN kommt von gen_server:start
% NOTE Maik says don't bother saving service state  
init(_) -> {ok, #transaction_service_state{subscriber_pids = []}}. % gibt initialen Zustand zurück

%%account_created_event() -> 

%%subscribe_transaction_event() -> 

calc_inc(Pid, Increment) -> gen_server:cast(Pid, #inc{increment = Increment}).
calc_get(Pid) -> gen_server:call(Pid, #get{}).

-type message() :: #reset{} | #inc{} 
                 | #mult{} | #divide{} | #get{}.

% TODO -spec handle_message(number(), message()) -> number().
handle_subscription_event(#transaction_service_state{subscriber_pids = SubscriberPids}, #transaction_event_subscription{from_transaction_id = FromTransactionId, subscriber_pid = SubscriberPid}) ->
  NewState = #transaction_service_state{subscriber_pids = [SubscriberPid|SubscriberPids]},
  Transactions = business_logic:get_transactions_from(FromTransactionId),
  send_transactions(SubscriberPid, Transactions),
  NewState.
% TODO transaction_event_subscription ohne from_transaction_id parameter

send_transactions(SubscriberPid, []) -> ok;
send_transactions(SubscriberPid, [Tx | RestTransactions]) ->
  TransactionEvent = #transaction_event{
    transaction_id = Tx#transaction.id,
    amount = Tx#transaction.amount,
    from_acc_nr = Tx#transaction.from_acc_nr,
    to_acc_nr = Tx#transaction.to_acc_nr,
    timestamp = Tx#transaction.timestamp,
    from_account_resulting_balance = Tx#transaction.from_account_resulting_balance,
    to_account_resulting_balance = Tx#transaction.to_account_resulting_balance
  },
  gen_server:cast(transaction_service, TransactionEvent),
  send_transactions(SubscriberPid, RestTransactions).


handle_transaction_event(State, #transaction_event{} = TransactionEvent) ->
  % An alle subscriber das transaction_event schicken
  send_event(State#transaction_service_state.subscriber_pids, TransactionEvent),
  State
.

send_event([], _TransactionEvent) -> ok;
send_event([FirstPid | RestPids], TransactionEvent) ->
    gen_server:cast(FirstPid, TransactionEvent),
    send_event(RestPids, TransactionEvent).

% muß auch noch Nachricht zurückschicken:
% #get{} ist anders als die anderen
% wird nicht benötigt, da es immer bei handle_call aufschlägt:
% update_calc_state(N, #get{}) -> N.

% Module:handle_cast(Request, State) -> Result
% Types
% Request = term()
% State = term()
% Result = {noreply,NewState} | 

% Mögliche Fälle
% - #transaction_event_subscription
% - #transaction_event
handle_cast(#transaction_event_subscription{} = Message, State) -> {noreply, handle_subscription_event(State, Message)}.
handle_cast(#transaction_event{} = Message, State) -> {noreply, handle_transaction_event(State, Message)}.

% Ein Request, der keine Antwort erfordert: cast (asynchron)
% Ein Request, der eine Antwort erfordert: call (synchron)

% Module:handle_call(Request, From, State) -> Result
	
% Types
% Request = term()
% From = {pid(),Tag}
% State = term()
% Result = {reply,Reply,NewState}

handle_call(#get{}, _From, N) -> 
    Reply = N,
    NewState = N,
    {reply, Reply, NewState}.

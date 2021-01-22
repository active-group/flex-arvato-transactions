-module(erlbank_transactions_server).
-include("data.hrl").
-include("transactions_events.hrl").

-behaviour(gen_server).
% gen_server ist Teil des OTP-Frameworks, das bei Erlang dabei ist
% Open Telephone Platform
% Hier eine Implementierung des Interface gen_server
% Schnittstelle für Callbacks
-export([init/1, handle_cast/2, handle_call/3,
         start/0, calc_reset/1, calc_inc/2, calc_mult/2, calc_div/2, calc_get/1]).

start() ->
  {ok, Pid} = gen_server:start(?MODULE, 0, []),
                           %  ^^^^^^^^^^^^ wird zum  Argument von init
  % macht einen neuen Prozess, ruft dort init auf, startet Schleife, die Nachrichten empfängt
  register(transaction_service, Pid),
  global:register_name(transaction_service, Pid).

%%account_created_event() -> 

%%subscribe_transaction_event() -> 

calc_inc(Pid, Increment) -> gen_server:cast(Pid, #inc{increment = Increment}).
calc_get(Pid) -> gen_server:call(Pid, #get{}).

-type message() :: #reset{} | #inc{} 
                 | #mult{} | #divide{} | #get{}.

-spec handle_message(number(), message()) -> number().
handle_message(State, #transaction_event_subscription{from_transaction_id = FromTransactionId, subscriber_pid = SubscriberPid}) ->
  % pid in State übernehmen
  % senden der transaction_events anstoßen
  FromTransactionId
;
% TODO transaction_event_subscription ohne from_transaction_id oarameter
handle_message(State, #transaction_event{} = TransactionEvent) ->
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

% InitialN kommt von gen_server:start
init(InitialN) -> {ok, InitialN}. % gibt initialen Zustand zurück

% Module:handle_cast(Request, State) -> Result
% Types
% Request = term()
% State = term()
% Result = {noreply,NewState} | 

handle_cast(Message, State) -> {noreply, handle_message(State, Message)}.

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

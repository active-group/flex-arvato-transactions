%%%-------------------------------------------------------------------
%% @doc erlbank_transactions public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_transactions_app).

-behaviour(application).

-export([start/2, stop/1]).




start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/transactions/create", web_frontend, create_transaction}

                              ]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8000}],
                                 #{env => #{dispatch => Dispatch}}).


keep_pinging(Node) ->
    case net_adm:ping(Node) of
        pong -> lager:info("successfully connected with node ~p", [Node]),
                ok;
        _ -> lager:info("re-pinging node ~p", [Node]),
             timer:sleep(1000),
             keep_pinging(Node)
    end.

start(_StartType, _StartArgs) ->
    database:init_database(),
    start_cowboy(),

    AccountNode = list_to_atom(os:getenv("ACCOUNT_NODE")),
    keep_pinging(AccountNode),

    lager:info("Consuming accounts on: ~p~n", [AccountNode]),

    account_poller:poll_process(),
    erlbank_transactions_server:start(),
    erlbank_transactions_sup:start_link().

stop(_State) ->
    ok.


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


start(_StartType, _StartArgs) ->
    database:init_database(),
    start_cowboy(),
    account_poller:poll_process(),
    erlbank_transactions_sup:start_link().

stop(_State) ->
    ok.


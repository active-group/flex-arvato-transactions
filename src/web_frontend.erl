
-module(web_frontend).
-include("data.hrl").
-export([init/2]).




-spec bin_to_int(binary()) -> integer().
bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).

-spec transaction_error() -> binary().
transaction_error() ->
    << "
      <p> An error occured: ~p </p> ~n
       <a href=\"/\"> Back </a>
    " >>.

-spec transaction_success() -> binary().
transaction_success() ->
            << "
      <p> Transaction with id ~p successfully created </p> ~n
               <a href=\"/\"> Back </a>
    " >>.


transaction_form() ->
                    << "
<h3> Create transaction </h3>
                       <form method=\"post\" action=\"/transactions/create\">
  <label for=\"transactions_from\"> From (account number) </label>
  <input type=\"text\" id=\"transactions_from\" name=\"transactions_from\" />

  <label for=\"transactions_to\"> To (account number) </label>
  <input type=\"text\" id=\"transactions_to\" name=\"transactions_to\" />

  <label for=\"transactions_amount\"> Amount </label>
  <input type=\"text\" id=\"transactions_amount\" name=\"transactions_amount\" />

  <input type=\"submit\" value=\"Create transaction\" />
</form>" >>.


index() ->
    io_lib:format("~s",
                  [
                   transaction_form()
                   ]).


%% /transactions/create
init(Req, create_transaction) ->

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    SenderAccountNumber =  bin_to_int(maps:get(<<"transactions_from">>, KeyValues)),
    ReceiverAccountNumber = bin_to_int(maps:get(<<"transactions_to">>, KeyValues)),
    Amount = bin_to_int(maps:get(<<"transactions_amount">>, KeyValues)),

    Body = case business_logic:transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) of
               {ok, TxId} ->
                   io_lib:format(transaction_success(), [TxId])
                  % Schicke transaction_created" an Server  
             ;
               {error, Err} ->
                   io_lib:format(transaction_error(), [Err])
           end,
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
    {ok, Req2, []};

%% /index
init(Req0, index) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           index(),
                           Req0),
    {ok, Req, []}.

-module(google_token).
-behaviour(gen_aaa).

%% TODO make this a full-fledged application with a gen server and some caching of already seen tokens.
-export([authenticate/1, principal/1, authorize/1]).

authenticate(Req) ->
    authenticate(Req, extract_maybe_token(Req)).

authenticate(_Req, []) ->
    {error, 401, [{<<"WWW-Authenticate">>, <<"Bearer realm=\"netronner\"">>}]};
authenticate(_Req, [Token]) ->
    {ok, <<ClientId/binary>>} = application:get_env(aaa, google_api_client_id),
    case google:validate_access_token(Token, ClientId) of
        ok -> ok;
        error -> {error, 401, [{<<"WWW-Authenticate">>, <<"Bearer error=\"invalid_token\"">>}]}
    end.

principal(Req) ->
    [Token] = extract_maybe_token(Req),
    GoogleUser = google:user_profile(Token, <<"me">>),
    user_to_principal(GoogleUser).

authorize(Principal) ->
    case google_viral_authorization:is_infected(principal:id(Principal)) of
        true -> ok;
        false -> {error, 401, [{<<"WWW-Authenticate">>, <<"Bearer error=\"invalid_token\" error_description=\"unauthorized\"">>}]}
    end.

extract_maybe_token(Req) -> 
    case cowboy_req:header(<<"authorization">>, Req) of
        { undefined, _} -> [];
        { <<"Bearer ", AccessToken/binary>>, _} -> [google:access_token_make(AccessToken)]
    end.

user_to_principal(GoogleUser) ->
    Id = google:user_id(GoogleUser),
    Name = google:user_name(GoogleUser),
    principal:make(Id, Name).

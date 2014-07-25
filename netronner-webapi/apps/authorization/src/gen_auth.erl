-module(gen_auth).

-export([is_authorized/2, principal/2, make_principal/3]).

-type principal() :: #{ id => binary(), displayName => binary(), image_url => binary()}.
-export_type([principal/0]).

-callback is_authorized(Req::cowboy_req:req()) -> ok | { error, StatusCode::number(), Headers::cowboy:http_headers()}.
-callback principal(Req::cowboy_req:req()) -> principal().

is_authorized(Module, Req) ->
    Module:is_authorized(Req).

principal(Module, Req) ->
    Module:principal(Req).

make_principal(Id, DisplayName, ImageUrl) ->
    #{ 
        id => Id,
        displayName => DisplayName,
        image_url => ImageUrl
    }.
-module(rinamo_auth_basic).

-export([authorize/4]).

% Every access key is a unique data owner
%
% In addition to be totally insecure, this
% is really stupid for data sharing, so
% please don't use this in production.
authorize(AccessKey, _, _, _) ->
    AccessKey.

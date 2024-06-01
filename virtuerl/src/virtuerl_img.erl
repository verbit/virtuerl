%%%-------------------------------------------------------------------
%%% @author ilya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virtuerl_img).

-export([list_images/0]).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([ensure_image/1]).

-define(SERVER, ?MODULE).
-define(APPLICATION, virtuerl).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

list_images() ->
  gen_server:call(?SERVER, list_images).

ensure_image(ImageName) ->
  gen_server:call(?SERVER, {ensure_image, ImageName}, infinity).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call(list_images, _Sender, State) ->
  {ok, Filenames} = file:list_dir(virtuerl_mgt:home_path()),
  Images = [Filename || Filename <- Filenames, virtuerl_util:ends_with(Filename, ".qcow2")],
  Result = lists:uniq(["debian-12-genericcloud-amd64-20240507-1740.qcow2" | Images]),
  {reply, Result, State};

handle_call({ensure_image, ImageName}, _Sender, State) ->
  io:format("DOWNLOADING..."),
  Path = filename:join(virtuerl_mgt:home_path(), ImageName),
  Res = case filelib:is_regular(Path) of
    true -> {ok, Path};
    false ->
      {ok, Pat} = re:compile("^debian-12-genericcloud-amd64-(\\d+-\\d+).qcow2$"),
      case re:run(ImageName, Pat, [{capture, all_but_first, list}]) of
        {match, [Build]} ->
          CacheImagePath = filename:join(["/tmp/virtuerl/cache", ImageName]),
          case filelib:is_regular(CacheImagePath) of
            true ->
              file:copy(CacheImagePath, Path),
              {ok, Path};
            false ->
              TempImagePath = filename:join(["/tmp/virtuerl/", ImageName]),
              ok = filelib:ensure_dir(CacheImagePath),
              {ok, _} = httpc:request(get, {["https://cloud.debian.org/images/cloud/bookworm/", Build, "/", ImageName], []}, [],
                [{stream, TempImagePath}]),
              file:rename(TempImagePath, CacheImagePath),
              file:copy(CacheImagePath, Path),
              {ok, Path}
          end;
        nomatch ->
          {error, not_supported}
      end
  end,
  {reply, Res, State}.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(_Req, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

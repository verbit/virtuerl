-module(virtuerl_util).

-export([uuid4/0, mac_to_str/1, delete_file/1, cmd/1, cmd/2, ends_with/2]).


uuid4() ->
    ID = string:lowercase(binary:encode_hex(<<(rand:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF) - 1):128>>)),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = ID,
    Uuid4 = iolist_to_binary([A, "-", B, "-", C, "-", D, "-", E]),
    Uuid4.


mac_to_str(<<Mac:48>>) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16>> = string:lowercase(binary:encode_hex(<<Mac:48>>)),
    <<A:16, $::8, B:16, $::8, C:16, $::8, D:16, $::8, E:16, $::8, F:16>>.


delete_file(Filename) ->
    case file:delete(Filename) of
        ok -> ok;
        {error, enoent} -> ok;
        Other -> Other
    end.


ends_with(Str, Suffix) ->
    StrLen = string:length(Str),
    SuffixLen = string:length(Suffix),
    case StrLen < SuffixLen of
        true -> false;
        false ->
            Suf = string:slice(Str, StrLen - SuffixLen),
            string:equal(Suf, Suffix)
    end.


cmd(Cmd) -> cmd(Cmd, []).


cmd(Cmd, Stdin) ->
    {ok, _Pid, OsPid} = exec:run(Cmd, [stdin, stderr, stdout, monitor]),
    exec:send(OsPid, iolist_to_binary(Stdin)),
    exec:send(OsPid, eof),
    {Res, Reason} = read_cmd_out(OsPid, #{}),
    case Reason of
        normal -> {ok, Res};
        {status, Status} -> {error, exec:status(Status), Res};
        Else -> {error, unknown, Else, Res}
    end.


read_cmd_out(OsPid, Acc) ->
    receive
        {FD, OsPid, Data} when FD == stdout; FD == stderr ->
            PreEx = maps:get(FD, Acc, ""),
            read_cmd_out(OsPid, Acc#{FD => [PreEx, Data]});
        {'DOWN', OsPid, process, _Pid, Reason} -> {Acc, Reason}
    end.

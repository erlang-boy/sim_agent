%%%---------------------------------------------------
%%% @Module   : company
%%% @Author   : wubx
%%% @Email    : thinear@gmail.com
%%% @Created  : 14-11-1
%%% @Description :
%%%---------------------------------------------------
-module(dummy_data).

-export([company/1]).
-export([company_size/0]).

-export([server_info/0]).

-export([msg/1]).
-export([msg_size/0]).

-export([random_company/0]).

-include("msg_type.hrl").
-include("types.hrl").

company(1) ->
    #company{
        id = <<"059f40aa9076e694f1a0">>,
        name = <<"company1@email.com">>,
        pubkey = <<"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAsK+KeyxI0tHGDpBkBhCZ\nEbXZZXookteA7Mt+I7qmrl7iaaWC44NQZtS94LVgc1Pj0zw6JkaVh3rK9Tpq2Nrt\nGTqeG7m39Jvbq1YhSghnYoRYbM5rww+i6O5GwCB1zJAbA5of2L5NXJF3BxhYOOz7\n3Hor+AYipZkzsb1d2zRKmfv+ovJK8LrMcOPXbuWZ3kf5hDAeNDXkkMhi2cCocLdS\nwgzkMd2J4KRiMxznQiNxgiGDDizJAR3QMHixjGjVX8Ywqu3w/3uXUPh7KYe1rZ/Y\nLeLlI6Sja1anR8HF0oOxJ60+aqU3g2Pnhrs4hgiMSCLi631bLAybsE9YgesK9dOH\nRwIDAQAB\n-----END PUBLIC KEY-----\n">>
    }.

company_size() -> 1.

server_info() ->
    #{
        <<"agent_ver">> => <<"1.0.0">>,
        <<"cpu_info">> => #{
            <<"cpu_bandstr">> => <<"Intel(R) Core(TM) i5-4460  CPU @ 3.20GHz">>,
            <<"cpu_number">> => 4,
            <<"cpu_producer">> => <<"GenuineIntel">>,
            <<"cpu_type">> => <<"amd64">>,
            <<"physical_id">> => <<"0x6">>
        },
        <<"hardid">> => <<"12345678901234567890123456789012">>,
        <<"ip_info">> => [
            #{<<"ip">> => <<"192.168.199.224">>,<<"mac">> => <<"08570032d7ea">>},
            #{<<"ip">> => <<"::">>,<<"mac">> => <<"74d435edb0fa">>},
            #{<<"ip">> => <<"">>,<<"mac">> => <<"005056c00001">>},
            #{<<"ip">> => <<"CDCD:910A:2222:5498:8475:1111:3900:2020">>,<<"mac">> => <<"005056c00008">>}
        ],
        <<"last_boot_time">> => <<"2014-08-14 19:29:43">>,
        <<"os_info">> => #{
            <<"name">> => <<"wbx's pc">>,
            <<"product_type">> => <<"server">>,
            <<"description">> => <<"Microsoft Windows 7 Ultimate Edition Service Pack 1 (build 7601), 64-bit">>,
            <<"is_64bit">> => true,
            <<"language">> => <<"chs">>,
            <<"sp_ver">> => <<"1.0">>,
            <<"ver">> => <<"6.1.7601">>
        },
        <<"os_type">> => <<"linux">>
    }.

msg(_) -> error.

msg_size() -> 0.

random_company() ->
    Idx = sim_agent_misc_lib:random(1, company_size()),
    company(Idx).
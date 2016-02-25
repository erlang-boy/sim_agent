%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_crypto
%%% =================================================================

-module(sim_agent_crypto).
-author("Liu HuiDong").

%%% =================================================================
%%% API functions
%%% =================================================================
-export([init_crypto/2]).
-export([encrypt/1]).
-export([decrypt/1]).
-export([clear/0]).

-export([rsa_decode_key/1,
    rsa_decrypt/2,
    rsa_encrypt/2]).

-record(dict_value, {
    name,
    state
}).

%%% -----------------------------------------------------------------
%%% init_crypto/2
%%% -----------------------------------------------------------------
init_crypto(rc4, Config) ->
    Key = case proplists:get_value(key, Config, undefined) of
              undefined ->
                  Size = proplists:get_value(key_size, Config, 67),
                  crypto:strong_rand_bytes(Size);
              Val -> Val
          end,
    Context = crypto:stream_init(rc4, Key),
    put(crypto, #dict_value{name = rc4, state = {Context, Context}}),
    {ok, Key};
init_crypto(Type, _Config) ->
    error_logger:error_report(
        "~n crypto type not supported: ~p ~n",
        [Type]),
    exit(normal).

%%% -----------------------------------------------------------------
%%% encrypt/1
%%% -----------------------------------------------------------------
encrypt(Text) ->
    encrypt(get(crypto), Text).

%%% -----------------------------------------------------------------
%%% encrypt/2
%%% -----------------------------------------------------------------
encrypt(Crypto = #dict_value{name = rc4, state = {Context, C}}, Text) ->
    {Context2, Cipher} = crypto:stream_encrypt(Context, Text),
    put(crypto, Crypto#dict_value{state = {Context2, C}}),
    Cipher.

%%% -----------------------------------------------------------------
%%% decrypt/1
%%% -----------------------------------------------------------------
decrypt(Cipher) ->
    decrypt(get(crypto), Cipher).

%%% -----------------------------------------------------------------
%%% decrypt/2
%%% -----------------------------------------------------------------
decrypt(Crypto = #dict_value{name = rc4, state = {C, Context}}, Cipher) ->
    {Context2, Text} = crypto:stream_decrypt(Context, Cipher),
    put(crypto, Crypto#dict_value{state = {C, Context2}}),
    Text.

%%% -----------------------------------------------------------------
%%% clear/0
%%% -----------------------------------------------------------------
clear() ->
    erase(crypto).

%%% -----------------------------------------------------------------
%%% rsa_dec1de_key/0
%%% -----------------------------------------------------------------
rsa_decode_key(KeyBin) ->
    [Info] = public_key:pem_decode(KeyBin),
    public_key:pem_entry_decode(Info).

%%% -----------------------------------------------------------------
%%% rsa_decrypt/2
%%% -----------------------------------------------------------------
rsa_decrypt(Cipher, PubKey) ->
    public_key:decrypt_public(Cipher, PubKey, [{rsa_pad, rsa_pkcs1_padding}]).

%%% -----------------------------------------------------------------
%%% rsa_encrypt/2
%%% -----------------------------------------------------------------
rsa_encrypt(Text, PubKey) ->
    public_key:encrypt_public(Text, PubKey, [{rsa_pad, rsa_pkcs1_padding}]).
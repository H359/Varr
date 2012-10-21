-module(socketio_impl).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->  
    [
     {on_init, 1},  
     {on_connect, 2},
     {on_message, 2},
     {on_disconnect, 2},
     {on_destroy, 1}
    ];

behaviour_info(_Other) ->  
    undefined.